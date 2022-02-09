use std::{collections::HashMap, convert::TryInto};

use super::specific::*;
use crate::{closure, id::generate_id, knormal::BinaryOp, span::Spanned, ty::Type};

// int 型が 2 の何乗 bytes か
static INT_BYTES_EXP: usize = 2;
// float 型が 2 の何乗 bytes か
static FLOAT_BYTES_EXP: usize = 2;

fn flatten_let(exp_id: Expr, id_outer: String, exp_suc: Expr) -> Box<Expr> {
    let converted_item = match exp_id.item {
        RawExpr::LetIn {
            id: id_inner,
            instr_id,
            instr_suc,
        } => RawExpr::LetIn {
            id: id_inner,
            instr_id,
            instr_suc: flatten_let(*instr_suc, id_outer, exp_suc),
        },
        RawExpr::Is(instr) => RawExpr::LetIn {
            id: id_outer,
            instr_id: instr,
            instr_suc: Box::new(exp_suc),
        },
    };
    Box::new(Spanned::new(converted_item, exp_id.span))
}

fn convert_expr(expr: closure::Expr, env: &mut HashMap<String, Type>) -> Expr {
    let expr_span = expr.span;
    let wrap = |instr| Box::new(Spanned::new(instr, expr_span));
    let wrap_expr = |expr| Box::new(Spanned::new(expr, expr_span));
    let wrap_instr = |instr| {
        Box::new(Spanned::new(
            RawExpr::Is(Spanned::new(instr, expr_span)),
            expr_span,
        ))
    };
    *match expr.item {
        closure::RawExpr::LetIn {
            id,
            exp_id,
            exp_suc,
        } => {
            let exp_id = convert_expr(*exp_id, env);
            let exp_suc = convert_expr(*exp_suc, env);
            flatten_let(exp_id, id, exp_suc)
        }
        closure::RawExpr::If {
            id_left,
            op,
            id_right,
            exp_then,
            exp_else,
        } => {
            let exp_then = Box::new(convert_expr(*exp_then, env));
            let exp_else = Box::new(convert_expr(*exp_else, env));
            let id_type = env.get(&id_left).unwrap();
            match id_type {
                Type::Int => {
                    // int の比較の場合は、beq/ble を使う
                    wrap_instr(RawInstr::If {
                        id_left,
                        op,
                        id_right,
                        exp_then,
                        exp_else,
                    })
                }
                Type::Float => {
                    // float の比較の場合は、feq/fle を変数に代入して beq を使う
                    let compare_id = generate_id("cm");
                    env.insert(compare_id.clone(), Type::Int);
                    wrap_expr(RawExpr::LetIn {
                        id: compare_id.clone(),
                        instr_id: *wrap(RawInstr::FCondOp {
                            id_left,
                            op,
                            id_right,
                        }),
                        instr_suc: wrap_instr(RawInstr::IfZero {
                            id: compare_id,
                            exp_then: exp_else,
                            exp_else: exp_then,
                        }),
                    })
                }
                t => {
                    log::error!("cannot compare values of type {t}");
                    panic!("internal compiler error");
                }
            }
        }
        closure::RawExpr::Unit => wrap_instr(RawInstr::Unit),
        closure::RawExpr::Int(integer) => wrap_instr(RawInstr::Int(integer)),
        closure::RawExpr::Float(float) => wrap_instr(RawInstr::Float(float)),
        closure::RawExpr::Var(id) => {
            let instr = match env.get(&id).unwrap() {
                Type::Unit => RawInstr::Unit,
                _ => RawInstr::Var(id),
            };
            wrap_instr(instr)
        }
        closure::RawExpr::UnOp { op, id } => wrap_instr(RawInstr::UnOp { op, id }),
        closure::RawExpr::BiOp {
            id_left,
            op,
            id_right,
        } => wrap_instr(RawInstr::BiOp {
            id_left,
            op,
            id_right,
        }),
        closure::RawExpr::ApplyCls { cls, args } => wrap_instr(RawInstr::CallCls { cls, args }),
        closure::RawExpr::ApplyDir { tag, args } => wrap_instr(RawInstr::CallDir { tag, args }),
        closure::RawExpr::NewTuple(elms) => wrap_instr(RawInstr::NewTuple(elms)),
        closure::RawExpr::NewClosure { tag, free_vars } => {
            wrap_instr(RawInstr::NewClosure { tag, free_vars })
        }
        closure::RawExpr::TupleGet { tuple, index } => {
            // タプルの型を得る
            let elm_types = match env.get(&tuple).unwrap() {
                Type::Tuple(elm_types) => elm_types.clone(),
                _ => panic!("internal compiler error"),
            };
            // 先頭から index 要素目のタプルが何 byte 目に位置するか計算
            let offset = elm_types[0..index]
                .iter()
                .map(|elm_type| match elm_type {
                    Type::Unit => 0usize,
                    Type::Float => FLOAT_BYTES_EXP,
                    _ => INT_BYTES_EXP,
                })
                .sum::<usize>()
                .try_into()
                .unwrap();
            wrap_instr(RawInstr::Read {
                address: tuple,
                offset,
            })
        }
        closure::RawExpr::ArrayGet { array, index } => {
            let elm_type = match env.get(&array).unwrap() {
                Type::Array(elm_type) => *elm_type.clone(),
                _ => panic!("internal compiler error"),
            };
            if elm_type == Type::Unit {
                wrap_instr(RawInstr::Unit)
            } else {
                let shift_amount: i32 = (if elm_type == Type::Float {
                    FLOAT_BYTES_EXP
                } else {
                    INT_BYTES_EXP
                })
                .try_into()
                .unwrap();
                let shift_id = generate_id("sh"); // シフトする幅を格納する変数
                let offset_id = generate_id("of"); // アドレスに足す offset を格納する変数
                let address_id = generate_id("pt"); // アドレス + offset の結果を格納する変数
                env.insert(shift_id.clone(), Type::Int);
                env.insert(offset_id.clone(), Type::Int);
                env.insert(address_id.clone(), Type::Array(Box::new(elm_type)));
                // 以下の式を構成する
                // shift : int = <シフト量>;
                // offset : int = (index << shift);
                // address : t array = array + offset;
                // READ 0(address)
                let whole_expr = RawExpr::LetIn {
                    id: shift_id.clone(),
                    instr_id: *wrap(RawInstr::Int(shift_amount)),
                    instr_suc: wrap_expr(RawExpr::LetIn {
                        id: offset_id.clone(),
                        instr_id: *wrap(RawInstr::BiOp {
                            id_left: index,
                            op: BinaryOp::LShift,
                            id_right: shift_id,
                        }),
                        instr_suc: wrap_expr(RawExpr::LetIn {
                            id: address_id.clone(),
                            instr_id: *wrap(RawInstr::BiOp {
                                id_left: array,
                                op: BinaryOp::Add,
                                id_right: offset_id,
                            }),
                            instr_suc: wrap_instr(RawInstr::Read {
                                address: address_id,
                                offset: 0,
                            }),
                        }),
                    }),
                };
                wrap_expr(whole_expr)
            }
        }
        closure::RawExpr::ArrayPut {
            array,
            index,
            value,
        } => {
            let elm_type = match env.get(&array).unwrap() {
                Type::Array(elm_type) => *elm_type.clone(),
                _ => panic!("internal compiler error"),
            };
            if elm_type == Type::Unit {
                wrap_instr(RawInstr::Unit)
            } else {
                let shift_amount: i32 = (if elm_type == Type::Float {
                    FLOAT_BYTES_EXP
                } else {
                    INT_BYTES_EXP
                })
                .try_into()
                .unwrap();
                let shift_id = generate_id("sh"); // シフトする幅を格納する変数
                let offset_id = generate_id("of"); // アドレスに足す offset を格納する変数
                let address_id = generate_id("pt"); // アドレス + offset の結果を格納する変数
                env.insert(shift_id.clone(), Type::Int);
                env.insert(offset_id.clone(), Type::Int);
                env.insert(address_id.clone(), Type::Array(Box::new(elm_type)));
                // 以下の式を構成する
                // shift : int = <シフト量>;
                // offset : int = (index << shift);
                // address : t array = array + offset;
                // WRITE 0(address), value
                let whole_expr = RawExpr::LetIn {
                    id: shift_id.clone(),
                    instr_id: *wrap(RawInstr::Int(shift_amount)),
                    instr_suc: wrap_expr(RawExpr::LetIn {
                        id: offset_id.clone(),
                        instr_id: *wrap(RawInstr::BiOp {
                            id_left: index,
                            op: BinaryOp::LShift,
                            id_right: shift_id,
                        }),
                        instr_suc: wrap_expr(RawExpr::LetIn {
                            id: address_id.clone(),
                            instr_id: *wrap(RawInstr::BiOp {
                                id_left: array,
                                op: BinaryOp::Add,
                                id_right: offset_id,
                            }),
                            instr_suc: wrap_instr(RawInstr::Write {
                                address: address_id,
                                offset: 0,
                                value,
                            }),
                        }),
                    }),
                };
                wrap_expr(whole_expr)
            }
        }
        closure::RawExpr::ExtArray { array } => wrap_instr(RawInstr::DataTag(array)),
    }
}

pub fn specify(
    expr: closure::Expr,
    toplevels: Vec<closure::Function>,
    env: &mut HashMap<String, Type>,
) -> Vec<Function> {
    let mut functions: Vec<Function> = vec![];
    toplevels.into_iter().for_each(|toplevel| {
        functions.push(Function {
            tag: toplevel.tag,
            args: toplevel.args,
            free_vars: toplevel.free_vars,
            body: convert_expr(toplevel.body, env),
        });
    });
    functions.push(Function {
        tag: "min_caml_start".to_string(),
        args: vec![],
        free_vars: vec![],
        body: convert_expr(expr, env),
    });
    functions
}
