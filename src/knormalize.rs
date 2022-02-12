use std::{cell::RefCell, rc::Rc};

use fnv::FnvHashMap;

use crate::id::generate_id;
use crate::knormal::{BinaryOp, CondOp, Expr, RawExpr, UnaryOp};
use crate::span::*;
use crate::syntax;
use crate::ty::{Type, VarType};

fn insert_let<F: FnOnce(String) -> RawExpr>(
    (expr, t): (Box<Expr>, Type),
    env: Rc<RefCell<&mut FnvHashMap<String, Type>>>,
    f_exp_t_cont: F,
) -> Box<Expr> {
    let expr_span = expr.span.clone();
    let raw_expr = match expr.item {
        RawExpr::Var(id) => f_exp_t_cont(id),
        _ => {
            let new_id = generate_id("kn");
            let exp_cont = f_exp_t_cont(new_id.clone());
            env.borrow_mut().insert(new_id.clone(), t);
            RawExpr::LetIn {
                id: new_id,
                exp_id: expr,
                exp_suc: Box::new(Spanned::new(exp_cont, expr_span)),
            }
        }
    };
    Box::new(Spanned::new(raw_expr, expr_span))
}

// t に含まれる Bool 型を Int 型に変える
fn sanitize_bool(t: Type) -> Type {
    match t {
        Type::Unit => Type::Unit,
        Type::Bool | Type::Int => Type::Int,
        Type::Float => Type::Float,
        Type::Fun(args, ret) => Type::Fun(
            args.into_iter().map(|arg| sanitize_bool(arg)).collect(),
            Box::new(sanitize_bool(*ret)),
        ),
        Type::Tuple(elms) => Type::Tuple(elms.into_iter().map(|elm| sanitize_bool(elm)).collect()),
        Type::Array(elm) => Type::Array(Box::new(sanitize_bool(*elm))),
    }
}

/// syntax::Expr 型の式を受け取り、knormal::Expr 型の式とその型をペアで返す
/// この際、式中の変数名はかぶらないようにα変換された上で、各変数の型を env に登録していく
fn convert_expr(
    expr: syntax::Expr<VarType>,
    env: &mut FnvHashMap<String, Type>,
    alpha: &mut FnvHashMap<String, Vec<String>>,
    extenv: &FnvHashMap<String, VarType>,
) -> (Box<Expr>, Type) {
    let expr_span = expr.span.clone();
    let wrap = |raw_expr: RawExpr| Box::new(Spanned::new(raw_expr, expr_span));
    let wrap_syntax_expr =
        |raw_expr: syntax::RawExpr<crate::ty::VarType>| Box::new(Spanned::new(raw_expr, expr_span));

    match expr.item {
        syntax::RawExpr::Unit => (wrap(RawExpr::Unit), Type::Unit),
        syntax::RawExpr::Bool(boolean) => {
            (wrap(RawExpr::Int(if boolean { 1 } else { 0 })), Type::Int)
        }
        syntax::RawExpr::Int(integer) => (wrap(RawExpr::Int(integer)), Type::Int),
        syntax::RawExpr::Float(float) => (wrap(RawExpr::Float(float)), Type::Float),
        syntax::RawExpr::UnOp {
            op: syntax::UnaryOp::Not,
            exp,
        } => convert_expr(
            *wrap_syntax_expr(syntax::RawExpr::If {
                cond: exp,
                exp_then: (wrap_syntax_expr(syntax::RawExpr::Bool(false))),
                exp_else: (wrap_syntax_expr(syntax::RawExpr::Bool(true))),
            }),
            env,
            alpha,
            extenv,
        ),
        syntax::RawExpr::UnOp { op, exp } => {
            let k_exp = convert_expr(*exp, env, alpha, extenv);
            let (k_op, res_type) = match op {
                syntax::UnaryOp::Neg => (UnaryOp::Neg, Type::Int),
                syntax::UnaryOp::FNeg => (UnaryOp::FNeg, Type::Float),
                syntax::UnaryOp::Not => unreachable!(),
            };
            let env = Rc::new(RefCell::new(env));
            (
                insert_let(
                    k_exp,
                    env,
                    Box::new(|id| RawExpr::UnOp {
                        op: k_op.clone(),
                        id,
                    }),
                ),
                res_type,
            )
        }
        syntax::RawExpr::BiOp {
            exp_left,
            op,
            exp_right,
        } => {
            if op == syntax::BinaryOp::Eq || op == syntax::BinaryOp::LEq {
                let expr = wrap_syntax_expr(syntax::RawExpr::If {
                    cond: wrap_syntax_expr(syntax::RawExpr::BiOp {
                        exp_left: exp_left,
                        op: op,
                        exp_right: exp_right,
                    }),
                    exp_then: wrap_syntax_expr(syntax::RawExpr::Bool(true)),
                    exp_else: wrap_syntax_expr(syntax::RawExpr::Bool(false)),
                });
                convert_expr(*expr, env, alpha, extenv)
            } else {
                let k_exp_left = convert_expr(*exp_left, env, alpha, extenv);
                let k_exp_right = convert_expr(*exp_right, env, alpha, extenv);
                let k_op = match op {
                    syntax::BinaryOp::Add => BinaryOp::Add,
                    syntax::BinaryOp::Sub => BinaryOp::Sub,
                    syntax::BinaryOp::Mul => BinaryOp::Mul,
                    syntax::BinaryOp::Div => BinaryOp::Div,
                    syntax::BinaryOp::FAdd => BinaryOp::FAdd,
                    syntax::BinaryOp::FSub => BinaryOp::FSub,
                    syntax::BinaryOp::FMul => BinaryOp::FMul,
                    syntax::BinaryOp::FDiv => BinaryOp::FDiv,
                    syntax::BinaryOp::Eq | syntax::BinaryOp::LEq => unreachable!(),
                };
                let res_type = match op {
                    syntax::BinaryOp::Add
                    | syntax::BinaryOp::Sub
                    | syntax::BinaryOp::Mul
                    | syntax::BinaryOp::Div => Type::Int,
                    syntax::BinaryOp::FAdd
                    | syntax::BinaryOp::FSub
                    | syntax::BinaryOp::FMul
                    | syntax::BinaryOp::FDiv => Type::Float,
                    syntax::BinaryOp::Eq | syntax::BinaryOp::LEq => unreachable!(),
                };
                let env = Rc::new(RefCell::new(env));
                (
                    insert_let(
                        k_exp_left,
                        env.clone(),
                        Box::new(|id_left| {
                            insert_let(
                                k_exp_right,
                                env.clone(),
                                Box::new(|id_right| RawExpr::BiOp {
                                    id_left,
                                    op: k_op,
                                    id_right,
                                }),
                            )
                            .item
                        }),
                    ),
                    res_type,
                )
            }
        }
        syntax::RawExpr::Apply { fun, args } => {
            if let syntax::RawExpr::Var(fun_id) = &fun.item {
                if !env.contains_key(fun_id) {
                    if extenv.contains_key(fun_id) {
                        log::info!("Assuming {fun_id} as external function");
                        // 外部関数の場合
                        let fun_type = match extenv.get(fun_id) {
                            Some(t) => t,
                            None => {
                                panic!("internal compiler error")
                            },
                        }.clone().to_type().unwrap();
                        let k_args: Vec<_> = args
                            .into_iter()
                            .map(|arg| convert_expr(arg, env, alpha, extenv))
                            .collect();
                        return match fun_type {
                            Type::Fun(_, return_type) => {
                                let env = Rc::new(RefCell::new(env));
                                let apply: Box<dyn FnOnce(Vec<String>) -> RawExpr> = Box::new(|arg_ids| {
                                    RawExpr::ExtApply {
                                        fun: fun_id.clone(),
                                        args: arg_ids,
                                    }
                                });
                                (
                                    wrap(k_args.into_iter().rev().fold(apply, |acc, arg| {
                                        Box::new(|mut arg_ids: Vec<String>| {
                                            insert_let(
                                                arg,
                                                env.clone(),
                                                Box::new(|arg_id| {
                                                    arg_ids.push(arg_id);
                                                    acc(arg_ids)
                                                }),
                                            )
                                            .item
                                        })
                                    })(vec![])),
                                    *return_type,
                                )
                            }
                            _ => {
                                panic!("internal compiler error")
                            }
                        }
                    }
                }
            }
            // 外部関数でない場合
            let k_fun = convert_expr(*fun, env, alpha, extenv);
            let fun_type = k_fun.1.clone();
            let k_args: Vec<_> = args
                .into_iter()
                .map(|arg| convert_expr(arg, env, alpha, extenv))
                .collect();
            match fun_type {
                Type::Fun(_, return_type) => {
                    let env = Rc::new(RefCell::new(env));
                    let apply: Box<dyn FnOnce(Vec<String>) -> RawExpr> = Box::new(|arg_ids| {
                        insert_let(
                            k_fun,
                            env.clone(),
                            Box::new(|fun_id| RawExpr::Apply {
                                fun: fun_id,
                                args: arg_ids,
                            }),
                        )
                        .item
                    });
                    (
                        wrap(k_args.into_iter().rev().fold(apply, |acc, arg| {
                            Box::new(|mut arg_ids: Vec<String>| {
                                insert_let(
                                    arg,
                                    env.clone(),
                                    Box::new(|arg_id| {
                                        arg_ids.push(arg_id);
                                        acc(arg_ids)
                                    }),
                                )
                                .item
                            })
                        })(vec![])),
                        *return_type,
                    )
                }
                _ => {
                    panic!("internal compiler error")
                }
            }
        }
        syntax::RawExpr::If {
            cond,
            exp_then,
            exp_else,
        } => match cond.item {
            syntax::RawExpr::UnOp {
                op: syntax::UnaryOp::Not,
                exp: not_cond,
            } => convert_expr(
                *wrap_syntax_expr(syntax::RawExpr::If {
                    cond: not_cond,
                    exp_then: exp_else,
                    exp_else: exp_then,
                }),
                env,
                alpha,
                extenv,
            ),
            syntax::RawExpr::BiOp {
                exp_left,
                op,
                exp_right,
            } if op == syntax::BinaryOp::Eq || op == syntax::BinaryOp::LEq => {
                let k_exp_left = convert_expr(*exp_left, env, alpha, extenv);
                let k_exp_right = convert_expr(*exp_right, env, alpha, extenv);
                let k_op = match op {
                    syntax::BinaryOp::Eq => CondOp::Eq,
                    syntax::BinaryOp::LEq => CondOp::LEq,
                    _ => unreachable!(),
                };
                let (exp_then, then_type) = convert_expr(*exp_then, env, alpha, extenv);
                let (exp_else, _) = convert_expr(*exp_else, env, alpha, extenv);
                let env = Rc::new(RefCell::new(env));
                (
                    insert_let(
                        k_exp_left,
                        env.clone(),
                        Box::new(|id_left| {
                            insert_let(
                                k_exp_right,
                                env.clone(),
                                Box::new(|id_right| RawExpr::If {
                                    id_left,
                                    op: k_op,
                                    id_right,
                                    exp_then,
                                    exp_else,
                                }),
                            )
                            .item
                        }),
                    ),
                    then_type,
                )
            }
            cond_item => convert_expr(
                *wrap_syntax_expr(syntax::RawExpr::If {
                    cond: wrap_syntax_expr(syntax::RawExpr::BiOp {
                        exp_left: wrap_syntax_expr(cond_item),
                        op: syntax::BinaryOp::Eq,
                        exp_right: wrap_syntax_expr(syntax::RawExpr::Bool(false)),
                    }),
                    exp_then: exp_else,
                    exp_else: exp_then,
                }),
                env,
                alpha,
                extenv,
            ),
        },
        syntax::RawExpr::LetIn {
            var,
            exp_var,
            exp_suc,
        } => {
            let old_var_name = var.item.name;
            let new_var_name = generate_id(&old_var_name);
            let (exp_var, type_var) = convert_expr(*exp_var, env, alpha, extenv);
            env.insert(new_var_name.clone(), type_var);
            let alpha_key = alpha.entry(old_var_name.clone()).or_insert(vec![]);
            alpha_key.push(new_var_name.clone());
            let (exp_suc, type_suc) = convert_expr(*exp_suc, env, alpha, extenv);
            alpha.get_mut(&old_var_name).unwrap().pop();
            (
                wrap(RawExpr::LetIn {
                    id: new_var_name,
                    exp_id: exp_var,
                    exp_suc: exp_suc,
                }),
                type_suc,
            )
        }
        syntax::RawExpr::LetRecIn {
            fun,
            args,
            exp_fun,
            exp_suc,
        } => {
            let old_fun_name = fun.item.name;
            let new_fun_name = generate_id(&old_fun_name);
            // fun の型を仮に代入する
            env.insert(
                new_fun_name.clone(),
                sanitize_bool(fun.item.t.to_type().unwrap()),
            );
            let alpha_key = alpha.entry(old_fun_name.clone()).or_insert(vec![]);
            alpha_key.push(new_fun_name.clone());
            let (exp_suc, type_suc) = convert_expr(*exp_suc, env, alpha, extenv);
            let mut old_arg_names = vec![];
            let mut new_arg_names = vec![];
            let mut arg_types = vec![];
            args.into_iter().for_each(|arg| {
                let old_arg_name = arg.item.name;
                let new_arg_name = generate_id(&old_arg_name);
                env.insert(
                    new_arg_name.clone(),
                    sanitize_bool(arg.item.t.clone().to_type().unwrap()),
                );
                let alpha_key = alpha.entry(old_arg_name.clone()).or_insert(vec![]);
                alpha_key.push(new_arg_name.clone());
                old_arg_names.push(old_arg_name);
                new_arg_names.push(new_arg_name);
                arg_types.push(sanitize_bool(arg.item.t.to_type().unwrap()));
            });
            let (exp_fun, fun_ret_type) = convert_expr(*exp_fun, env, alpha, extenv);
            // 実際の型を後に代入 (Bool が戻り値の時などは、こうしないとバグる)
            let fun_type = Type::Fun(arg_types, Box::new(fun_ret_type));
            env.insert(new_fun_name.clone(), fun_type);
            alpha.get_mut(&old_fun_name).unwrap().pop();
            old_arg_names.into_iter().for_each(|old_arg_name| {
                alpha.get_mut(&old_arg_name).unwrap().pop();
            });
            (
                wrap(RawExpr::LetRecIn {
                    fun: new_fun_name,
                    args: new_arg_names,
                    exp_fun,
                    exp_suc,
                }),
                type_suc,
            )
        }
        syntax::RawExpr::LetTupleIn {
            vars,
            exp_var,
            exp_suc,
        } => {
            let k_exp_var = convert_expr(*exp_var, env, alpha, extenv);
            let renamed_vars: Vec<_> = vars
                .iter()
                .map(|elm| {
                    let old_elm_name = elm.item.name.clone();
                    let new_elm_name = generate_id(&old_elm_name);
                    // 仮の型を代入
                    //env.insert(new_elm_name.clone(), elm.item.t.clone().to_type().unwrap());
                    let alpha_key = alpha.entry(old_elm_name.clone()).or_insert(vec![]);
                    alpha_key.push(new_elm_name.clone());
                    new_elm_name
                })
                .collect();
            let elm_types = match &k_exp_var.1 {
                Type::Tuple(elm_types) => elm_types,
                _ => panic!("internal compiler error"),
            };
            // 正しい型を代入
            renamed_vars.iter().zip(elm_types).for_each(|(elm, t)| {
                env.insert(elm.clone(), t.clone());
            });
            let k_exp_suc = convert_expr(*exp_suc, env, alpha, extenv);
            vars.iter().for_each(|elm| {
                let old_elm_name = elm.item.name.clone();
                alpha.get_mut(&old_elm_name).unwrap().pop();
            });
            let res_type = k_exp_suc.1.clone();
            let env = Rc::new(RefCell::new(env));
            let res_expr = insert_let(
                k_exp_var,
                env,
                Box::new(|tuple_id: String| {
                    let mut exp_with_elms = k_exp_suc.0;
                    for (index, new_elm_name) in renamed_vars.into_iter().enumerate() {
                        exp_with_elms = wrap(RawExpr::LetIn {
                            id: new_elm_name,
                            exp_id: wrap(RawExpr::TupleGet {
                                tuple: tuple_id.clone(),
                                index,
                            }),
                            exp_suc: exp_with_elms,
                        })
                    }
                    exp_with_elms.item
                }),
            );
            (res_expr, res_type)
        }
        syntax::RawExpr::NewTuple(exps) => {
            let mut tuple_types = vec![];
            let exp_tuple: Box<dyn FnOnce(Vec<String>) -> RawExpr> =
                Box::new(|elm_ids| RawExpr::NewTuple(elm_ids));
            let k_exps: Vec<_> = exps
                .into_iter()
                .map(|elm| convert_expr(elm, env, alpha, extenv))
                .collect();
            let env = Rc::new(RefCell::new(env));
            let exp_result = wrap(k_exps.into_iter().rev().fold(exp_tuple, |acc, k_elm| {
                tuple_types.push(k_elm.1.clone());
                Box::new(|mut elm_ids| {
                    insert_let(
                        k_elm,
                        env.clone(),
                        Box::new(|elm_id| {
                            elm_ids.push(elm_id);
                            acc(elm_ids)
                        }),
                    )
                    .item
                })
            })(vec![]));
            tuple_types.reverse();
            (exp_result, Type::Tuple(tuple_types))
        }
        syntax::RawExpr::NewArray { size, value } => {
            let mut type_array = Type::Unit;
            let k_value = convert_expr(*value, env, alpha, extenv);
            let k_size = convert_expr(*size, env, alpha, extenv);
            let env = Rc::new(RefCell::new(env));
            let exp_array = insert_let(
                k_size,
                env.clone(),
                Box::new(|size_id| {
                    let elm_type = k_value.1.clone();
                    type_array = Type::Array(Box::new(elm_type.clone()));
                    insert_let(
                        k_value,
                        env.clone(),
                        Box::new(|value_id| {
                            let external_fun_name = match elm_type.clone() {
                                Type::Float => "create_float_array",
                                _ => "create_array",
                            }
                            .to_string();
                            RawExpr::ExtApply {
                                fun: external_fun_name,
                                args: vec![size_id, value_id],
                            }
                        }),
                    )
                    .item
                }),
            );
            (exp_array, type_array)
        }
        syntax::RawExpr::ArrayGet { array, index } => {
            let k_array = convert_expr(*array, env, alpha, extenv);
            let k_index = convert_expr(*index, env, alpha, extenv);
            match k_array.1.clone() {
                Type::Array(elm_type) => {
                    let env = Rc::new(RefCell::new(env));
                    (
                        insert_let(
                            k_array,
                            env.clone(),
                            Box::new(|array_id| {
                                insert_let(
                                    k_index,
                                    env.clone(),
                                    Box::new(|index_id| RawExpr::ArrayGet {
                                        array: array_id,
                                        index: index_id,
                                    }),
                                )
                                .item
                            }),
                        ),
                        *elm_type,
                    )
                }
                _ => panic!("internal compiler error"),
            }
        }
        syntax::RawExpr::ArrayPut {
            array,
            index,
            value,
        } => {
            let k_array = convert_expr(*array, env, alpha, extenv);
            let k_index = convert_expr(*index, env, alpha, extenv);
            let k_value = convert_expr(*value, env, alpha, extenv);
            let env = Rc::new(RefCell::new(env));
            (
                insert_let(
                    k_array,
                    env.clone(),
                    Box::new(|array_id: String| {
                        insert_let(
                            k_index,
                            env.clone(),
                            Box::new(|index_id: String| {
                                insert_let(
                                    k_value,
                                    env.clone(),
                                    Box::new(|value_id: String| RawExpr::ArrayPut {
                                        array: array_id.clone(),
                                        index: index_id.clone(),
                                        value: value_id.clone(),
                                    }),
                                )
                                .item
                            }),
                        )
                        .item
                    }),
                ),
                Type::Unit,
            )
        }
        syntax::RawExpr::Var(id) => {
            if let Some(renamed_ids) = alpha.get(&id) {
                let renamed_id = renamed_ids.last().unwrap();
                let var_type = env.get(renamed_id).unwrap().clone();
                (wrap(RawExpr::Var(renamed_id.clone())), var_type)
            } else {
                let var_type = extenv.get(&id).unwrap().clone();
                (
                    wrap(RawExpr::ExtArray { array: id }),
                    sanitize_bool(var_type.to_type().unwrap()),
                )
            }
        }
    }
}

pub fn k_normalize(
    expr: syntax::Expr<VarType>,
    extenv: &FnvHashMap<String, VarType>,
) -> (Expr, FnvHashMap<String, Type>) {
    let mut env = FnvHashMap::default();
    let mut alpha = FnvHashMap::default();
    let (expr, _) = convert_expr(expr, &mut env, &mut alpha, extenv);
    for (name, t) in extenv.iter() {
        env.insert(format!("min_caml_{}", name.clone()), sanitize_bool(t.clone().to_type().unwrap()));
    }
    (*expr, env)
}
