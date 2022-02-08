use std::collections::HashMap;

use crate::{knormal::BinaryOp, span::Spanned};

use super::specific::*;

static EMBED_MIN: i32 = -2048;
static EMBED_MAX: i32 = 2047;

fn embed_expr(expr: Expr, consts: &mut HashMap<String, i32>) -> Box<Expr> {
    let expr_span = expr.span;
    let wrap_expr = |expr| Box::new(Spanned::new(expr, expr_span));
    let wrap = |instr| Box::new(Spanned::new(instr, expr_span));
    let raw_expr = match expr.item {
        RawExpr::LetIn {
            id,
            instr_id,
            instr_suc,
        } => {
            match &instr_id.item {
                RawInstr::Int(i) => {
                    if EMBED_MIN <= *i && *i <= EMBED_MAX {
                        consts.insert(id.clone(), *i);
                    }
                }
                _ => {}
            };
            let embed_instr = match instr_id.item {
                RawInstr::BiOp {
                    id_left,
                    op,
                    id_right,
                } => {
                    if op == BinaryOp::Add || op == BinaryOp::Sub || op == BinaryOp::LShift || op == BinaryOp::RShift {
                        match consts.get(&id_right) {
                            Some(value_right) => RawInstr::BiImm {
                                id_left,
                                op,
                                imm: *value_right,
                            },
                            None => match consts.get(&id_left) {
                                Some(value_left) => RawInstr::BiImm {
                                    id_left: id_right,
                                    op,
                                    imm: *value_left,
                                },
                                None => RawInstr::BiOp {
                                    id_left,
                                    op,
                                    id_right,
                                },
                            },
                        }
                    } else {
                        RawInstr::BiOp {
                            id_left,
                            op,
                            id_right,
                        }
                    }
                }
                RawInstr::If {
                    id_left,
                    op,
                    id_right,
                    exp_then,
                    exp_else,
                } => RawInstr::If {
                    id_left,
                    op,
                    id_right,
                    exp_then: embed_expr(*exp_then, consts),
                    exp_else: embed_expr(*exp_else, consts),
                },
                RawInstr::IfZero {
                    id,
                    exp_then,
                    exp_else,
                } => RawInstr::IfZero {
                    id,
                    exp_then: embed_expr(*exp_then, consts),
                    exp_else: embed_expr(*exp_else, consts),
                },
                otherwise => otherwise,
            };
            RawExpr::LetIn {
                id,
                instr_id: *wrap(embed_instr),
                instr_suc: embed_expr(*instr_suc, consts),
            }
        }
        RawExpr::Is(instr) => RawExpr::Is(instr),
    };
    wrap_expr(raw_expr)
}

pub fn embed(functions: Vec<Function>) -> Vec<Function> {
    functions
        .into_iter()
        .map(|function| {
            let mut consts = HashMap::new();
            Function {
                tag: function.tag,
                args: function.args,
                free_vars: function.free_vars,
                body: *embed_expr(function.body, &mut consts),
            }
        })
        .collect()
}
