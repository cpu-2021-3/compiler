use std::collections::{HashMap, HashSet};

use crate::{closure::*, knormal, span::Spanned, ty::Type};

fn free_vars(expr: &RawExpr) -> HashSet<String> {
    match expr {
        RawExpr::Unit | RawExpr::Int(_) | RawExpr::Float(_) => HashSet::new(),
        RawExpr::ExtArray { array: _ } => HashSet::new(),
        RawExpr::Var(id) => vec![id.clone()].into_iter().collect(),
        RawExpr::UnOp { op: _, id } => vec![id.clone()].into_iter().collect(),
        RawExpr::BiOp {
            id_left,
            op: _,
            id_right,
        } => vec![id_left.clone(), id_right.clone()]
            .into_iter()
            .collect(),
        RawExpr::If {
            id_left,
            op: _,
            id_right,
            exp_then,
            exp_else,
        } => {
            let exp_then = free_vars(&exp_then.item);
            let exp_else = free_vars(&exp_else.item);
            let mut res: HashSet<_> = vec![id_left.clone(), id_right.clone()]
                .into_iter()
                .collect();
            res.extend(exp_then);
            res.extend(exp_else);
            res
        }
        RawExpr::LetIn {
            id,
            exp_id,
            exp_suc,
        } => {
            let mut res = free_vars(&exp_suc.item);
            res.remove(id);
            res.extend(free_vars(&exp_id.item));
            res
        }
        RawExpr::ApplyCls { cls, args } => {
            let mut res: HashSet<String> = args.clone().into_iter().collect();
            res.insert(cls.clone());
            res
        }
        RawExpr::ApplyDir { tag: _, args } => args.clone().into_iter().collect(),
        RawExpr::NewTuple(elms) => elms.clone().into_iter().collect(),
        RawExpr::NewClosure { tag: _, free_vars } => free_vars.clone().into_iter().collect(),
        RawExpr::TupleGet { tuple, index: _ } => vec![tuple.clone()].into_iter().collect(),
        RawExpr::ArrayGet { array, index } => {
            vec![array.clone(), index.clone()].into_iter().collect()
        }
        RawExpr::ArrayPut {
            array,
            index,
            value,
        } => vec![array.clone(), index.clone(), value.clone()]
            .into_iter()
            .collect(),
    }
}

fn convert_expr(
    expr: &knormal::Expr,
    env: &HashMap<String, Type>,
    direct_funs: &mut HashSet<String>,
) -> (Box<Expr>, Vec<Function>) {
    let expr_span = expr.span.clone();
    let wrap = |raw_expr: RawExpr| Box::new(Spanned::new(raw_expr, expr_span));
    let mut toplevels: Vec<Function> = vec![];

    let expr = match &expr.item {
        knormal::RawExpr::Unit => wrap(RawExpr::Unit),
        knormal::RawExpr::Int(integer) => wrap(RawExpr::Int(*integer)),
        knormal::RawExpr::Float(float) => wrap(RawExpr::Float(*float)),
        knormal::RawExpr::Var(id) => wrap(RawExpr::Var(id.clone())),
        knormal::RawExpr::UnOp { op, id } => wrap(RawExpr::UnOp {
            op: match op {
                knormal::UnaryOp::Neg => UnaryOp::Neg,
                knormal::UnaryOp::FNeg => UnaryOp::FNeg,
            },
            id: id.clone(),
        }),
        knormal::RawExpr::BiOp {
            id_left,
            op,
            id_right,
        } => wrap(RawExpr::BiOp {
            id_left: id_left.clone(),
            op: match op {
                knormal::BinaryOp::Add => BinaryOp::Add,
                knormal::BinaryOp::Sub => BinaryOp::Sub,
                knormal::BinaryOp::Mul => BinaryOp::Mul,
                knormal::BinaryOp::Div => BinaryOp::Div,
                knormal::BinaryOp::FAdd => BinaryOp::FAdd,
                knormal::BinaryOp::FSub => BinaryOp::FSub,
                knormal::BinaryOp::FMul => BinaryOp::FMul,
                knormal::BinaryOp::FDiv => BinaryOp::FDiv,
            },
            id_right: id_right.clone(),
        }),
        knormal::RawExpr::If {
            id_left,
            op,
            id_right,
            exp_then,
            exp_else,
        } => {
            let (exp_then, top_then) = convert_expr(exp_then, env, direct_funs);
            let (exp_else, top_else) = convert_expr(exp_else, env, direct_funs);
            toplevels.extend(top_then);
            toplevels.extend(top_else);
            let op = match op {
                knormal::CondOp::Eq => CondOp::Eq,
                knormal::CondOp::LEq => CondOp::LEq,
            };
            wrap(RawExpr::If {
                id_left: id_left.clone(),
                op,
                id_right: id_right.clone(),
                exp_then,
                exp_else,
            })
        }
        knormal::RawExpr::LetIn {
            id,
            exp_id,
            exp_suc,
        } => {
            let (exp_id, top_id) = convert_expr(exp_id, env, direct_funs);
            let (exp_suc, top_suc) = convert_expr(exp_suc, env, direct_funs);
            toplevels.extend(top_id);
            toplevels.extend(top_suc);
            wrap(RawExpr::LetIn {
                id: id.clone(),
                exp_id: exp_id,
                exp_suc: exp_suc,
            })
        }
        knormal::RawExpr::LetRecIn {
            fun,
            args,
            exp_fun,
            exp_suc,
        } => {
            // fun がクロージャー無しでトップレベルにできると仮定して、exp_fun をクロージャー変換
            direct_funs.insert(fun.clone());
            let (c_exp_fun, new_toplevels) = convert_expr(&exp_fun, env, direct_funs);
            // exp_fun 中の自由変数 (引数除く) を計算
            let arg_set = args.iter().cloned().collect();
            let exp_fun_frees = free_vars(&c_exp_fun.item);
            let exp_fun_frees: HashSet<_> = exp_fun_frees.difference(&arg_set).collect();
            let c_exp_fun = if !exp_fun_frees.is_empty() {
                // exp_fun に自由変数がある場合は、fun を直接呼出しする関数の集合 direct_funs から外し、もう一度変換
                log::info!("Function {} has free variables: {:?}", fun, exp_fun_frees);
                direct_funs.remove(fun);
                let (c_exp_fun, new_toplevels) = convert_expr(&exp_fun, env, direct_funs);
                // exp_fun に含まれた new_toplevels を toplevels に追加
                toplevels.extend(new_toplevels);
                c_exp_fun
            } else {
                // exp_fun に自由変数が無い場合は、何もしない
                // exp_fun に含まれた new_toplevels を toplevels に追加
                log::info!("Function {} has no free variables", fun);
                toplevels.extend(new_toplevels);
                c_exp_fun
            };
            // クロージャーに含む必要のある自由変数を取得
            let mut fun_arg_set = arg_set;
            fun_arg_set.insert(fun.clone());
            let exp_fun_frees: Vec<String> = free_vars(&c_exp_fun.item)
                .difference(&fun_arg_set)
                .cloned()
                .collect();
            // toplevels に自分自身を追加
            toplevels.push(Function {
                tag: fun.clone(),
                args: args.to_vec(),
                free_vars: exp_fun_frees.clone(),
                body: *c_exp_fun,
            });
            // exp_suc をクロージャー変換
            let (c_exp_suc, new_toplevels) = convert_expr(exp_suc, env, direct_funs);
            toplevels.extend(new_toplevels);
            if free_vars(&c_exp_suc.item).contains(fun) {
                // 関数自身を自由変数として含む場合はクロージャー定義を追加
                wrap(RawExpr::LetIn {
                    id: fun.clone(),
                    exp_id: wrap(RawExpr::NewClosure {
                        tag: fun.clone(),
                        free_vars: exp_fun_frees,
                    }),
                    exp_suc: c_exp_suc,
                })
            } else {
                // クロージャーが必要無い場合は何もしない
                c_exp_suc
            }
        }
        knormal::RawExpr::Apply { fun, args } => {
            if direct_funs.contains(fun) {
                wrap(RawExpr::ApplyDir {
                    tag: fun.clone(),
                    args: args.to_vec(),
                })
            } else {
                wrap(RawExpr::ApplyCls {
                    cls: fun.clone(),
                    args: args.to_vec(),
                })
            }
        }
        knormal::RawExpr::NewTuple(elms) => wrap(RawExpr::NewTuple(elms.to_vec())),
        knormal::RawExpr::TupleGet { tuple, index } => wrap(RawExpr::TupleGet {
            tuple: tuple.clone(),
            index: *index,
        }),
        knormal::RawExpr::ArrayGet { array, index } => wrap(RawExpr::ArrayGet {
            array: array.clone(),
            index: index.clone(),
        }),
        knormal::RawExpr::ArrayPut {
            array,
            index,
            value,
        } => wrap(RawExpr::ArrayPut {
            array: array.clone(),
            index: index.clone(),
            value: value.clone(),
        }),
        knormal::RawExpr::ExtArray { array } => wrap(RawExpr::ExtArray {
            array: array.clone(),
        }),
        knormal::RawExpr::ExtApply { fun, args } => wrap(RawExpr::ApplyDir {
            tag: format!("min_caml_{}", fun),
            args: args.to_vec(),
        }),
    };

    (expr, toplevels)
}

pub fn closurize(expr: knormal::Expr, env: &HashMap<String, Type>) -> (Expr, Vec<Function>) {
    let mut direct_funs: HashSet<String> = vec![].into_iter().collect();
    let (expr, toplevels) = convert_expr(&expr, env, &mut direct_funs);
    (*expr, toplevels)
}
