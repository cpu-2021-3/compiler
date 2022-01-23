use std::collections::HashMap;

use crate::knormal::{Expr, RawExpr, UnaryOp, BinaryOp, CondOp};
use crate::span::*;
use crate::syntax;
use crate::ty::{Type, VarType};
use crate::id::generate_id;

// fn insert_let((expr, t): (Box<Expr>, Type), f_exp_t_cont: &dyn Fn(String) -> RawExpr) -> Box<Expr> {
//     match expr.item {
//         RawExpr::Var(id) => wrap(f_exp_t_cont(id)),
//         _ => {
//             let new_id = generate_id("kn");
//             let exp_cont = wrap(f_exp_t_cont(new_id.clone()));
//             wrap(RawExpr::LetIn{
//                 id: new_id, 
//                 exp_id: expr, 
//                 exp_suc: exp_cont})
//         }
//     }
// }

/// syntax::Expr 型の式を受け取り、knormal::Expr 型の式とその型をペアで返す
/// この際、式中の変数名はかぶらないようにα変換された上で、各変数の型を env に登録していく
fn convert_expr(expr: syntax::Expr<VarType>, env: &mut HashMap<String, Type>, alpha: &mut HashMap<String, Vec<String>>, extenv: &HashMap<String, VarType>) -> (Box<Expr>, Type) {
    let expr_span = expr.span.clone();
    let wrap = |raw_expr: RawExpr| Box::new(Spanned::new(raw_expr, expr_span));
    let wrap_syntax_expr = |raw_expr: syntax::RawExpr<crate::ty::VarType>| Box::new(Spanned::new(raw_expr, expr_span));
    // expr, t, f_exp_t_cont を受け取り、
    // let new_var : t = expr in (<f_exp_t_cont(new_var)>)
    // となる式を返す
    let insert_let = |(expr, t) : (Box<Expr>, Type),  f_exp_t_cont: &dyn Fn(String) -> RawExpr| -> Box<Expr> {
        match expr.item {
            RawExpr::Var(id) => wrap(f_exp_t_cont(id)),
            _ => {
                let new_id = generate_id("kn");
                let exp_cont = wrap(f_exp_t_cont(new_id.clone()));
                wrap(RawExpr::LetIn{
                    id: new_id, 
                    exp_id: expr, 
                    exp_suc: exp_cont})
            }
        }
    };
    match expr.item {
        syntax::RawExpr::Unit => (wrap(RawExpr::Unit), Type::Unit),
        syntax::RawExpr::Bool(boolean) => (wrap(RawExpr::Int(if boolean { 1 } else { 0 })), Type::Int),
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
            env, alpha, extenv
        ),
        syntax::RawExpr::UnOp { op, exp } => {
            let k_exp = convert_expr(*exp, env, alpha, extenv);
            let (k_op, res_type) = match op {
                syntax::UnaryOp::Neg => (UnaryOp::Neg, Type::Int),
                syntax::UnaryOp::FNeg => (UnaryOp::FNeg, Type::Float),
                syntax::UnaryOp::Not => unreachable!()
            };
            (insert_let(k_exp, &|id| (RawExpr::UnOp{op: k_op.clone(), id})), res_type)
        },
        syntax::RawExpr::BiOp { exp_left, op, exp_right} => {
            if op == syntax::BinaryOp::Eq || op == syntax::BinaryOp::LEq {
                let expr = wrap_syntax_expr(syntax::RawExpr::If { 
                    cond: wrap_syntax_expr(syntax::RawExpr::BiOp{exp_left: exp_left, op: op, exp_right: exp_right}), 
                    exp_then: wrap_syntax_expr(syntax::RawExpr::Bool(true)), 
                    exp_else: wrap_syntax_expr(syntax::RawExpr::Bool(false)) 
                });
                convert_expr(*expr, env, alpha, extenv)
            }
            else {
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
                    syntax::BinaryOp::Add |
                    syntax::BinaryOp::Sub |
                    syntax::BinaryOp::Mul |
                    syntax::BinaryOp::Div => Type::Int,
                    syntax::BinaryOp::FAdd |
                    syntax::BinaryOp::FSub |
                    syntax::BinaryOp::FMul |
                    syntax::BinaryOp::FDiv => Type::Float,
                    syntax::BinaryOp::Eq |
                    syntax::BinaryOp::LEq => Type::Bool
                };
                (insert_let(k_exp_left, &|id_left| {
                    insert_let(k_exp_right, &|id_right| {
                        RawExpr::BiOp{id_left, op: k_op ,id_right}
                    }).item
                }), res_type)
            }
        },
        syntax::RawExpr::Apply { fun, args } => {
            let k_fun = convert_expr(*fun, env, alpha, extenv);
            let (_, fun_type) = k_fun;
            match fun_type {
                Type::Fun(arg_types, return_type) => {
                    (insert_let(k_fun, &|fun_id| {
                        let apply: Box<dyn FnOnce(Vec<String>) -> RawExpr> = Box::new(|arg_ids| RawExpr::Apply{fun: fun_id, args: arg_ids});
                        args.into_iter().rev().fold(apply, |acc, arg| {
                            Box::new(|mut arg_ids: Vec<String>| {
                                insert_let(convert_expr(arg, env, alpha, extenv), &|arg_id| {
                                    arg_ids.push(arg_id);
                                    acc(arg_ids)
                                }).item
                            })
                        })(vec![])
                    }), *return_type)
                },
                _ => {
                    panic!("internal compiler error")
                }
            }
        },
        syntax::RawExpr::If {
            cond,
            exp_then, exp_else
        } => {
            match cond.item {
                syntax::RawExpr::UnOp {
                    op: syntax::UnaryOp::Not, exp: not_cond
                } => 
                convert_expr(*wrap_syntax_expr(syntax::RawExpr::If{
                    cond: not_cond, exp_then: exp_else, exp_else: exp_then
                }), env, alpha, extenv),
                syntax::RawExpr::BiOp {
                    exp_left, op, exp_right
                } if op == syntax::BinaryOp::Eq || op == syntax::BinaryOp::LEq => {
                    let k_exp_left = convert_expr(*exp_left, env, alpha, extenv);
                    let k_exp_right = convert_expr(*exp_right, env, alpha, extenv);
                    let (_, if_type) = k_exp_left;
                    let k_op = match op {
                        syntax::BinaryOp::Eq => CondOp::Eq,
                        syntax::BinaryOp::LEq => CondOp::LEq,
                        _ => unreachable!()
                    };
                    (insert_let(k_exp_left, &|id_left| {
                        insert_let(k_exp_right, &|id_right| {
                            let (exp_then, res_type) = convert_expr(*exp_then, env, alpha, extenv);
                            let (exp_else, res_type) = convert_expr(*exp_else, env, alpha, extenv);
                            RawExpr::If{id_left, op: k_op, id_right, exp_then, exp_else}
                        }).item
                    }), if_type)
                },
                cond_item => {
                    convert_expr(*wrap_syntax_expr(syntax::RawExpr::If{
                        cond: wrap_syntax_expr(syntax::RawExpr::BiOp{
                            exp_left: wrap_syntax_expr(cond_item),
                            op: syntax::BinaryOp::Eq,
                            exp_right: wrap_syntax_expr(syntax::RawExpr::Bool(false))
                        }),
                        exp_then: exp_else,
                        exp_else: exp_then
                    }), env, alpha, extenv)
                },
            }
        },
        syntax::RawExpr::LetIn {
            var,
            exp_var,
            exp_suc,
        } => {
            let old_var_name = var.item.name;
            let new_var_name = generate_id(&old_var_name);
            let (exp_var, type_var) = convert_expr(*exp_var, env, alpha, extenv);
            env.insert(new_var_name, type_var);
            alpha.entry(old_var_name).or_insert(vec![]);
            alpha[&old_var_name].push(new_var_name);
            let (exp_suc, type_suc) = convert_expr(*exp_suc, env, alpha, extenv);
            alpha[&old_var_name].pop();
            (wrap(RawExpr::LetIn{
                id: new_var_name,
                exp_id: exp_var,
                exp_suc: exp_suc,
            }), type_suc)
        },
        syntax::RawExpr::LetRecIn {
            fun,
            args,
            exp_fun,
            exp_suc,
        } => {
            let old_fun_name = fun.item.name;
            let new_fun_name = generate_id(&old_fun_name);
            env.insert(new_fun_name, fun.item.t.to_type().unwrap());
            alpha.entry(old_fun_name).or_insert(vec![]);
            alpha[&old_fun_name].push(new_fun_name);
            let (exp_suc, type_suc) = convert_expr(*exp_suc, env, alpha, extenv);
            let mut old_arg_names = vec![];
            let mut new_arg_names = vec![];
            args.into_iter().for_each(|arg| {
                let old_arg_name = arg.item.name;
                let new_arg_name = generate_id(&old_arg_name);
                env.insert(new_arg_name, arg.item.t.to_type().unwrap());
                alpha.entry(old_arg_name).or_insert(vec![]);
                alpha[&old_arg_name].push(new_arg_name);
                old_arg_names.push(old_arg_name);
                new_arg_names.push(new_arg_name);
            });
            let (exp_fun, type_fun) = convert_expr(*exp_fun, env, alpha, extenv);
            alpha[&old_fun_name].pop();
            old_arg_names.into_iter().for_each(|old_arg_name|{
                alpha[&old_arg_name].pop();
            });
            (wrap(RawExpr::LetRecIn{
                fun: new_fun_name,
                args: new_arg_names,
                exp_fun,
                exp_suc,
            }), type_suc)
        },
        syntax::RawExpr::LetTupleIn {
            vars,
            exp_var,
            exp_suc,
        } => {
            let res_type = Type::Unit;
            let exp_suc: Box<dyn FnOnce(i32, &mut HashMap<String, Type>, &mut HashMap<String, Vec<String>>) -> RawExpr> = Box::new(|index, env, alpha| {
                let (exp, t) = convert_expr(*exp_suc, env, alpha, extenv);
                res_type = t;
                exp.item
            });
            let res_expr = insert_let(convert_expr(*exp_var, env, alpha, extenv), &|tuple_id| {
                vars.into_iter().rev().fold(exp_suc, |acc, elm| {
                    Box::new(|index, env, alpha| {
                        let old_elm_name = elm.item.name;
                        let new_elm_name = generate_id(&old_elm_name);
                        let index_id = generate_id("ti");
                        env.insert(new_elm_name, elm.item.t.to_type().unwrap());
                        env.insert(index_id, Type::Int);
                        alpha.entry(old_elm_name).or_insert(vec![]);
                        alpha[&old_elm_name].push(new_elm_name);
                        let exp_suc = wrap(acc(index + 1, env, alpha));
                        alpha[&old_elm_name].pop();
                        RawExpr::LetIn{
                            id: index_id,
                            exp_id: wrap(RawExpr::Int(index)),
                            exp_suc: 
                            wrap(RawExpr::LetIn{
                                id: new_elm_name,
                                exp_id: wrap(RawExpr::TupleGet{tuple: tuple_id, index: index_id}),
                                exp_suc
                            })
                        }
                    })
                })(0, env, alpha)
            });
            (res_expr, res_type)
        },
        syntax::RawExpr::NewTuple(exps) => {
            let tuple_types = vec![];
            let exp_tuple: Box<dyn FnOnce(Vec<String>) -> RawExpr> = Box::new(|elm_ids| {
                RawExpr::NewTuple(elm_ids)
            });
            let exp_result = wrap(exps.into_iter().rev().fold(exp_tuple, |acc, elm| {
                Box::new(|mut elm_ids| {
                    let k_elm = convert_expr(elm, env, alpha, extenv);
                    let (_, elm_type) = k_elm;
                    insert_let(k_elm, &|elm_id| {
                        elm_ids.push(elm_id);
                        tuple_types.push(elm_type);
                        acc(elm_ids)
                    }).item
                })
            })(vec![]));
            (exp_result, Type::Tuple(tuple_types))
        },
        syntax::RawExpr::NewArray { size, value } => {
            let mut type_array = Type::Unit;
            let exp_array = insert_let(convert_expr(*size, env, alpha, extenv), &|size_id| {
                let k_value = convert_expr(*value, env, alpha, extenv);
                let (_, value_type) = k_value;
                type_array = Type::Array(Box::new(value_type));
                    insert_let(k_value, &|value_id| {
                    let external_fun_name = match value_type {
                        Type::Float => "create_float_array",
                        _ => "create_array",
                    }.to_string();
                    RawExpr::ExtApply {
                        fun: external_fun_name,
                        args: vec![size_id, value_id]
                    }
                }).item
            });
            (exp_array, type_array)
        },
        syntax::RawExpr::ArrayGet { array, index } => {
            let k_array = convert_expr(*array, env, alpha, extenv);
            match k_array.1 {
                Type::Array(elm_type) => {
                    (insert_let(k_array, &|array_id| {
                        insert_let(convert_expr(*index, env, alpha, extenv), &|index_id| {
                            RawExpr::ArrayGet{array: array_id, index: index_id}
                        }).item
                    }), *elm_type)
                },
                _ => panic!("internal compiler error")
            }
        },
        syntax::RawExpr::ArrayPut {
            array,
            index,
            value,
        } => {
            (insert_let(convert_expr(*array, env, alpha, extenv), &|array_id| {
                insert_let(convert_expr(*index, env, alpha, extenv), &|index_id| {
                    insert_let(convert_expr(*value, env, alpha, extenv), &|value_id| {
                        RawExpr::ArrayPut{ array: array_id.clone(), index: index_id.clone(), value: value_id.clone() }
                    }).item
                }).item
            }), Type::Unit)
        },
        syntax::RawExpr::Var(id) => {
            if let Some(renamed_ids) = alpha.get(&id) {
                let renamed_id = renamed_ids.last().unwrap();
                let var_type = env[renamed_id].clone();
                (wrap(RawExpr::Var(renamed_id.clone())), var_type)
            }
            else {
                let var_type = extenv.get(&id).unwrap().clone();
                (wrap(RawExpr::ExtArray{array: id}), var_type.to_type().unwrap())
            }
        },
    }
}

pub fn k_normalize(expr: syntax::Expr<VarType>, extenv: &HashMap<String, VarType>) -> Expr {
    let mut env = HashMap::new();
    let mut alpha = HashMap::new();
    let (expr, _) = convert_expr(expr, &mut env, &mut alpha, extenv);
    *expr
}