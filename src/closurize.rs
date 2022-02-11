use std::collections::{HashMap, HashSet};

use crate::knormal::{BinaryOp, CondOp, UnaryOp};
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
            op: op.clone(),
            id: id.clone(),
        }),
        knormal::RawExpr::BiOp {
            id_left,
            op,
            id_right,
        } => wrap(RawExpr::BiOp {
            id_left: id_left.clone(),
            op: op.clone(),
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
            wrap(RawExpr::If {
                id_left: id_left.clone(),
                op: op.clone(),
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

// 式 (Expr) の型検査
fn typecheck_expr(
    expr: &Expr,
    env: &HashMap<String, Type>,
    scope: &mut HashSet<String>,
    freevars_dict: &HashMap<String, Vec<String>>,
) -> Type {
    let get_type = |id| match env.get(id) {
        Some(t) => t.clone(),
        None => {
            panic!("{} is not in list", id);
        }
    };
    match &expr.item {
        RawExpr::Unit => Type::Unit,
        RawExpr::Int(_) => Type::Int,
        RawExpr::Float(_) => Type::Float,
        RawExpr::Var(id) => {
            if !scope.contains(id) {
                panic!("typecheck for closurized expression failed!");
            }
            get_type(id)
        }
        RawExpr::UnOp { op, id } => {
            let res = match op {
                UnaryOp::Neg => Type::Int,
                UnaryOp::FNeg => Type::Float,
            };
            let actual = get_type(id);
            if actual != res {
                panic!("typecheck for closurized expression failed!");
            }
            res
        }
        RawExpr::BiOp {
            id_left,
            op,
            id_right,
        } => {
            let res = match op {
                BinaryOp::Add
                | BinaryOp::Sub
                | BinaryOp::Mul
                | BinaryOp::Div
                | BinaryOp::LShift
                | BinaryOp::RShift => Type::Int,
                BinaryOp::FAdd | BinaryOp::FSub | BinaryOp::FMul | BinaryOp::FDiv => Type::Float,
            };
            let actual_left = get_type(id_left);
            let actual_right = get_type(id_right);
            if actual_left != res || actual_right != res {
                panic!("typecheck for closurized expression failed!");
            }
            res
        }
        RawExpr::If {
            id_left,
            op,
            id_right,
            exp_then,
            exp_else,
        } => {
            let type_left = get_type(id_left);
            let type_right = get_type(id_right);
            if type_left != type_right {
                panic!("typecheck for closurized expression failed!");
            }
            if *op == CondOp::LEq {
                if type_left != Type::Int && type_right != Type::Float {
                    panic!("typecheck for closurized expression failed!");
                }
            }
            let type_then = typecheck_expr(exp_then, env, scope, freevars_dict);
            let type_else = typecheck_expr(exp_else, env, scope, freevars_dict);
            if type_then != type_else {
                panic!("typecheck for closurized expression failed!");
            }
            type_then
        }
        RawExpr::LetIn {
            id,
            exp_id,
            exp_suc,
        } => {
            let type_id = typecheck_expr(exp_id, env, scope, freevars_dict);
            if type_id != env.get(id).unwrap().clone() {
                panic!("typecheck for closurized expression failed!");
            }
            if !scope.insert(id.clone()) {
                panic!("typecheck for closurized expression failed!");
            }
            let type_suc = typecheck_expr(exp_suc, env, scope, freevars_dict);
            scope.remove(id);
            type_suc
        }
        RawExpr::ApplyCls { cls, args } => {
            let type_cls = env.get(cls).unwrap().clone();
            if !scope.contains(cls) {
                panic!("typecheck for closurized expression failed!");
            }
            let actual_type_args: Vec<_> = args
                .iter()
                .map(|arg| env.get(arg).unwrap().clone())
                .collect();
            match type_cls {
                Type::Fun(type_args, type_ret) => {
                    if actual_type_args != *type_args {
                        panic!("typecheck for closurized expression failed!");
                    }
                    *type_ret
                }
                _ => panic!("typecheck for closurized expression failed!"),
            }
        }
        RawExpr::ApplyDir { tag, args } => {
            // min_caml_create_array / min_caml_create_float_array のみ例外的に処理 (多相型なので扱いが難しい)
            if tag == "min_caml_create_array" {
                if env.get(args.get(0).unwrap()).unwrap() != &Type::Int {
                    panic!("typecheck for closurized expression failed!");
                }
                return Type::Array(Box::new(env.get(args.get(1).unwrap()).unwrap().clone()));
            }
            if tag == "min_caml_create_float_array" {
                if env.get(args.get(0).unwrap()).unwrap() != &Type::Int {
                    panic!("typecheck for closurized expression failed!");
                }
                if env.get(args.get(1).unwrap()).unwrap() != &Type::Float {
                    panic!("typecheck for closurized expression failed!");
                }
                return Type::Array(Box::new(Type::Float));
            }
            log::debug!("{tag}");
            let type_tag = env.get(tag).unwrap().clone();
            if scope.contains(tag) {
                panic!("typecheck for closurized expression failed!");
            }
            let actual_type_args: Vec<_> = args
                .iter()
                .map(|arg| env.get(arg).unwrap().clone())
                .collect();
            match type_tag {
                Type::Fun(type_args, type_ret) => {
                    if actual_type_args != *type_args {
                        panic!("typecheck for closurized expression failed!");
                    }
                    *type_ret
                }
                _ => panic!("typecheck for closurized expression failed!"),
            }
        }
        RawExpr::NewTuple(elms) => Type::Tuple(
            elms.iter()
                .map(|elm| env.get(elm).unwrap().clone())
                .collect(),
        ),
        RawExpr::NewClosure { tag, free_vars } => {
            let type_tag = env.get(tag).unwrap().clone();
            let freevars_page = freevars_dict.get(tag).unwrap();
            if freevars_page != free_vars {
                panic!("typecheck for closurized expression failed!")
            }
            type_tag
        }
        RawExpr::TupleGet { tuple, index } => {
            let type_tuple = env.get(tuple).unwrap().clone();
            match type_tuple {
                Type::Tuple(elms) => {
                    if *index >= elms.len() {
                        panic!("typecheck for closurized expression failed!")
                    }
                    elms.get(*index).unwrap().clone()
                }
                _ => panic!("typecheck for closurized expression failed!"),
            }
        }
        RawExpr::ArrayGet { array, index } => {
            let type_array = get_type(array);
            let type_index = get_type(index);
            if type_index != Type::Int {
                panic!("typecheck for closurized expression failed!")
            }
            match type_array {
                Type::Array(elm) => *elm,
                _ => panic!("typecheck for closurized expression failed!"),
            }
        }
        RawExpr::ArrayPut {
            array,
            index,
            value,
        } => {
            let type_array = get_type(array);
            let type_index = get_type(index);
            let type_value = get_type(value);
            if type_index != Type::Int {
                panic!("typecheck for closurized expression failed!")
            }
            let actual_type_value = match type_array {
                Type::Array(elm) => *elm,
                _ => panic!("typecheck for closurized expression failed!"),
            };
            if type_value != actual_type_value {
                panic!("typecheck for closurized expression failed!")
            }
            Type::Unit
        }
        RawExpr::ExtArray { array } => get_type(array),
    }
}

fn typecheck_function(
    function: &Function,
    env: &HashMap<String, Type>,
    freevars_dict: &HashMap<String, Vec<String>>,
) {
    let get_type = |id| env.get(id).unwrap().clone();
    let mut scope: HashSet<String> = HashSet::new();
    let type_tag = env.get(&function.tag).unwrap().clone();
    // 自由変数がある場合、クロージャーとみなし、scope に自分自身を入れる
    if !freevars_dict.get(&function.tag).unwrap().is_empty() {
        scope.insert(function.tag.clone());
    }
    let actual_type_args: Vec<_> = function.args.iter().map(|arg| get_type(arg)).collect();
    function.args.iter().for_each(|arg| {
        if !scope.insert(arg.clone()) {
            panic!("typecheck for closurized expression failed!")
        }
    });
    function.free_vars.iter().for_each(|var| {
        if !scope.insert(var.clone()) {
            panic!("typecheck for closurized expression failed!")
        }
    });
    let type_ret = match type_tag {
        Type::Fun(args, ret) => {
            if args != actual_type_args {
                panic!("typecheck for closurized expression failed!")
            }
            *ret
        }
        _ => panic!("typecheck for closurized expression failed!"),
    };
    let actual_type_ret = typecheck_expr(&function.body, env, &mut scope, freevars_dict);
    if type_ret != actual_type_ret {
        panic!("typecheck for closurized expression failed!")
    }
}

pub fn typecheck(expr: &Expr, env: &HashMap<String, Type>, toplevels: &Vec<Function>) {
    let mut freevars_dict = HashMap::<String, Vec<String>>::new();
    for function in toplevels {
        freevars_dict.insert(function.tag.clone(), function.free_vars.clone());
    }
    let mut scope = HashSet::new();
    typecheck_expr(expr, env, &mut scope, &freevars_dict);
    if !scope.is_empty() {
        panic!("typecheck for closurized expression failed!")
    }
    for function in toplevels {
        typecheck_function(function, env, &freevars_dict)
    }
    log::info!("Closure typecheck finished");
}

#[cfg(test)]
mod tests {
    use crate::closurize::*;
    #[test]
    #[should_panic]
    fn applycls_on_dir() {
        let wrap = |x| Box::new(Spanned::new(x, (0, 0)));
        let expr1 = *wrap(RawExpr::LetIn {
            id: "x".to_string(),
            exp_id: wrap(RawExpr::Int(1)),
            exp_suc: wrap(RawExpr::ApplyCls {
                cls: "f".to_string(),
                args: vec!["x".to_string()],
            }),
        });
        let mut env = HashMap::new();
        env.insert(
            "f".to_string(),
            Type::Fun(vec![Type::Int], Box::new(Type::Unit)),
        );
        env.insert("x".to_string(), Type::Int);
        let mut scope = HashSet::new();
        let mut freevars_dict = HashMap::new();
        freevars_dict.insert("f".to_string(), vec![]);
        typecheck_expr(&expr1, &env, &mut scope, &freevars_dict);
    }

    #[test]
    #[should_panic]
    fn applydir_on_cls() {
        let wrap = |x| Box::new(Spanned::new(x, (0, 0)));
        let expr2 = *wrap(RawExpr::LetIn {
            id: "x".to_string(),
            exp_id: wrap(RawExpr::Int(1)),
            exp_suc: wrap(RawExpr::LetIn {
                id: "y".to_string(),
                exp_id: wrap(RawExpr::Float(2.0)),
                exp_suc: wrap(RawExpr::LetIn {
                    id: "f".to_string(),
                    exp_id: wrap(RawExpr::NewClosure {
                        tag: "f".to_string(),
                        free_vars: vec!["y".to_string()],
                    }),
                    exp_suc: wrap(RawExpr::ApplyCls {
                        cls: "f".to_string(),
                        args: vec!["x".to_string()],
                    }),
                }),
            }),
        });
        let mut env = HashMap::new();
        env.insert(
            "f".to_string(),
            Type::Fun(vec![Type::Int], Box::new(Type::Unit)),
        );
        env.insert("x".to_string(), Type::Int);
        env.insert("y".to_string(), Type::Float);
        let mut scope = HashSet::new();
        let mut freevars_dict = HashMap::new();
        freevars_dict.insert("f".to_string(), vec![]);
        typecheck_expr(&expr2, &env, &mut scope, &freevars_dict);
    }

    #[test]
    #[should_panic]
    fn wrong_closure() {
        let wrap = |x| Box::new(Spanned::new(x, (0, 0)));
        let expr3 = *wrap(RawExpr::LetIn {
            id: "x".to_string(),
            exp_id: wrap(RawExpr::Int(1)),
            exp_suc: wrap(RawExpr::LetIn {
                id: "z".to_string(),
                exp_id: wrap(RawExpr::Float(2.0)),
                exp_suc: wrap(RawExpr::LetIn {
                    id: "f".to_string(),
                    exp_id: wrap(RawExpr::NewClosure {
                        tag: "f".to_string(),
                        free_vars: vec!["z".to_string()],
                    }),
                    exp_suc: wrap(RawExpr::ApplyCls {
                        cls: "f".to_string(),
                        args: vec!["x".to_string()],
                    }),
                }),
            }),
        });
        let mut env = HashMap::new();
        env.insert(
            "f".to_string(),
            Type::Fun(vec![Type::Int], Box::new(Type::Unit)),
        );
        env.insert("x".to_string(), Type::Int);
        env.insert("y".to_string(), Type::Float);
        let mut scope = HashSet::new();
        let mut freevars_dict = HashMap::new();
        freevars_dict.insert("f".to_string(), vec!["y".to_string()]);
        assert_eq!(
            typecheck_expr(&expr3, &env, &mut scope, &freevars_dict),
            Type::Unit
        );
    }

    #[test]
    fn applycls_on_cls() {
        let wrap = |x| Box::new(Spanned::new(x, (0, 0)));
        let expr3 = *wrap(RawExpr::LetIn {
            id: "x".to_string(),
            exp_id: wrap(RawExpr::Int(1)),
            exp_suc: wrap(RawExpr::LetIn {
                id: "y".to_string(),
                exp_id: wrap(RawExpr::Float(2.0)),
                exp_suc: wrap(RawExpr::LetIn {
                    id: "f".to_string(),
                    exp_id: wrap(RawExpr::NewClosure {
                        tag: "f".to_string(),
                        free_vars: vec!["y".to_string()],
                    }),
                    exp_suc: wrap(RawExpr::ApplyCls {
                        cls: "f".to_string(),
                        args: vec!["x".to_string()],
                    }),
                }),
            }),
        });
        let mut env = HashMap::new();
        env.insert(
            "f".to_string(),
            Type::Fun(vec![Type::Int], Box::new(Type::Unit)),
        );
        env.insert("x".to_string(), Type::Int);
        env.insert("y".to_string(), Type::Float);
        let mut scope = HashSet::new();
        let mut freevars_dict = HashMap::new();
        freevars_dict.insert("f".to_string(), vec!["y".to_string()]);
        assert_eq!(
            typecheck_expr(&expr3, &env, &mut scope, &freevars_dict),
            Type::Unit
        );
    }
}
