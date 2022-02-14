use fnv::FnvHashMap;

use crate::{knormal::{Expr, RawExpr}, ty::Type, span::Spanned, id::generate_id};

static EXPAND_SIZE_LIMIT: u32 = 200;

// 式中に登場する変数 (束縛変数含む) の名前を新しく付け直す
fn rename(expr: Expr, name_map: &mut FnvHashMap<String, String>, k_env: &mut FnvHashMap<String, Type>) -> Box<Expr> {
    let raw_expr = match expr.item {
        RawExpr::Unit => RawExpr::Unit,
        RawExpr::Int(i) => RawExpr::Int(i),
        RawExpr::Float(f) => RawExpr::Float(f),
        RawExpr::Var(id) => {
            RawExpr::Var(name_map.get(&id).unwrap_or(&id).clone())
        },
        RawExpr::UnOp { op, id } => {
            RawExpr::UnOp{ op, id: name_map.get(&id).unwrap_or(&id).clone() }
        },
        RawExpr::BiOp { id_left, op, id_right } => {
            RawExpr::BiOp{ id_left: name_map.get(&id_left).unwrap_or(&id_left).clone(), op, id_right: name_map.get(&id_right).unwrap_or(&id_right).clone() }
        },
        RawExpr::If { id_left, op, id_right, exp_then, exp_else } => {
            RawExpr::If {
                id_left: name_map.get(&id_left).unwrap_or(&id_left).clone(),
                op,
                id_right: name_map.get(&id_right).unwrap_or(&id_right).clone(),
                exp_then: rename(*exp_then, name_map, k_env),
                exp_else: rename(*exp_else, name_map, k_env),
            }
        },
        RawExpr::LetIn { id, exp_id, exp_suc } => {
            let id_type = k_env.get(&id).unwrap().clone();
            let new_id = generate_id(&id);
            k_env.insert(new_id.clone(), id_type);
            let exp_id = rename(*exp_id, name_map, k_env);
            name_map.insert(id.clone(), new_id.clone());
            let exp_suc = rename(*exp_suc, name_map, k_env);
            name_map.remove(&id).unwrap();
            RawExpr::LetIn {
                id: new_id,
                exp_id,
                exp_suc,
            }
        },
        RawExpr::LetRecIn { fun, args, exp_fun, exp_suc } => {
            let fun_type = k_env.get(&fun).unwrap().clone();
            let new_fun = generate_id(&fun);
            k_env.insert(new_fun.clone(), fun_type);
            let arg_types: Vec<_> = args.iter().map(|arg| k_env.get(arg).unwrap().clone()).collect();
            let new_args: Vec<_> = args.iter().map(|arg| generate_id(arg)).collect();
            new_args.iter().zip(arg_types).for_each(|(arg, arg_type)| {
                k_env.insert(arg.clone(), arg_type);    
            });
            name_map.insert(fun.clone(), new_fun.clone());
            let exp_suc = rename(*exp_suc, name_map, k_env);
            args.iter().zip(new_args.iter()).for_each(|(arg, new_arg)| {
                name_map.insert(arg.clone(), new_arg.clone());
            });
            let exp_fun = rename(*exp_fun, name_map, k_env);
            args.iter().for_each(|arg| {
                name_map.remove(arg).unwrap();
            });
            name_map.remove(&fun).unwrap();
            RawExpr::LetRecIn {
                fun: new_fun,
                args: new_args,
                exp_fun,
                exp_suc,
            }
        },
        RawExpr::Apply { fun, args } => {
            let fun = name_map.get(&fun).unwrap_or(&fun).clone();
            let args = args.into_iter().map(|arg| name_map.get(&arg).unwrap_or(&arg).clone()).collect();
            RawExpr::Apply {
                fun,
                args
            }
        },
        RawExpr::NewTuple(elms) => {
            let elms = elms.into_iter().map(|elm| name_map.get(&elm).unwrap_or(&elm).clone()).collect();
            RawExpr::NewTuple(elms)
        },
        RawExpr::TupleGet { tuple, index } => {
            RawExpr::TupleGet {
                tuple: name_map.get(&tuple).unwrap_or(&tuple).clone(),
                index,
            }
        },
        RawExpr::ArrayGet { array, index } => {
            RawExpr::ArrayGet {
                array: name_map.get(&array).unwrap_or(&array).clone(),
                index: name_map.get(&index).unwrap_or(&index).clone(),
            }
        },
        RawExpr::ArrayPut { array, index, value } => {
            RawExpr::ArrayPut {
                array: name_map.get(&array).unwrap_or(&array).clone(),
                index: name_map.get(&index).unwrap_or(&index).clone(),
                value: name_map.get(&value).unwrap_or(&value).clone(),
            }
        },
        RawExpr::ExtArray { array } => {
            RawExpr::ExtArray {
                array,
            }
        },
        RawExpr::ExtApply { fun, args } => {
            RawExpr::ExtApply {
                fun,
                args: args.into_iter().map(|arg| name_map.get(&arg).unwrap_or(&arg).clone()).collect(),
            }
        },
    };
    Box::new(Spanned::new(raw_expr, expr.span))
}

fn expand(expr: Expr, k_env: &mut FnvHashMap<String, Type>, small_funs: &mut FnvHashMap<String, (Vec<String>, Expr)>) -> Box<Expr> {
    let raw_expr= match expr.item {
        RawExpr::If { id_left, op, id_right, exp_then, exp_else } => {
            let exp_then = expand(*exp_then, k_env, small_funs);
            let exp_else = expand(*exp_else, k_env, small_funs);
            RawExpr::If {id_left, op, id_right, exp_then, exp_else}
        },
        RawExpr::LetIn { id, exp_id, exp_suc } => {
            let exp_id = expand(*exp_id, k_env, small_funs);
            let exp_suc = expand(*exp_suc, k_env, small_funs);
            RawExpr::LetIn {id, exp_id, exp_suc}
        },
        RawExpr::LetRecIn { fun, args, exp_fun, exp_suc } => {
            let size = exp_fun.size();
            if size <= EXPAND_SIZE_LIMIT {
                // 小さい関数はインライン展開をする関数群に追加する
                log::info!("Size of {} is {}: inlining will occur", fun, size);
                small_funs.insert(fun.clone(), (args.clone(), *exp_fun.clone()));
            }
            let exp_fun = expand(*exp_fun, k_env, small_funs);
            let exp_suc = expand(*exp_suc, k_env, small_funs);
            small_funs.remove(&fun);
            RawExpr::LetRecIn {fun, args, exp_fun, exp_suc}
        },
        RawExpr::Apply { fun, args } => {
            if let Some((original_args, expr)) = small_funs.get(&fun) {
                // インライン展開をする場合
                log::info!("Inlining {fun}");
                let mut name_map = FnvHashMap::default();
                original_args.iter().zip(args.iter()).for_each(|(arg, new_arg)| {
                    name_map.insert(arg.clone(), new_arg.clone());
                });
                rename(expr.clone(), &mut name_map, k_env).item
            }
            else {
                RawExpr::Apply {fun, args}
            }
        },
        otherwise => otherwise
    };
    Box::new(Spanned::new(raw_expr, expr.span))
}

pub fn do_inline_expansion(expr: Expr, k_env: &mut FnvHashMap<String, Type>) -> Expr {
    let mut small_funs = FnvHashMap::default();
    *expand(expr, k_env, &mut small_funs)
}