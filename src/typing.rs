use std::collections::HashMap;

use crate::syntax::{Expr, UnaryOp::*, BinaryOp::*, Spanned, TypedVar};
use crate::syntax::RawExpr::*;
use crate::ty::{VarType};
use std::{rc::Rc, cell::RefCell};
use anyhow::Result;

/// 型変数 r が t 中に出現するかを返す
fn occurs(r: &Rc<RefCell<Option<VarType>>>, t: &VarType) -> bool{
    match t {
        VarType::Fun(targs, tret) => {
            targs.iter().any(|targ| occurs(r, targ)) || occurs(r, tret)
        },
        VarType::Tuple(telms) => {
            telms.iter().any(|telm| occurs(r, telm))
        },
        VarType::Array(tarr) => {
            occurs(r, tarr)
        },
        VarType::Var(rother) => {
            if Rc::ptr_eq(r, rother) {
                true
            }
            else {
                match &*rother.borrow() {
                    Some(tother) => {
                        occurs(r, &tother)
                    },
                    None => false
                }
            }
        },
        VarType::Unit | VarType::Bool | VarType::Int | VarType::Float => 
            false
    }
}

/// 型方程式 t1 = t2 を解く
/// 成功時: t1, t2 中の型変数が書き変わる
/// 失敗時: Err を投げる
fn unify(t1: &VarType, t2: &VarType) -> Result<()> {
    match (t1, t2) {
        (VarType::Var(r1), VarType::Var(r2)) if Rc::ptr_eq(r1, r2) => {
            Ok(())
        },
        (VarType::Var(r1), _) if r1.borrow().is_some() => {
            let t1 = r1.borrow();
            unify(&t1.clone().unwrap(), t2)
        },
        (_, VarType::Var(r2)) if r2.borrow().is_some() => {
            let t2 = r2.borrow();
            unify(t1, &t2.clone().unwrap())
        },
        (VarType::Var(r1), _) => {
            if occurs(r1, t2) {
                return Err(anyhow::anyhow!("Unification error: {:?} occurs in {:?}", r1, t2));
            }
            *r1.borrow_mut() = Some(t2.clone());
            Ok(())
        },
        (_, VarType::Var(r2)) => {
            if occurs(r2, t1) {
                return Err(anyhow::anyhow!("Unification error: {:?} occurs in {:?}", r2, t1));
            }
            *r2.borrow_mut() = Some(t1.clone());
            Ok(())
        }
        (VarType::Fun(t1a, t1r), VarType::Fun(t2a, t2r)) => {
            if t1a.len() != t2a.len() {
                return Err(anyhow::anyhow!("Unification error: function arguments {:?} and {:?} have different size", t1a, t2a));
            }
            for (a1, a2) in t1a.iter().zip(t2a) {
                unify(a1, a2)?
            }
            unify(t1r, t2r)
        }
        (VarType::Tuple(t1s), VarType::Tuple(t2s)) => {
            if t1s.len() != t2s.len() {
                return Err(anyhow::anyhow!("Unification error: function arguments {:?} and {:?} have different size", t1s, t2s));
            }
            for (s1, s2) in t1s.iter().zip(t2s) {
                unify(s1, s2)?
            }
            Ok(())
        }
        (VarType::Array(t1), VarType::Array(t2)) => {
            unify(t1, t2)
        }
        (t1, t2) => {
            if t1 != t2 {
                return Err(anyhow::anyhow!("Unification error: type {:?} and {:?} cannot be unified", t1, t2));
            }
            Ok(())
        }
    }
}

trait SolveVarType {
    /// 型変数の中身を代入する
    /// 例えば、v1 = v2; v2 = Fun(v3, v4); v3 = Int; v4 = Unit; が、
    /// v1 = Fun(Int, Unit), v2 = Fun(Int, Unit), v3 = Int, v4 = Unit になる
    fn solve(self) -> Self;
}

impl SolveVarType for Expr<VarType> {
    fn solve(self) -> Self {
        let new_item = match self.item {
            UnOp { op, exp } => 
                UnOp{op, exp: Box::new(exp.solve())},
            BiOp { exp_left, op, exp_right } => 
                BiOp{exp_left: Box::new(exp_left.solve()), op, exp_right: Box::new(exp_right.solve())},
            Apply { fun, args } => 
                Apply{fun: Box::new(fun.solve()), args: args.into_iter().map(|arg| arg.solve()).collect()},
            If { cond, exp_then, exp_else } => 
                If{cond: Box::new(cond.solve()), exp_then: Box::new(exp_then.solve()), exp_else: Box::new(exp_else.solve())},
            LetIn { var, exp_var, exp_suc } => 
                LetIn{var: var.solve(), exp_var: Box::new(exp_var.solve()), exp_suc: Box::new(exp_suc.solve())},
            LetRecIn { fun, args, exp_fun, exp_suc } => 
                LetRecIn {fun: fun.solve(), args: args.into_iter().map(|arg| arg.solve()).collect(), exp_fun: Box::new(exp_fun.solve()), exp_suc: Box::new(exp_suc.solve())},
            LetTupleIn { vars, exp_var, exp_suc } => 
                LetTupleIn {vars: vars.into_iter().map(|var| var.solve()).collect(), exp_var: Box::new(exp_var.solve()), exp_suc: Box::new(exp_suc.solve())},
            NewTuple(elms ) => 
                NewTuple(elms.into_iter().map(|elm| elm.solve()).collect()),
            NewArray { size, value } => 
                NewArray{size: Box::new(size.solve()), value: Box::new(value.solve())},
            ArrayGet { array, index } => 
                ArrayGet{array: Box::new(array.solve()), index: Box::new(index.solve())},
            ArrayPut { array, index, value } => 
                ArrayPut{array: Box::new(array.solve()), index: Box::new(index.solve()), value: Box::new(value.solve())},
                
            Unit | Bool(_) | Int(_) | Float(_) | Var(_) => self.item 
        };
        Spanned::new(new_item, self.span)
    }
}

impl SolveVarType for TypedVar<VarType> {
    fn solve(self) -> Self {
        Self {name: self.name, t: self.t.solve()}
    }
}

impl SolveVarType for VarType {
    fn solve(self) -> Self {
        match self {
            VarType::Fun(args, ret) => 
                VarType::Fun(args.into_iter().map(|arg| arg.solve()).collect(), Box::new(ret.solve())),
            VarType::Tuple(elms) => 
                VarType::Tuple(elms.into_iter().map(|elm| elm.solve()).collect()),
            VarType::Array(elm) => 
                VarType::Array(Box::new(elm.solve())),
            VarType::Var(var) if var.borrow().is_some() => {
                let solved_t = var.take().unwrap().solve();
                *var.borrow_mut() = Some(solved_t.clone());
                solved_t
            },
            VarType::Var(var) => {
                log::info!("Uninstantiated type variable assumed as int");
                *var.borrow_mut() = Some(VarType::Int);
                VarType::Int
            },    
            VarType::Bool | VarType::Int | VarType::Float | VarType::Unit => self
        }
    }
}

/// 型環境 env のもとで expr の型を推論する
fn infer_expr(env: &mut HashMap<String, VarType>, expr: &Expr<VarType>, extenv: &mut HashMap<String, VarType>) -> Result<VarType> {
    let result = match &expr.item {
        Unit => VarType::Unit,
        Bool(_) => VarType::Bool,
        Int(_) => VarType::Int,
        Float(_) => VarType::Float,
        UnOp { op, exp } => {
            let inferred_type = match op {
                Not => VarType::Bool,
                Neg => VarType::Int,
                FNeg => VarType::Float,
            };
            unify(&inferred_type, &infer_expr(env, exp, extenv)?)?;
            inferred_type
        },
        BiOp { exp_left, op, exp_right } => {
            match op {
                Add | Sub | Mul | Div => {
                    unify(&VarType::Int, &infer_expr(env, exp_left, extenv)?)?;
                    unify(&VarType::Int, &infer_expr(env, exp_right, extenv)?)?;
                    VarType::Int
                },
                FAdd | FSub | FMul | FDiv => {
                    unify(&VarType::Float, &infer_expr(env, exp_left, extenv)?)?;
                    unify(&VarType::Float, &infer_expr(env, exp_right, extenv)?)?;
                    VarType::Float
                },
                Eq | LEq => {
                    unify(&infer_expr(env,exp_left, extenv)?, &infer_expr(env, exp_right, extenv)?)?;
                    VarType::Bool
                }
            }
        },
        Apply { fun, args } => {
            let return_type = VarType::new();
            let arg_types = args.iter().map(|arg| infer_expr(env, arg, extenv)).collect::<Result<_>>()?;
            unify(&infer_expr(env, fun, extenv)?, &VarType::Fun(arg_types, Box::new(return_type.clone())))?;
            return_type
        },
        If { cond, exp_then, exp_else } => {
            unify(&infer_expr(env, cond, extenv)?, &VarType::Bool)?;
            let type_then = infer_expr(env, exp_then, extenv)?;
            let type_else = infer_expr(env, exp_else, extenv)?;
            unify(&type_then, &type_else)?;
            type_then
        },
        LetIn { var, exp_var, exp_suc } => {
            unify(&var.t, &infer_expr(env, exp_var, extenv)?)?;
            env.insert(var.name.clone(),  var.t.clone());
            infer_expr(env, exp_suc, extenv)?
        },
        LetRecIn { fun, args, exp_fun, exp_suc } => {
            env.insert(fun.name.clone(), fun.t.clone());
            let whole_type = infer_expr(env, exp_suc, extenv)?;
            for arg in args {
                env.insert(arg.name.clone(), arg.t.clone());
            }
            let return_type = infer_expr(env, exp_fun, extenv)?;
            unify(&fun.t, &VarType::Fun(args.iter().map(|arg| arg.t.clone()).collect(), Box::new(return_type)))?;
            whole_type
        },
        LetTupleIn { vars, exp_var, exp_suc } => {
            unify(&VarType::Tuple(vars.iter().map(|var| var.t.clone()).collect()), &infer_expr(env, exp_var, extenv)?)?;
            for var in vars {
                env.insert(var.name.clone(), var.t.clone());
            };
            infer_expr(env, exp_suc, extenv)?
        },
        NewTuple(tuple) => {
            VarType::Tuple(tuple.iter().map(|elm| infer_expr(env, elm, extenv)).collect::<Result<_>>()?)
        },
        NewArray { size, value } => {
            unify(&infer_expr(env, size, extenv)?, &VarType::Int)?;
            VarType::Array(Box::new(infer_expr(env, value, extenv)?))
        },
        ArrayGet { array, index } => {
            let result_type = VarType::new();
            unify(&VarType::Array(Box::new(result_type.clone())), &infer_expr(env, array, extenv)?)?;
            unify(&VarType::Int, &infer_expr(env, index, extenv)?)?;
            result_type
        },
        ArrayPut { array, index, value } => {
            let element_type = infer_expr(env, value, extenv)?;
            unify(&VarType::Array(Box::new(element_type.clone())), &infer_expr(env,array, extenv)?)?;
            unify(&VarType::Int, &infer_expr(env, index, extenv)?)?;
            element_type
        },
        Var(name) => {
            if let Some(t) = env.get(name) {
                t.clone()
            }
            else if let Some(t) = extenv.get(name) {
                t.clone()
            }
            else {
                log::info!("Free variable {} assumed as external.", name);
                let t = VarType::new();
                extenv.insert(name.clone(),t.clone());
                t
            }
        },
    };
    Ok(result)
}

pub fn do_typing(expr: Expr<VarType>) -> Result<(Expr<VarType>, HashMap<String, VarType>)> {
    let mut env = HashMap::new();
    let mut extenv = HashMap::new();
    unify(&VarType::Unit,&infer_expr(&mut env, &expr,&mut extenv)?)?;
    extenv.iter_mut().for_each(|(_name ,t )| {*t = t.clone().solve();});
    Ok((expr.solve(), extenv))
}
