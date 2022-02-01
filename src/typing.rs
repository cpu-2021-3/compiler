use std::collections::HashMap;

use crate::syntax::RawExpr::*;
use crate::syntax::{BinaryOp::*, Expr, RawTypedVar, TypedVar, UnaryOp::*};
use crate::ty::VarType;
use crate::{code, span::*};
use anyhow::Result;
use std::{cell::RefCell, rc::Rc};

#[derive(Debug)]
pub enum TypingError {
    WrongType {
        span: Span,
        should_be: VarType,
        but_is: VarType,
    },
    UnequalType {
        span1: Span,
        span2: Span,
        t1: VarType,
        t2: VarType,
    },
    Recursive {
        span: Span,
    },
}

impl TypingError {
    pub fn message(&self) -> String {
        let frag = match self {
            TypingError::WrongType { span, .. } => (code::indexed_fragment(span)),
            TypingError::UnequalType { span1, span2, .. } => {
                format!(
                    "{} and {}",
                    code::indexed_fragment(span1),
                    code::indexed_fragment(span2)
                )
            }
            TypingError::Recursive { span } => (code::indexed_fragment(span)),
        };
        format!("type error: {}", frag)
    }
}

enum UnificationError {
    Recursive,
    Unequal,
}

/// 型変数 r が t 中に出現するかを返す
fn occurs(r: &Rc<RefCell<Option<VarType>>>, t: &VarType) -> bool {
    match t {
        VarType::Fun(targs, tret) => targs.iter().any(|targ| occurs(r, targ)) || occurs(r, tret),
        VarType::Tuple(telms) => telms.iter().any(|telm| occurs(r, telm)),
        VarType::Array(tarr) => occurs(r, tarr),
        VarType::Var(rother) => {
            if Rc::ptr_eq(r, rother) {
                true
            } else {
                match &*rother.borrow() {
                    Some(tother) => occurs(r, &tother),
                    None => false,
                }
            }
        }
        VarType::Unit | VarType::Bool | VarType::Int | VarType::Float => false,
    }
}

/// 型方程式 t1 = t2 を解く
/// 成功時: t1, t2 中の型変数が書き変わる
/// 失敗時: Err を投げる
fn unify(t1: &VarType, t2: &VarType) -> Result<(), UnificationError> {
    match (t1, t2) {
        (VarType::Var(r1), VarType::Var(r2)) if Rc::ptr_eq(r1, r2) => Ok(()),
        (VarType::Var(r1), _) if r1.borrow().is_some() => {
            let t1 = r1.borrow();
            unify(&t1.clone().unwrap(), t2)
        }
        (_, VarType::Var(r2)) if r2.borrow().is_some() => {
            let t2 = r2.borrow();
            unify(t1, &t2.clone().unwrap())
        }
        (VarType::Var(r1), _) => {
            if occurs(r1, t2) {
                return Err(UnificationError::Recursive);
            }
            *r1.borrow_mut() = Some(t2.clone());
            Ok(())
        }
        (_, VarType::Var(r2)) => {
            if occurs(r2, t1) {
                return Err(UnificationError::Recursive);
            }
            *r2.borrow_mut() = Some(t1.clone());
            Ok(())
        }
        (VarType::Fun(t1a, t1r), VarType::Fun(t2a, t2r)) => {
            if t1a.len() != t2a.len() {
                return Err(UnificationError::Unequal);
            }
            for (a1, a2) in t1a.iter().zip(t2a) {
                unify(a1, a2)?
            }
            unify(t1r, t2r)
        }
        (VarType::Tuple(t1s), VarType::Tuple(t2s)) => {
            if t1s.len() != t2s.len() {
                return Err(UnificationError::Unequal);
            }
            for (s1, s2) in t1s.iter().zip(t2s) {
                unify(s1, s2)?
            }
            Ok(())
        }
        (VarType::Array(t1), VarType::Array(t2)) => unify(t1, t2),
        (t1, t2) => {
            if t1 != t2 {
                return Err(UnificationError::Unequal);
            }
            Ok(())
        }
    }
}

/// 一つの式を infer し、ある型と unify する
fn unify_one_expr(
    t: &VarType,
    expr: &Expr<VarType>,
    env: &mut HashMap<String, VarType>,
    extenv: &mut HashMap<String, VarType>,
) -> Result<(), TypingError> {
    let inferred_type = infer_expr(env, expr, extenv)?;
    unify(&t, &inferred_type).map_err(|u| match u {
        UnificationError::Recursive => TypingError::Recursive { span: expr.span },
        UnificationError::Unequal => TypingError::WrongType {
            span: expr.span,
            should_be: t.clone(),
            but_is: inferred_type,
        },
    })
}

// 二つの式を infer し、その二つの型を unify する
fn unify_two_exprs(
    exp1: &Expr<VarType>,
    exp2: &Expr<VarType>,
    env: &mut HashMap<String, VarType>,
    extenv: &mut HashMap<String, VarType>,
) -> Result<(), TypingError> {
    let _res = unify_two_exprs_and_return_type(exp1, exp2, env, extenv)?;
    Ok(())
}

// 二つの式を infer し、その二つの型を unify する (unify したあと型を返す)
fn unify_two_exprs_and_return_type(
    exp1: &Expr<VarType>,
    exp2: &Expr<VarType>,
    env: &mut HashMap<String, VarType>,
    extenv: &mut HashMap<String, VarType>,
) -> Result<VarType, TypingError> {
    let inferred_type1 = infer_expr(env, exp1, extenv)?;
    let inferred_type2 = infer_expr(env, exp2, extenv)?;
    unify(&inferred_type1, &inferred_type2).map_err(|u| match u {
        UnificationError::Recursive => TypingError::Recursive { span: exp1.span },
        UnificationError::Unequal => TypingError::UnequalType {
            span1: exp1.span,
            t1: inferred_type1.clone(),
            span2: exp2.span,
            t2: inferred_type2.clone(),
        },
    })?;
    Ok(inferred_type1)
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
            UnOp { op, exp } => UnOp {
                op,
                exp: Box::new(exp.solve()),
            },
            BiOp {
                exp_left,
                op,
                exp_right,
            } => BiOp {
                exp_left: Box::new(exp_left.solve()),
                op,
                exp_right: Box::new(exp_right.solve()),
            },
            Apply { fun, args } => Apply {
                fun: Box::new(fun.solve()),
                args: args.into_iter().map(|arg| arg.solve()).collect(),
            },
            If {
                cond,
                exp_then,
                exp_else,
            } => If {
                cond: Box::new(cond.solve()),
                exp_then: Box::new(exp_then.solve()),
                exp_else: Box::new(exp_else.solve()),
            },
            LetIn {
                var,
                exp_var,
                exp_suc,
            } => LetIn {
                var: var.solve(),
                exp_var: Box::new(exp_var.solve()),
                exp_suc: Box::new(exp_suc.solve()),
            },
            LetRecIn {
                fun,
                args,
                exp_fun,
                exp_suc,
            } => LetRecIn {
                fun: fun.solve(),
                args: args.into_iter().map(|arg| arg.solve()).collect(),
                exp_fun: Box::new(exp_fun.solve()),
                exp_suc: Box::new(exp_suc.solve()),
            },
            LetTupleIn {
                vars,
                exp_var,
                exp_suc,
            } => LetTupleIn {
                vars: vars.into_iter().map(|var| var.solve()).collect(),
                exp_var: Box::new(exp_var.solve()),
                exp_suc: Box::new(exp_suc.solve()),
            },
            NewTuple(elms) => NewTuple(elms.into_iter().map(|elm| elm.solve()).collect()),
            NewArray { size, value } => NewArray {
                size: Box::new(size.solve()),
                value: Box::new(value.solve()),
            },
            ArrayGet { array, index } => ArrayGet {
                array: Box::new(array.solve()),
                index: Box::new(index.solve()),
            },
            ArrayPut {
                array,
                index,
                value,
            } => ArrayPut {
                array: Box::new(array.solve()),
                index: Box::new(index.solve()),
                value: Box::new(value.solve()),
            },

            Unit | Bool(_) | Int(_) | Float(_) | Var(_) => self.item,
        };
        Spanned::new(new_item, self.span)
    }
}

impl SolveVarType for TypedVar<VarType> {
    fn solve(self) -> Self {
        Spanned::new(
            RawTypedVar::<VarType> {
                name: self.item.name,
                t: self.item.t.solve(),
            },
            self.span,
        )
    }
}

impl SolveVarType for VarType {
    fn solve(self) -> Self {
        match self {
            VarType::Fun(args, ret) => VarType::Fun(
                args.into_iter().map(|arg| arg.solve()).collect(),
                Box::new(ret.solve()),
            ),
            VarType::Tuple(elms) => {
                VarType::Tuple(elms.into_iter().map(|elm| elm.solve()).collect())
            }
            VarType::Array(elm) => VarType::Array(Box::new(elm.solve())),
            VarType::Var(var) if var.borrow().is_some() => {
                let solved_t = var.take().unwrap().solve();
                *var.borrow_mut() = Some(solved_t.clone());
                solved_t
            }
            VarType::Var(var) => {
                log::info!("Uninstantiated type variable assumed as int");
                *var.borrow_mut() = Some(VarType::Int);
                VarType::Int
            }
            VarType::Bool | VarType::Int | VarType::Float | VarType::Unit => self,
        }
    }
}

/// 型環境 env のもとで expr の型を推論する
fn infer_expr(
    env: &mut HashMap<String, VarType>,
    expr: &Expr<VarType>,
    extenv: &mut HashMap<String, VarType>,
) -> Result<VarType, TypingError> {
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
            unify_one_expr(&inferred_type, exp, env, extenv)?;
            inferred_type
        }
        BiOp {
            exp_left,
            op,
            exp_right,
        } => match op {
            Add | Sub | Mul | Div => {
                unify_one_expr(&VarType::Int, exp_left, env, extenv)?;
                unify_one_expr(&VarType::Int, exp_right, env, extenv)?;
                VarType::Int
            }
            FAdd | FSub | FMul | FDiv => {
                unify_one_expr(&VarType::Float, exp_left, env, extenv)?;
                unify_one_expr(&VarType::Float, exp_right, env, extenv)?;
                VarType::Float
            }
            Eq | LEq => {
                unify_two_exprs(exp_left, exp_right, env, extenv)?;
                VarType::Bool
            }
        },
        Apply { fun, args } => {
            let return_type = VarType::new();
            let arg_types = args
                .iter()
                .map(|arg| infer_expr(env, arg, extenv))
                .collect::<Result<_, _>>()?;
            unify_one_expr(
                &VarType::Fun(arg_types, Box::new(return_type.clone())),
                fun,
                env,
                extenv,
            )?;
            return_type
        }
        If {
            cond,
            exp_then,
            exp_else,
        } => {
            unify_one_expr(&VarType::Bool, cond, env, extenv)?;
            unify_two_exprs_and_return_type(exp_then, exp_else, env, extenv)?
        }
        LetIn {
            var,
            exp_var,
            exp_suc,
        } => {
            unify_one_expr(&var.item.t, exp_var, env, extenv)?;
            env.insert(var.item.name.clone(), var.item.t.clone());
            let suc_type = infer_expr(env, exp_suc, extenv)?;
            env.remove(&var.item.name);
            suc_type
        }
        LetRecIn {
            fun,
            args,
            exp_fun,
            exp_suc,
        } => {
            env.insert(fun.item.name.clone(), fun.item.t.clone());
            let whole_type = infer_expr(env, exp_suc, extenv)?;
            for arg in args {
                env.insert(arg.item.name.clone(), arg.item.t.clone());
            }
            let return_type = infer_expr(env, exp_fun, extenv)?;
            for arg in args {
                env.remove(&arg.item.name);
            }
            env.remove(&fun.item.name);
            let fun_should_be = VarType::Fun(
                args.iter().map(|arg| arg.item.t.clone()).collect(),
                Box::new(return_type),
            );
            unify(&fun.item.t, &fun_should_be).map_err(|u| match u {
                UnificationError::Recursive => TypingError::Recursive { span: fun.span },
                UnificationError::Unequal => TypingError::WrongType {
                    span: fun.span,
                    should_be: fun_should_be,
                    but_is: fun.item.t.clone(),
                },
            })?;
            whole_type
        }
        LetTupleIn {
            vars,
            exp_var,
            exp_suc,
        } => {
            unify_one_expr(
                &VarType::Tuple(vars.iter().map(|var| var.item.t.clone()).collect()),
                exp_var,
                env,
                extenv,
            )?;
            for var in vars {
                env.insert(var.item.name.clone(), var.item.t.clone());
            }
            let suc_type = infer_expr(env, exp_suc, extenv)?;
            for var in vars {
                env.remove(&var.item.name);
            }
            suc_type
        }
        NewTuple(tuple) => VarType::Tuple(
            tuple
                .iter()
                .map(|elm| infer_expr(env, elm, extenv))
                .collect::<Result<_, _>>()?,
        ),
        NewArray { size, value } => {
            unify_one_expr(&VarType::Int, size, env, extenv)?;
            VarType::Array(Box::new(infer_expr(env, value, extenv)?))
        }
        ArrayGet { array, index } => {
            let result_type = VarType::new();
            unify_one_expr(
                &VarType::Array(Box::new(result_type.clone())),
                array,
                env,
                extenv,
            )?;
            unify_one_expr(&VarType::Int, index, env, extenv)?;
            result_type
        }
        ArrayPut {
            array,
            index,
            value,
        } => {
            let element_type = infer_expr(env, value, extenv)?;
            unify_one_expr(
                &VarType::Array(Box::new(element_type.clone())),
                array,
                env,
                extenv,
            )?;
            unify_one_expr(&VarType::Int, index, env, extenv)?;
            VarType::Unit
        }
        Var(name) => {
            if let Some(t) = env.get(name) {
                t.clone()
            } else if let Some(t) = extenv.get(name) {
                t.clone()
            } else {
                log::info!("Free variable {} assumed as external.", name);
                let t = VarType::new();
                extenv.insert(name.clone(), t.clone());
                t
            }
        }
    };
    Ok(result)
}

pub fn do_typing(
    expr: Expr<VarType>,
) -> Result<(Expr<VarType>, HashMap<String, VarType>), TypingError> {
    let mut env = HashMap::new();
    let mut extenv = HashMap::new();
    unify_one_expr(&VarType::Unit, &expr, &mut env, &mut extenv)?;
    extenv.iter_mut().for_each(|(_name, t)| {
        *t = t.clone().solve();
    });
    Ok((expr.solve(), extenv))
}
