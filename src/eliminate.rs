use crate::{knormal::{Expr, RawExpr}, span::Spanned};

impl RawExpr {
    // 副作用があるかどうか
    pub fn has_side_effect(&self) -> bool {
        match self {
            RawExpr::If { id_left: _, op: _, id_right: _, exp_then, exp_else } => {
                exp_then.item.has_side_effect() || exp_else.item.has_side_effect()
            },
            RawExpr::LetIn { id: _, exp_id, exp_suc } => {
                exp_id.item.has_side_effect() || exp_suc.item.has_side_effect()
            },
            RawExpr::LetRecIn { fun: _, args: _, exp_fun: _, exp_suc } => {
                exp_suc.item.has_side_effect()
            },
            RawExpr::Apply { fun: _, args: _ } |
            RawExpr::ArrayPut { array: _, index: _, value: _ } |
            RawExpr::ExtApply { fun: _, args: _ } => true,
            _ => false
        }
    }
}

fn eliminate(expr: Expr) -> Box<Expr> {
    let raw_expr = match expr.item {
        RawExpr::If { id_left, op, id_right, exp_then, exp_else } => {
            let exp_then = eliminate(*exp_then);
            let exp_else = eliminate(*exp_else);
            RawExpr::If { id_left, op, id_right, exp_then, exp_else }
        },
        RawExpr::LetIn { id, exp_id, exp_suc } => {
            let exp_id = eliminate(*exp_id);
            let exp_suc = eliminate(*exp_suc);
            if !exp_id.item.has_side_effect() && !exp_suc.item.free_vars().contains(&id) {
                log::info!("Eliminating variable {id}");
                exp_suc.item
            }
            else {
                RawExpr::LetIn {
                    id,
                    exp_id,
                    exp_suc,
                }
            }
        },
        RawExpr::LetRecIn { fun, args, exp_fun, exp_suc } => {
            let exp_suc = eliminate(*exp_suc);
            if !exp_suc.item.free_vars().contains(&fun) {
                log::info!("Eliminating function {fun}");
                exp_suc.item
            }
            else {
                let exp_fun = eliminate(*exp_fun);
                RawExpr::LetRecIn {
                    fun,
                    args,
                    exp_fun,
                    exp_suc,
                }
            }
        },
        otherwise => otherwise
    };
    Box::new(Spanned::new(raw_expr, expr.span))
}

pub fn eliminate_dead_code(expr: Expr) -> Expr {
    *eliminate(expr)
}