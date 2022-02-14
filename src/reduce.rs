use fnv::FnvHashMap;

use crate::{knormal::{Expr, RawExpr}, span::Spanned};

// 変数置き換えテーブルを検索する
fn search(table: &mut FnvHashMap<String, String>, id: String) -> String {
    if let Some(id) = table.get(&id) {
        id.clone()
    }
    else {
        id
    }
}

// β 簡約を行う
fn reduce(expr: Expr, id_table: &mut FnvHashMap<String, String>) -> Box<Expr> {
    let raw_expr = match expr.item {
        RawExpr::Unit => RawExpr::Unit,
        RawExpr::Int(i) => RawExpr::Int(i),
        RawExpr::Float(f) => RawExpr::Float(f),
        RawExpr::Var(id) => RawExpr::Var(search(id_table, id)),
        RawExpr::UnOp { op, id } => RawExpr::UnOp{ op, id: search(id_table, id) },
        RawExpr::BiOp { id_left, op, id_right } => {
            RawExpr::BiOp{ id_left: search(id_table, id_left), op, id_right: search(id_table, id_right) }
        },
        RawExpr::If { id_left, op, id_right, exp_then, exp_else } => {
            let exp_then = reduce(*exp_then, id_table);
            let exp_else = reduce(*exp_else, id_table);
            let id_left = search(id_table, id_left);
            let id_right = search(id_table, id_right);
            RawExpr::If { id_left, op, id_right, exp_then, exp_else }
        },
        RawExpr::LetIn { id, exp_id, exp_suc } => {
            let exp_id = reduce(*exp_id, id_table);
            if let RawExpr::Var(other_id) = &exp_id.item {
                log::info!("{id} will be reduced to {other_id}");
                id_table.insert(id.clone(), other_id.clone());
            }
            let exp_suc = reduce(*exp_suc, id_table);
            RawExpr::LetIn { id, exp_id, exp_suc }
        },
        RawExpr::LetRecIn { fun, args, exp_fun, exp_suc } => {
            let exp_fun = reduce(*exp_fun, id_table);
            let exp_suc = reduce(*exp_suc, id_table);
            RawExpr::LetRecIn { fun, args, exp_fun, exp_suc }
        },
        RawExpr::Apply { fun, args } => {
            let fun = search(id_table, fun);
            let args = args.into_iter().map(|arg| search(id_table, arg)).collect();
            RawExpr::Apply { fun, args }
        },
        RawExpr::NewTuple(elms) => {
            RawExpr::NewTuple(elms.into_iter().map(|elm| search(id_table, elm)).collect())
        },
        RawExpr::TupleGet { tuple, index } => {
            RawExpr::TupleGet{ tuple: search(id_table, tuple), index}
        },
        RawExpr::ArrayGet { array, index } => {
            let array = search(id_table, array);
            let index = search(id_table, index);
            RawExpr::ArrayGet { array, index }
        },
        RawExpr::ArrayPut { array, index, value } => {
            let array = search(id_table, array);
            let index = search(id_table, index);
            let value = search(id_table, value);
            RawExpr::ArrayPut {array, index, value}
        },
        RawExpr::ExtArray { array } => {
            RawExpr::ExtArray { array }
        },
        RawExpr::ExtApply { fun, args } => {
            RawExpr::ExtApply { fun, args: args.into_iter().map(|arg| search(id_table, arg)).collect() }
        },
    };
    Box::new(Spanned::new(raw_expr, expr.span))
}

pub fn do_beta_reduction(expr: Expr) -> Expr {
    let mut id_table = FnvHashMap::default();
    *reduce(expr, &mut id_table)
}