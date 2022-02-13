use std::{fmt};

use fnv::FnvHashMap;

use crate::{knormal::{Expr, RawExpr, UnaryOp, BinaryOp, CondOp}, span::Spanned};

#[derive(Clone)]
enum Const {
    Int(i32),
    Float(f32),
    Tuple(Vec<Const>)
}

impl fmt::Display for Const {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Const::Int(i) => write!(f, "{i}"),
            Const::Float(v) => write!(f, "{v}"),
            Const::Tuple(elms) => {
                write!(f, "(")?;
                for elm in elms {
                    write!(f, "{elm}, ")?;
                }
                write!(f, ")")
            },
        }
    }
}


impl Const {
    pub fn expr(&self) -> Option<RawExpr> {
        match self {
            Const::Int(i) => Some(RawExpr::Int(*i)),
            Const::Float(f) => Some(RawExpr::Float(*f)),
            Const::Tuple(e) => None,
        }
    }
}

fn const_of(expr: &Expr, const_table: &FnvHashMap<String, Const>) -> Option<Const> {
    match &expr.item {
        RawExpr::Int(i) => Some(Const::Int(*i)),
        RawExpr::Float(f) => Some(Const::Float(*f)),
        RawExpr::NewTuple(elms) => {
            let maybe_consts: Vec<Option<&Const>> = elms.iter().map(|elm| const_table.get(elm)).collect();
            if maybe_consts.iter().all(|x| {
                if let Some(_) = x {true} else {false}
            }) {
                Some(Const::Tuple(maybe_consts.into_iter().map(|elm| elm.unwrap().clone()).collect()))
            }
            else {
                None
            }
        },
        _ => None
    }
}
// expr の定数埋め込みを行う
fn fold(expr: Expr, const_table: &mut FnvHashMap<String, Const>) -> Box<Expr> {
    let raw_expr = match expr.item {
        RawExpr::Var(id) => {
            loop {
                if let Some(constant) = const_table.get(&id) {
                    if let Some(constant) = constant.expr() {
                        // int や float の場合は、変数を定数に置き換える
                        break constant;
                    }
                }
                break RawExpr::Var(id);
            }
        },
        RawExpr::UnOp { op, id } => {
            loop {
                match op {
                    UnaryOp::Neg => {
                        if let Some(Const::Int(i)) = const_table.get(&id) {
                            log::info!("Folded -{i} to {}", -i);
                            break RawExpr::Int(-*i);
                        }
                    },
                    UnaryOp::FNeg => {
                        if let Some(Const::Float(f)) = const_table.get(&id) {
                            log::info!("Folded -.{f} to {}", -f);
                            break RawExpr::Float(-*f);
                        }
                    },
                }
                break RawExpr::UnOp {op, id}
            }
        },
        RawExpr::BiOp { id_left, op, id_right } => {
            loop {
                match op {
                    BinaryOp::Add => {
                        if let (Some(Const::Int(i_left)), Some(Const::Int(i_right))) = (const_table.get(&id_left), const_table.get(&id_right)) {
                            log::info!("Folded {i_left} + {i_right} to {}", i_left + i_right);
                            break RawExpr::Int(i_left + i_right);
                        }
                    },
                    BinaryOp::Sub => {
                        if let (Some(Const::Int(i_left)), Some(Const::Int(i_right))) = (const_table.get(&id_left), const_table.get(&id_right)) {
                            log::info!("Folded {i_left} - {i_right} to {}", i_left - i_right);
                            break RawExpr::Int(i_left - i_right);
                        }
                    },
                    BinaryOp::Mul => {
                        if let (Some(Const::Int(i_left)), Some(Const::Int(i_right))) = (const_table.get(&id_left), const_table.get(&id_right)) {
                            log::info!("Folded {i_left} * {i_right} to {}", i_left * i_right);
                            break RawExpr::Int(i_left * i_right);
                        }
                    },
                    BinaryOp::Div => {
                        if let (Some(Const::Int(i_left)), Some(Const::Int(i_right))) = (const_table.get(&id_left), const_table.get(&id_right)) {
                            log::info!("Folded {i_left} / {i_right} to {}", i_left / i_right);
                            break RawExpr::Int(i_left / i_right);
                        }
                    },
                    BinaryOp::FAdd => {
                        if let (Some(Const::Float(f_left)), Some(Const::Float(f_right))) = (const_table.get(&id_left), const_table.get(&id_right)) {
                            log::info!("Folded {f_left} +. {f_right} to {}", f_left + f_right);
                            break RawExpr::Float(f_left + f_right);
                        }
                    },
                    BinaryOp::FSub => {
                        if let (Some(Const::Float(f_left)), Some(Const::Float(f_right))) = (const_table.get(&id_left), const_table.get(&id_right)) {
                            log::info!("Folded {f_left} -. {f_right} to {}", f_left - f_right);
                            break RawExpr::Float(f_left - f_right);
                        }
                    },
                    BinaryOp::FMul => {
                        if let (Some(Const::Float(f_left)), Some(Const::Float(f_right))) = (const_table.get(&id_left), const_table.get(&id_right)) {
                            log::info!("Folded {f_left} *. {f_right} to {}", f_left * f_right);
                            break RawExpr::Float(f_left * f_right);
                        }
                    },
                    BinaryOp::FDiv => {
                        if let (Some(Const::Float(f_left)), Some(Const::Float(f_right))) = (const_table.get(&id_left), const_table.get(&id_right)) {
                            log::info!("Folded {f_left} /. {f_right} to {}", f_left / f_right);
                            break RawExpr::Float(f_left / f_right);
                        }
                    },
                    BinaryOp::LShift => {
                        if let (Some(Const::Int(i_left)), Some(Const::Int(i_right))) = (const_table.get(&id_left), const_table.get(&id_right)) {
                            log::info!("Folded {i_left} << {i_right} to {}", i_left << i_right);
                            break RawExpr::Int(i_left << i_right);
                        }
                    },
                    BinaryOp::RShift => {
                        if let (Some(Const::Int(i_left)), Some(Const::Int(i_right))) = (const_table.get(&id_left), const_table.get(&id_right)) {
                            log::info!("Folded {i_left} >> {i_right} to {}", i_left >> i_right);
                            break RawExpr::Int(i_left >> i_right);
                        }
                    },
                };
                break RawExpr::BiOp {id_left, op, id_right}
            }
        },
        RawExpr::If { id_left, op, id_right, exp_then, exp_else } => {
            let exp_then = fold(*exp_then, const_table);
            let exp_else = fold(*exp_else, const_table);
            loop {
                match op {
                    CondOp::Eq => {
                        if let (Some(Const::Int(i_left)), Some(Const::Int(i_right))) = (const_table.get(&id_left), const_table.get(&id_right)) {
                            log::info!("Folded {i_left} == {i_right} to {}", i_left == i_right);
                            if *i_left == *i_right {
                                break exp_then.item;
                            }
                            else {
                                break exp_else.item;
                            }
                        }
                        if let (Some(Const::Float(f_left)), Some(Const::Float(f_right))) = (const_table.get(&id_left), const_table.get(&id_right)) {
                            log::info!("Folded {f_left} == {f_right} to {}", f_left == f_right);
                            if *f_left == *f_right {
                                break exp_then.item;
                            }
                            else {
                                break exp_else.item;
                            }
                        }
                    },
                    CondOp::LEq => {
                        if let (Some(Const::Int(i_left)), Some(Const::Int(i_right))) = (const_table.get(&id_left), const_table.get(&id_right)) {
                            log::info!("Folded {i_left} <= {i_right} to {}", i_left <= i_right);
                            if *i_left <= *i_right {
                                break exp_then.item;
                            }
                            else {
                                break exp_else.item;
                            }
                        }
                        if let (Some(Const::Float(f_left)), Some(Const::Float(f_right))) = (const_table.get(&id_left), const_table.get(&id_right)) {
                            log::info!("Folded {f_left} <= {f_right} to {}", f_left <= f_right);
                            if *f_left <= *f_right {
                                break exp_then.item;
                            }
                            else {
                                break exp_else.item;
                            }
                        }
                    },
                }
                break RawExpr::If{id_left, op, id_right, exp_then, exp_else}; 
            }
        },
        RawExpr::LetIn { id, exp_id, exp_suc } => {
            let exp_id = fold(*exp_id, const_table);
            match const_of(&exp_id, const_table) {
                Some(constant) => {
                    log::info!("Added {id}: {constant} to constant table");
                    const_table.insert(id.clone(), constant);
                },
                None => {},
            }
            let exp_suc = fold(*exp_suc, const_table);
            RawExpr::LetIn{id, exp_id, exp_suc}
        },
        RawExpr::LetRecIn { fun, args, exp_fun, exp_suc } => {
            let exp_fun = fold(*exp_fun, const_table);
            let exp_suc = fold(*exp_suc, const_table);
            RawExpr::LetRecIn {
                fun, args, exp_fun, exp_suc
            }
        },
        otherwise => otherwise
    };
    Box::new(Spanned::new(raw_expr, expr.span))
}

pub fn do_constant_folding(expr: Expr) -> Expr {
    let mut const_table = FnvHashMap::default();
    *fold(expr, &mut const_table)
}