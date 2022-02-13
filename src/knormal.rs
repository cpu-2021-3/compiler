use std::fmt;

use fnv::FnvHashSet;

use std::iter::FromIterator;

use crate::span::Spanned;

/// K 正規化された構文木
#[derive(Debug, Clone)]
pub enum RawExpr {
    Unit,
    Int(i32),
    Float(f32),
    Var(String),
    UnOp {
        op: UnaryOp,
        id: String,
    },
    BiOp {
        id_left: String,
        op: BinaryOp,
        id_right: String,
    },
    If {
        id_left: String,
        op: CondOp,
        id_right: String,
        exp_then: Box<Expr>,
        exp_else: Box<Expr>,
    },
    LetIn {
        id: String,
        exp_id: Box<Expr>,
        exp_suc: Box<Expr>,
    },
    LetRecIn {
        fun: String,
        args: Vec<String>,
        exp_fun: Box<Expr>,
        exp_suc: Box<Expr>,
    },
    Apply {
        fun: String,
        args: Vec<String>,
    },
    NewTuple(Vec<String>),
    TupleGet {
        tuple: String,
        index: usize,
    },
    ArrayGet {
        array: String,
        index: String,
    },
    ArrayPut {
        array: String,
        index: String,
        value: String,
    },
    ExtArray {
        array: String,
    },
    ExtApply {
        fun: String,
        args: Vec<String>,
    },
}

pub type Expr = Spanned<RawExpr>;

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Neg,
    FNeg,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOp::Neg => write!(f, "-"),
            UnaryOp::FNeg => write!(f, "-."),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    FAdd,
    FSub,
    FMul,
    FDiv,
    LShift,
    RShift,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "+"),
            BinaryOp::Sub => write!(f, "-"),
            BinaryOp::Mul => write!(f, "*"),
            BinaryOp::Div => write!(f, "/"),
            BinaryOp::FAdd => write!(f, "+."),
            BinaryOp::FSub => write!(f, "-."),
            BinaryOp::FMul => write!(f, "*."),
            BinaryOp::FDiv => write!(f, "/."),
            BinaryOp::LShift => write!(f, "<<"),
            BinaryOp::RShift => write!(f, ">>"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum CondOp {
    Eq,
    LEq,
}

impl fmt::Display for CondOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CondOp::Eq => write!(f, "="),
            CondOp::LEq => write!(f, "<="),
        }
    }
}


impl Expr {
    // 式の大きさ
    pub fn size(&self) -> u32 {
        match &self.item {
            RawExpr::If { id_left: _, op: _, id_right: _, exp_then: e1, exp_else: e2 } |
            RawExpr::LetIn { id: _, exp_id: e1, exp_suc: e2 } |
            RawExpr::LetRecIn { fun: _, args: _, exp_fun: e1, exp_suc: e2 } => {
                e1.size() + e2.size() + 1
            },
            _ => 1
        }
    }
}

impl RawExpr {
    // 式中に出現する自由変数の集合
    pub fn free_vars(&self) -> FnvHashSet<String> {
        match self {
            RawExpr::Unit |
            RawExpr::Int(_) |
            RawExpr::Float(_) |
            RawExpr::ExtArray { array: _ } => {
                FnvHashSet::default()
            },
            RawExpr::Var(id) |
            RawExpr::UnOp { op: _, id } => {
                FnvHashSet::from_iter(vec![id.clone()])
            },
            RawExpr::BiOp { id_left, op: _, id_right } => {
                FnvHashSet::from_iter(vec![id_left.clone(), id_right.clone()])
            },
            RawExpr::If { id_left, op: _, id_right, exp_then, exp_else } => {
                let mut free_vars = FnvHashSet::from_iter(vec![id_left.clone(), id_right.clone()]);
                free_vars.extend(exp_then.item.free_vars());
                free_vars.extend(exp_else.item.free_vars());
                free_vars
            },
            RawExpr::LetIn { id, exp_id, exp_suc } => {
                let mut free_vars = exp_suc.item.free_vars();
                free_vars.remove(id);
                free_vars.extend(exp_id.item.free_vars());
                free_vars
            },
            RawExpr::LetRecIn { fun, args, exp_fun, exp_suc } => {
                let mut free_vars = exp_fun.item.free_vars();
                args.iter().for_each(|arg| {
                    free_vars.remove(arg);
                });
                free_vars.extend(exp_suc.item.free_vars());
                free_vars.remove(fun);
                free_vars
            },
            RawExpr::Apply { fun, args } => {
                let mut free_vars = FnvHashSet::from_iter(args.clone().into_iter());
                free_vars.insert(fun.clone());
                free_vars
            },
            RawExpr::NewTuple(elms) => {
                FnvHashSet::from_iter(elms.clone().into_iter())
            },
            RawExpr::TupleGet { tuple, index: _ } => {
                FnvHashSet::from_iter(vec![tuple.clone()])
            },
            RawExpr::ArrayGet { array, index } => {
                FnvHashSet::from_iter(vec![array.clone(), index.clone()])
            },
            RawExpr::ArrayPut { array, index, value } => {
                FnvHashSet::from_iter(vec![array.clone(), index.clone(), value.clone()])
            },
            RawExpr::ExtApply { fun: _, args } => {
                FnvHashSet::from_iter(args.clone().into_iter())
            },
        }
    }
}