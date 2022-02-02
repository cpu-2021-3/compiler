use std::fmt;

use crate::span::Spanned;

/// K 正規化された構文木
#[derive(Debug)]
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
