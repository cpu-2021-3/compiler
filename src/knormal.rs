use crate::span::{Spanned};

#[derive(Debug)]
pub enum RawExpr {
    Unit,
    Int(i32),
    Float(f32),
    Var(String),
    UnOp {
        op: UnaryOp, 
        id: String
    },
    BiOp {
        id_left: String, op: BinaryOp, id_right: String
    },
    If {
        id_left: String, op: CondOp, id_right: String, exp_then: Box<Expr>, exp_else: Box<Expr>
    },
    LetIn {
        id: String, exp_id: Box<Expr>, exp_suc: Box<Expr>
    },
    LetRecIn {
        fun: String,
        args: Vec<String>,
        exp_fun: Box<RawExpr>,
        exp_suc: Box<RawExpr>,
    },
    Apply {
        fun: String,
        args: Vec<String>,
    },
    NewTuple(Vec<String>),
    TupleGet {
        tuple: String,
        index: String,
    },
    ArrayGet {
        array: String,
        index: String,
    },
    ArrayPut {
        array: String,
        index: String,
        value: String,
    }
}

pub type Expr = Spanned<RawExpr>;



#[derive(Debug)]
pub enum UnaryOp {
    Not,
    Neg,
    FNeg,
}

#[derive(Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    FAdd,
    FSub,
    FMul,
    FDiv,
}

#[derive(Debug)]
pub enum CondOp {
    Eq,
    LEq,
}