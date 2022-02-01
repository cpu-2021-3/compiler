use crate::knormal::{BinaryOp, CondOp, UnaryOp};
use crate::span::Spanned;

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
    ApplyCls {
        cls: String,
        args: Vec<String>,
    },
    ApplyDir {
        tag: String,
        args: Vec<String>,
    },
    NewTuple(Vec<String>),
    NewClosure {
        tag: String,
        free_vars: Vec<String>,
    },
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
}

#[derive(Debug)]
pub struct Function {
    pub tag: String,
    pub args: Vec<String>,
    pub free_vars: Vec<String>,
    pub body: Expr,
}

pub type Expr = Spanned<RawExpr>;
