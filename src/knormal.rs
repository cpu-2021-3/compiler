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
    },
    ExtArray {
        array: String,
    },
    ExtApply {
        fun: String,
        args: Vec<String>,
    }
}

pub type Expr = Spanned<RawExpr>;

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Neg,
    FNeg,
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
}

#[derive(Debug, Clone, PartialEq)]
pub enum CondOp {
    Eq,
    LEq,
}
