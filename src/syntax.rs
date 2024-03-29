use crate::span::Spanned;

/// min-caml コードの抽象構文木
/// Expr<Option<Type>> とすることで型推論をしていない木を、Expr<Type> とすることで型が決定した木を表現できる
#[derive(Debug)]
pub enum RawExpr<T> {
    Unit,
    Bool(bool),
    Int(i32),
    Float(f32),
    Var(String),

    UnOp {
        op: UnaryOp,
        exp: Box<Expr<T>>,
    },
    BiOp {
        exp_left: Box<Expr<T>>,
        op: BinaryOp,
        exp_right: Box<Expr<T>>,
    },

    Apply {
        fun: Box<Expr<T>>,
        args: Vec<Expr<T>>,
    },

    If {
        cond: Box<Expr<T>>,
        exp_then: Box<Expr<T>>,
        exp_else: Box<Expr<T>>,
    },

    LetIn {
        var: TypedVar<T>,
        exp_var: Box<Expr<T>>,
        exp_suc: Box<Expr<T>>,
    },
    LetRecIn {
        fun: TypedVar<T>,
        args: Vec<TypedVar<T>>,
        exp_fun: Box<Expr<T>>,
        exp_suc: Box<Expr<T>>,
    },
    LetTupleIn {
        vars: Vec<TypedVar<T>>,
        exp_var: Box<Expr<T>>,
        exp_suc: Box<Expr<T>>,
    },

    NewTuple(Vec<Expr<T>>),
    NewArray {
        size: Box<Expr<T>>,
        value: Box<Expr<T>>,
    },
    ArrayGet {
        array: Box<Expr<T>>,
        index: Box<Expr<T>>,
    },
    ArrayPut {
        array: Box<Expr<T>>,
        index: Box<Expr<T>>,
        value: Box<Expr<T>>,
    },
}

pub type Expr<T> = Spanned<RawExpr<T>>;
pub type TypedVar<T> = Spanned<RawTypedVar<T>>;

#[derive(Debug)]
pub struct RawTypedVar<T> {
    pub name: String,
    pub t: T,
}

impl<T> RawTypedVar<T> {
    pub fn new(name: String, t: T) -> Self {
        Self { name, t }
    }
}

#[derive(Debug, PartialEq)]
pub enum UnaryOp {
    Not,
    Neg,
    FNeg,
}

#[derive(Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    FAdd,
    FSub,
    FMul,
    FDiv,
    Eq,
    LEq,
}
