use crate::{
    knormal::{BinaryOp, CondOp, UnaryOp},
    span::Spanned,
};

/// 仮想命令の構文木 (RISC-V 版)

/// 最小単位の命令 (If 文のみ分岐あり)
#[derive(Debug)]
pub enum RawInstr {
    Unit,
    Int(i32),
    Float(f32),
    VarI32(String),
    VarF32(String),
    DataTag(String),
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
    CallCls {
        cls: String,
        args: Vec<String>,
    },
    CallDir {
        tag: String,
        args: Vec<String>,
    },
    NewTuple(Vec<String>),
    NewClosure {
        tag: String,
        free_vars: Vec<String>,
    },
    Read {
        address: String,
        offset: i32,
    },
    Write {
        address: String,
        offset: i32,
        value: String,
    },
}

/// 構文木
#[derive(Debug)]
pub enum RawExpr {
    LetIn {
        id: String,
        instr_id: Instr,
        instr_suc: Box<Expr>,
    },
    Is(Instr),
}

pub type Instr = Spanned<RawInstr>;
pub type Expr = Spanned<RawExpr>;

#[derive(Debug)]
pub struct Function {
    pub tag: String,
    pub args: Vec<String>,
    pub free_vars: Vec<String>,
    pub body: Expr,
}
