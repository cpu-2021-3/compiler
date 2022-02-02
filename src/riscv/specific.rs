use std::fmt;

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

impl fmt::Display for RawInstr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RawInstr::Unit => write!(f, "()"),
            RawInstr::Int(integer) => write!(f, "{}", integer),
            RawInstr::Float(float) => write!(f, "{}", float),
            RawInstr::VarI32(id) => write!(f, "{}", id),
            RawInstr::VarF32(id) => write!(f, "{}.", id),
            RawInstr::DataTag(id) => write!(f, "{}!", id),
            RawInstr::UnOp { op, id } => write!(f, "{} {}", op, id),
            RawInstr::BiOp {
                id_left,
                op,
                id_right,
            } => write!(f, "{} {} {}", id_left, op, id_right),
            RawInstr::If {
                id_left,
                op,
                id_right,
                exp_then,
                exp_else,
            } => {
                write!(f, "if {} {} {} then\n", id_left, op, id_right)?;
                exp_then.item.fmt(f)?;
                write!(f, "\nelse\n")?;
                exp_else.item.fmt(f)
            }
            RawInstr::CallCls { cls, args } => {
                write!(f, "{cls} @")?;
                for arg in args {
                    write!(f, " {arg}")?
                }
                Ok(())
            }
            RawInstr::CallDir { tag, args } => {
                write!(f, "{tag} ")?;
                for arg in args {
                    write!(f, " {arg}")?
                }
                Ok(())
            }
            RawInstr::NewTuple(elms) => {
                write!(f, "(")?;
                for elm in elms {
                    write!(f, "{elm},")?;
                }
                write!(f, ")")
            }
            RawInstr::NewClosure { tag, free_vars } => {
                write!(f, "<")?;
                write!(f, "{tag}!")?;
                for free_var in free_vars {
                    write!(f, ", {free_var}")?;
                }
                write!(f, ">")
            }
            RawInstr::Read { address, offset } => {
                write!(f, "{offset}({address})")
            }
            RawInstr::Write {
                address,
                offset,
                value,
            } => {
                write!(f, "{offset}({address}) <- {value}")
            }
        }
    }
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

impl fmt::Display for RawExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RawExpr::LetIn {
                id,
                instr_id,
                instr_suc,
            } => {
                write!(f, "let {id} = {};\n", instr_id.item)?;
                instr_suc.item.fmt(f)
            }
            RawExpr::Is(instr) => {
                write!(f, "return {}", instr.item)
            }
        }
    }
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

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "let rec {} ", self.tag)?;
        for arg in &self.args {
            write!(f, "{} ", arg)?;
        }
        write!(f, "<")?;
        for free_var in &self.free_vars {
            write!(f, "{}, ", free_var)?;
        }
        write!(f, "> = \n")?;
        self.body.item.fmt(f)
    }
}
