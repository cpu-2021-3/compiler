use core::fmt;
use std::collections::HashSet;

use crate::{span::Spanned, code};

pub static AREG_NUM: usize = 9;
pub static SREG_NUM: usize = 50;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Register {
    Zero, // ゼロレジスタ
    Ra, // リターンアドレス
    Sp, // スタックポインタ
    Hp, // ヒープポインタ
    X, // 使い捨てレジスタ
    A(usize), // 引数用レジスタ
    S(usize), // 汎用レジスタ
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Register::Zero => write!(f, "zero"),
            Register::Ra => write!(f, "ra"),
            Register::Sp => write!(f, "sp"),
            Register::Hp => write!(f, "hp"),
            Register::X => write!(f, "x0"),
            Register::A(i) => write!(f, "a{i}"),
            Register::S(i) => write!(f, "s{i}"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ROp {
    Add, Sub, Mul, Div, FAdd, FSub, FMul, FDiv, LShift, RShift, FEq, FLEq
}

#[derive(Debug, Clone)]
pub enum IOp {
    Add, LShift, RShift
}

#[derive(Debug, Clone)]
pub enum BOp {
    Eq, LEq
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Tag(String),
    SetImm {
        rd: Register, imm: i32
    },
    Move {
        rd: Register, rs: Register
    },
    ROp {
        rd: Register, op: ROp, rs1: Register, rs2: Register
    },
    IOp {
        rd: Register, op: IOp, rs1: Register, imm: i32
    },
    Branch {
        op: BOp, rs1: Register, rs2: Register, tag: String
    },
    Jump {
        tag: String
    },
    Call(Register, bool),
    CallTag(String, bool),
    LoadTag {
        rd: Register,
        tag: String
    },
    Load {
        rd: Register, 
        imm: i32, 
        rs1: Register
    },
    Store {
        rs2: Register, 
        imm: i32, 
        rs1: Register
    },
    Return
}

pub type Program = Vec<Spanned<Instruction>>;


#[derive(Debug)]
pub struct Function {
    pub tag: String,
    program: Program
}

impl Function {
    pub fn new(tag: String, program: Program) -> Self {
        Self {
            tag, program
        }
    }
    pub fn add_prologue_epilogue(self) -> Self {
        let used = self.used_registers();
        let mut program = vec![];
        let stack_size = used.len() as i32 * 4;
        // prologue を追加
        program.push(Spanned::new(Instruction::IOp{ rd: Register::Sp, op: IOp::Add, rs1: Register::Sp, imm: -stack_size }, (0, 0)));
        for (index, reg) in used.iter().enumerate() {
            program.push(Spanned::new(Instruction::Store{ rs2: *reg, imm: index as i32 * 4, rs1: Register::Sp  }, (0, 0)));
        }
        // epilogue を計算
        let mut epilogue = vec![];
        for (index, reg) in used.iter().enumerate() {
            epilogue.push(Spanned::new(Instruction::Load{ rd: *reg, imm: index as i32 * 4, rs1: Register::Sp  }, (0, 0)));
        }
        epilogue.push(Spanned::new(Instruction::IOp{ rd: Register::Sp, op: IOp::Add, rs1: Register::Sp, imm: stack_size }, (0, 0)));
        // 本体に epilogue を挿入
        for Spanned {item, span} in self.program {
            match &item {
                Instruction::Call(_, true) | Instruction::CallTag(_, true) | Instruction::Return => {
                    program.extend(epilogue.clone());
                },
                _ => {}
            }
            program.push(Spanned::new(item, span));
        }
        Self::new(self.tag, program)
    }
    pub fn used_registers(&self) -> HashSet<Register> {
        let mut used = HashSet::new();
        for line in &self.program {
            match &line.item {
                Instruction::SetImm { rd, imm: _ } |
                Instruction::Move { rd, rs: _ } |
                Instruction::ROp { rd, op: _, rs1: _, rs2: _ } |
                Instruction::LoadTag { rd, tag: _ } |
                Instruction::Load { rd, imm: _, rs1: _ } => {
                    if let Register::S(_) = rd {
                        used.insert(*rd);
                    }
                },
                Instruction::Call(_, false) |
                Instruction::CallTag(_, false) => {
                    used.insert(Register::Ra);
                }
                _ => {}
            }
        }
        used
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:\n", self.tag)?;
        for Spanned{item, span} in &self.program {
            match item {
                Instruction::Tag(tag) => write!(f, "{tag}:")?,
                Instruction::SetImm { rd, imm } => write!(f, "\tli {rd}, {imm}")?,
                Instruction::Move { rd, rs } => write!(f, "\tmv {rd}, {rs}")?,
                Instruction::ROp { rd, op, rs1, rs2 } => {
                    let op = match op {
                        ROp::Add => "add",
                        ROp::Sub => "sub",
                        ROp::Mul => "mul",
                        ROp::Div => "div",
                        ROp::FAdd => "fadd",
                        ROp::FSub => "fsub",
                        ROp::FMul => "fmul",
                        ROp::FDiv => "fdiv",
                        ROp::LShift => "sll",
                        ROp::RShift => "sra",
                        ROp::FEq => "feq",
                        ROp::FLEq => "fle",
                    };
                    write!(f, "\t{op} {rd}, {rs1}, {rs2}")?
                },
                Instruction::IOp { rd, op, rs1, imm } => {
                    let op = match op {
                        IOp::Add => "addi",
                        IOp::LShift => "slli",
                        IOp::RShift => "srai",
                    };
                    write!(f, "\t{op} {rd}, {rs1}, {imm}")?
                },
                Instruction::Branch { op, rs1, rs2, tag } => {
                    let op = match op {
                        BOp::Eq => "beq",
                        BOp::LEq => "ble",
                    };
                    write!(f, "\t{op} {rs1}, {rs2}, {tag}")?
                },
                Instruction::Jump { tag } => {
                    write!(f, "\tj {tag}")?
                },
                Instruction::Call( reg, is_tail) => {
                    if *is_tail {
                        write!(f, "\tjr {reg}")?
                    }
                    else {
                        write!(f, "\tjalr {reg}")?
                    }
                },
                Instruction::CallTag(tag, is_tail) => {
                    if *is_tail {
                        write!(f, "\tj {tag}")?
                    }
                    else {
                        write!(f, "\tjal {tag}")?
                    }
                },
                Instruction::LoadTag { rd, tag } => {
                    write!(f, "\tla {rd}, {tag}")?
                },
                Instruction::Load { rd, imm, rs1 } => {
                    write!(f, "\tlw {rd}, {imm}({rs1})")?
                },
                Instruction::Store { rs2, imm, rs1 } => {
                    write!(f, "\tsw {rs2}, {imm}({rs1})")?
                },
                Instruction::Return => {
                    write!(f, "\tret")?;
                },
            }
            if span.1 != 0 {
                write!(f, "\t# {} - {}", code::line_column(span.0), code::line_column(span.1))?;
            }
            write!(f, "\n")?;
        }
        Ok(())
    }
}