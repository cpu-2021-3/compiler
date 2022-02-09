pub enum Register {
    Zero,
    Ra,
    Sp,
    Hp,
    A(usize),
    S(usize),
}

pub enum ROp {
    Add, Sub, Mul, Div, FAdd, FSub, FMul, FDiv, LShift, RShift
}

pub enum IOp {
    Add, LShift, RShift
}

pub enum BOp {
    NEq, Lt
}

pub enum Instruction {
    Tag(String),
    SetImm(Register, i32),
    ROp(Register, ROp, Register, Register),
    IOp(Register, IOp, Register, Register),
    Branch(BOp, Register, Register),
    Call(String),
    LoadTag(String),
    Load(Register, i32, Register),
    Store(Register, i32, Register),
}