use std::{rc::Rc, cell::RefCell};

#[derive(Debug, Clone, PartialEq)]
pub enum VarType {
    Unit,
    Bool,
    Int,
    Float,
    Fun(Vec<VarType>, Box<VarType>),
    Tuple(Vec<VarType>),
    Array(Box<VarType>),
    Var(Rc<RefCell<Option<VarType>>>)
}

impl VarType {
    pub fn wrap_var(self) -> Self {
        Self::Var(Rc::new(RefCell::new(Some(self))))
    }
    pub fn new() -> Self {
        Self::Var(Rc::new(RefCell::new(None)))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Unit,
    Bool,
    Int,
    Float,
    Fun(Vec<Type>, Box<Type>),
    Tuple(Vec<Type>),
    Array(Box<Type>),
}

