use anyhow::Result;
use std::{
    cell::RefCell,
    fmt::{self},
    rc::Rc,
};

#[derive(Debug, Clone, PartialEq)]
pub enum VarType {
    Unit,
    Bool,
    Int,
    Float,
    Fun(Vec<VarType>, Box<VarType>),
    Tuple(Vec<VarType>),
    Array(Box<VarType>),
    Var(Rc<RefCell<Option<VarType>>>),
}

impl VarType {
    pub fn wrap_var(self) -> Self {
        Self::Var(Rc::new(RefCell::new(Some(self))))
    }
    pub fn new() -> Self {
        Self::Var(Rc::new(RefCell::new(None)))
    }
    pub fn to_type(self) -> Result<Type> {
        let result = match self {
            VarType::Unit => Type::Unit,
            VarType::Bool => Type::Bool,
            VarType::Int => Type::Int,
            VarType::Float => Type::Float,
            VarType::Fun(args, ret) => Type::Fun(
                args.into_iter()
                    .map(|arg| arg.to_type())
                    .collect::<Result<_, _>>()?,
                Box::new(ret.to_type()?),
            ),
            VarType::Tuple(elms) => Type::Tuple(
                elms.into_iter()
                    .map(|elm| elm.to_type())
                    .collect::<Result<_, _>>()?,
            ),
            VarType::Array(elm) => Type::Array(Box::new(elm.to_type()?)),
            VarType::Var(_) => {
                return Err(anyhow::anyhow!("undecided type detected"));
            }
        };
        Ok(result)
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

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Unit => write!(f, "unit"),
            Type::Bool => write!(f, "bool"),
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::Fun(args, res) => {
                write!(f, "(")?;
                for arg in args {
                    arg.fmt(f)?;
                }
                write!(f, ") -> ")?;
                res.fmt(f)
            }
            Type::Tuple(elms) => {
                for (index, elm) in elms.iter().enumerate() {
                    write!(f, "{elm}")?;
                    if index + 1 != elms.len() {
                        write!(f, " * ")?;
                    }
                }
                Ok(())
            }
            Type::Array(elm) => {
                write!(f, "{elm} list")
            }
        }
    }
}
