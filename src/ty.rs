#[derive(Debug)]
pub enum Type {
    Unit,
    Bool,
    Int,
    Float,
    Fun(Vec<Type>, Box<Type>),
    Tuple(Vec<Type>),
    Array(Vec<Type>)
}