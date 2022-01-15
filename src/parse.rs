lalrpop_mod!(pub mincaml);

use crate::syntax::{RawExpr, Spanned};
use crate::ty::Type;

pub fn parse(source_code: &str) -> Box<Spanned<RawExpr<Option<Type>>>> {
    mincaml::ExprParser::new().parse(source_code).unwrap()
}
