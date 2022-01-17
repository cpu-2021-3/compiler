lalrpop_mod!(pub mincaml);

use crate::syntax::Expr;
use crate::ty::VarType;

pub fn parse(source_code: &str) -> Box<Expr<VarType>> {
    mincaml::ExprParser::new().parse(source_code).unwrap()
}
