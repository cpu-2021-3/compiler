lalrpop_mod!(pub mincaml);

use crate::code::line_column;
use crate::syntax::Expr;
use crate::ty::VarType;
use lalrpop_util::ParseError::*;

pub fn parse(source_code: &str) -> Box<Expr<VarType>> {
    match mincaml::ExprParser::new().parse(source_code) {
        Ok(result) => result,
        Err(parse_error) => {
            let lc = |position| line_column(position);
            match parse_error {
                InvalidToken { location } => {
                    panic!("syntax error: invalid token in {}", lc(location));
                }
                UnrecognizedEOF { location, expected } => {
                    panic!(
                        "syntax error: unexpected EOF in {}, expected these tokens: {:?}",
                        lc(location),
                        expected
                    );
                }
                UnrecognizedToken { token, expected } => {
                    panic!(
                        "syntax error: unexpected token {} in {} ~ {}, expected these tokens: {:?}",
                        token.1,
                        lc(token.0),
                        lc(token.2),
                        expected
                    );
                }
                ExtraToken { token } => {
                    panic!(
                        "syntax error: unexpected token {} in {} ~ {}, expected nothing",
                        token.1,
                        lc(token.0),
                        lc(token.2)
                    );
                }
                User { error } => {
                    panic!("{}", error);
                }
            }
        }
    }
}
