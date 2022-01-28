//#![warn(clippy::pedantic, clippy::nursery)]

#[macro_use]
extern crate lalrpop_util;
extern crate global_counter;
pub mod closure;
pub mod closurize;
pub mod id;
pub mod knormal;
pub mod knormalize;
pub mod parse;
pub mod span;
pub mod syntax;
pub mod ty;
pub mod typing;

use std::{env, fs};

fn main() {
    let args: Vec<String> = env::args().collect();

    let filename = &args[1];
    let source_code = fs::read_to_string(filename).expect("error reading file");

    let expr = parse::parse(&source_code);
    let (syntax_expr, extenv) = match typing::do_typing(*expr) {
        Ok(elm) => elm,
        Err(err) => {
            println!("{}", err.message(&source_code));
            panic!("{}", err);
        }
    };

    let (k_normalized, k_env) = knormalize::k_normalize(syntax_expr, &extenv);

    let (closurized, toplevels) = closurize::closurize(k_normalized, &k_env);
    println!("{:#?}", closurized);
    println!("{:#?}", toplevels);
}
