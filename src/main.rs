//#![warn(clippy::pedantic, clippy::nursery)]

#[macro_use]
extern crate lalrpop_util;
extern crate global_counter;
pub mod closure;
pub mod closurize;
pub mod code;
pub mod compile;
pub mod id;
pub mod inline;
pub mod knormal;
pub mod knormalize;
pub mod parse;
pub mod riscv;
pub mod span;
pub mod syntax;
pub mod ty;
pub mod typing;
pub mod constfold;
pub mod eliminate;
pub mod reduce;

use std::env;

fn main() {
    simple_logger::init().unwrap();

    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        log::error!("No source code path was supplied");
        panic!("command line argument error");
    }

    let is_globals = args.len() >= 3 && args[2] == "-g";

    let is_main = args.len() >= 3 && args[2] == "-m";

    let filename = &args[1];

    compile::compile(filename, is_globals, is_main);
}
