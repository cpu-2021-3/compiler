//#![warn(clippy::pedantic, clippy::nursery)]

#[macro_use]
extern crate lalrpop_util;
extern crate global_counter;
pub mod code;
pub mod compile;
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

use std::env;

fn main() {
    simple_logger::init().unwrap();

    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        log::error!("No source code path was supplied");
        panic!("command line argument error");
    }

    let filename = &args[1];

    compile::compile(filename);
}
