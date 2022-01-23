#[macro_use]
extern crate lalrpop_util;
extern crate global_counter;
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

    //let source_code = "let rec f x = if x >= 0 then (f (x-1)) else () in print_float (-1.0)".to_string();

    let expr = parse::parse(&source_code);
    let typed = match typing::do_typing(*expr) {
        Ok(elm) => elm,
        Err(err) => {
            println!("{}", err.message(&source_code));
            panic!("{}", err);
        }
    };
    //println!("{:?}", typed.0);
    //println!("{:?}", typed.1);
    let k_normalized = knormalize::k_normalize(typed.0, &typed.1);
    println!("{:#?}", k_normalized);
}
