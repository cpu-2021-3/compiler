#[macro_use] extern crate lalrpop_util;
#[macro_use] extern crate global_counter;
lalrpop_mod!(pub mincaml);
pub mod syntax;
pub mod ty;
pub mod id;

fn main() {
    println!("Hello, world!");
    let expr = mincaml::ExprParser::new()
        .parse("let (p,q) = f 3 in p + q")
        .unwrap();
    println!("{:#?}", &expr);
}

