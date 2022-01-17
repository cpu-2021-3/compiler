#[macro_use]
extern crate lalrpop_util;
#[macro_use]
extern crate global_counter;
pub mod id;
pub mod parse;
pub mod syntax;
pub mod ty;
pub mod typing;

fn main() {
    println!("Hello, world!");
    let expr = parse::parse(
        "a; let b = c in ()",
    );
    let typed = typing::do_typing(*expr).unwrap();
    println!("{:#?}", typed.0);
    println!("{:?}", typed.1);
}
