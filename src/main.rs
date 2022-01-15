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
    let ast = parse::parse(
        "let rec fib n =
if n <= 1 then n else
fib (n - 1) + fib (n - 2) in
print_int (fib 30)",
    );
    println!("{:#?}", &ast);
}
