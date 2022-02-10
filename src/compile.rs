use std::fs;

use crate::{closurize, code, knormalize, parse, riscv, typing};

pub fn compile(filename: &String) {
    code::SOURCE_CODE
        .set(fs::read_to_string(filename).unwrap())
        .unwrap();

    let expr = parse::parse(code::SOURCE_CODE.get().unwrap());
    let (syntax_expr, extenv) = match typing::do_typing(*expr) {
        Ok(elm) => elm,
        Err(err) => {
            log::error!("{}", err.message());
            panic!("type error");
        }
    };

    let (k_normalized, mut k_env) = knormalize::k_normalize(syntax_expr, &extenv);

    let (closurized, toplevels) = closurize::closurize(k_normalized, &k_env);
    closurize::typecheck(&closurized, &k_env, &toplevels);

    let functions = riscv::specify::specify(closurized, toplevels, &mut k_env);
    let functions = riscv::embed::embed(functions);

    // for function in functions {
    //     println!("{function}");
    // }

    let functions = riscv::regalloc::do_register_allocation(functions);

    for function in functions {
        print!("{}", function);
    }
}
