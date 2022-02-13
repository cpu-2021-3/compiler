use std::{fs::{self, File}, io::Write};

use crate::{closurize, code, knormalize, parse, riscv, typing, inline};

pub fn compile(filename: &String) {
    code::SOURCE_CODE
        .set(fs::read_to_string(filename).unwrap())
        .unwrap();

    code::build_index_table().expect("fail to build table");

    let expr = parse::parse(code::SOURCE_CODE.get().unwrap());
    let (syntax_expr, extenv) = match typing::do_typing(*expr) {
        Ok(elm) => elm,
        Err(err) => {
            log::error!("{}", err.message());
            panic!("type error");
        }
    };

    let (k_normalized, mut k_env) = knormalize::k_normalize(syntax_expr, &extenv);

    let k_normalized = inline::do_inline_expansion(k_normalized, &mut k_env);

    let (closurized, toplevels) = closurize::closurize(k_normalized, &k_env);
    closurize::typecheck(&closurized, &k_env, &toplevels);

    let functions = riscv::specify::specify(closurized, toplevels, &mut k_env);
    let functions = riscv::embed::embed(functions);

    let functions = riscv::regalloc::do_register_allocation(functions);

    let functions: Vec<_> = functions.into_iter().map(|function| function.coalesce()).collect();
    
    let mut f = File::create("output_before_elim.s").expect("Failed to open output file");

    for function in &functions {
        f.write_all(format!("{}", function).as_bytes()).expect("Failed to write into output file");
    }
    let functions: Vec<_> = functions.into_iter().map(|function| function.remove_dead_code()).collect();

    let mut f = File::create("output_after_elim.s").expect("Failed to open output file");

    for function in &functions {
        f.write_all(format!("{}", function).as_bytes()).expect("Failed to write into output file");
    }

    let functions: Vec<_> = functions.into_iter().map(|function|
    if function.tag == "min_caml_start" {
        function 
    }
    else {
        function.add_prologue_epilogue()
    }).collect();

    let mut f = File::create("output.s").expect("Failed to open output file");

    for function in &functions {
        f.write_all(format!("{}", function).as_bytes()).expect("Failed to write into output file");
    }
}
