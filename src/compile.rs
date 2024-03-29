use std::{fs::{self, File}, io::Write};

use fnv::FnvHashMap;

use crate::{closurize, code, knormalize, parse, riscv, typing, inline, constfold, eliminate, knormal, ty::Type, reduce};

static OPTIMIZATION_LIMIT: u32 = 1000;

// K 正規化されたコードを、コードの長さが変化しなくなるまで最適化しつづける
fn optimization_loop(mut k_normalized: knormal::Expr, mut k_env: FnvHashMap<String, Type>) -> (knormal::Expr, FnvHashMap<String, Type>) {
    let mut counter = 0;
    for _ in 0..OPTIMIZATION_LIMIT {
        let len_0 = k_normalized.size();
        k_normalized = reduce::do_beta_reduction(k_normalized);
        let len_1 = k_normalized.size();
        k_normalized = inline::do_inline_expansion(k_normalized, &mut k_env);
        let len_2 = k_normalized.size();
        k_normalized = constfold::do_constant_folding(k_normalized);
        let len_3 = k_normalized.size();
        k_normalized = eliminate::eliminate_dead_code(k_normalized);
        let len_4 = k_normalized.size();
        counter += 1;
        log::debug!("Optimization looped {counter} times (code size: {len_0}, {len_1}, {len_2}, {len_3}, {len_4})");
        if len_0 == len_1 && len_1 == len_2 && len_2 == len_3 && len_3 == len_4 {
            log::debug!("Optimization finished!");
            return (k_normalized, k_env)
        }
    }
    log::debug!("Optimization stopped.");
    return (k_normalized, k_env)
}

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

    let (k_normalized, k_env) = knormalize::k_normalize(syntax_expr, &extenv);

    let (k_normalized, mut k_env) = optimization_loop(k_normalized, k_env);

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
    let functions: Vec<_> = functions.into_iter().map(|function| function.do_constant_folding()).collect();

    let mut f = File::create("output_after_elim.s").expect("Failed to open output file");

    for function in &functions {
        f.write_all(format!("{}", function).as_bytes()).expect("Failed to write into output file");
    }

    let functions: Vec<_> = functions.into_iter().map(|function| function.remove_dead_code()).collect();
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
