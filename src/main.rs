use std::env;
use std::fs::File;
use std::io::prelude::*;

use rust_9cc::code::Code;
use rust_9cc::codegen::gen;
use rust_9cc::objs::{variable_block_check, Obj};
use rust_9cc::token::Tokens;
use rust_9cc::tree_gen::program;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("error");
    }

    let code_str = &args[1];
    let code = Code::new(code_str);
    let mut tokens_iter = Tokens::parse(&code).into_iter();
    let objs = Obj::from_tokens(&mut tokens_iter);
    tokens_iter.index_reset();
    let nodes = program(&mut tokens_iter);
    variable_block_check(&nodes, &objs);
    let mut block_num = 0;
    let assemry = gen(&nodes, &objs, &mut block_num);

    let mut file = File::create("res.S").unwrap();
    file.write_all(assemry.as_bytes()).unwrap();
}
