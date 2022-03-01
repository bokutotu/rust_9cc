use std::env;
use std::fs::File;
use std::io::prelude::*;

use rust_9cc::code::Code;
use rust_9cc::node::expr;
use rust_9cc::token::Tokens;
use rust_9cc::codegen::gen;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("error");
    }

    let code_str = &args[1];

    let code = Code::new(&code_str);
    let tokens = Tokens::parse(&code);
    let mut tokens_iter = tokens.into_iter();
    let nodes = expr(&mut tokens_iter);
    let assemry = gen(&nodes);

    let mut file = File::create("res.S").unwrap();
    file.write_all(assemry.as_bytes()).unwrap();
}
