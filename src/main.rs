use std::env;
use std::fs::File;
use std::io::prelude::*;

use rust_9cc::error::CompilerError;
use rust_9cc::parse::Code;

fn main() -> Result<(), CompilerError> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        return Err(CompilerError::ArgError(args.len())) 
    }

    let code = &args[1];

    let code_parser = Code::new(code.to_string());
    let parse = code_parser.parse()?;
    let assembry = parse.assembry();

    let mut file = File::create("add.S")?;
    file.write_all(assembry.as_bytes())?;
    Ok(())
}
