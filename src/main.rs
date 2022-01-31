use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::num::ParseIntError;

#[derive(Debug)]
enum CompilerError {
    IoERROR(std::io::Error),
    ArgError(String),
    ParseError(ParseIntError),
}

impl From<std::io::Error> for CompilerError {
    fn from(error: std::io::Error) -> CompilerError {
        CompilerError::IoERROR(error)
    }
}

impl From<std::num::ParseIntError> for CompilerError {
    fn from(error: std::num::ParseIntError) -> CompilerError {
        CompilerError::ParseError(error)
    }
}

fn assembry_string(number: usize) -> String {
    let mut assembry = String::new();

    assembry.push_str(".intel_syntax noprefix\n");
    assembry.push_str(".global main\n");
    assembry.push_str("main:\n");
    assembry.push_str(&format!("\tmov rax, {value}\n", value=number));
    assembry.push_str("\tret\n");
    assembry
}

fn main() -> Result<(), CompilerError> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        return Err(CompilerError::ArgError("number of input args is not collect".to_string())) 
    }

    let number: usize = args[1].parse()?;

    let buf = assembry_string(number);
    println!("{:?}", buf);

    let mut file = File::create("add.S")?;
    file.write_all(buf.as_bytes())?;

    Ok(())
}
