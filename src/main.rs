use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::num::ParseIntError;

use thiserror::Error;

#[derive(Debug, Error)]
enum CompilerError {
    #[error("Number of Arguments(`{0}`) is not collect")]
    ArgError(usize),
    #[error("IoERROR(`{0}`")]
    IoERROR(std::io::Error),
    #[error("ParseIntError(`{0}`")]
    ParseError(ParseIntError),
    #[error("`{0}` is not compilerable string")]
    NotACompilerbleCharError(CharError),
    #[error("Syntax Error")]
    SyntaxError
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

impl From<CharError> for CompilerError {
    fn from(error: CharError) -> CompilerError {
        CompilerError::NotACompilerbleCharError(error)
    }
}

#[derive(Debug)]
enum Parse {
    Int(usize),
    Plus,
    Minus,
}

#[derive(Debug)]
pub struct CharError {
    char: char
}

impl std::fmt::Display for CharError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} is not compilerable", &self.char)
    }
}

impl std::error::Error for CharError {}

fn _char_to_num_single(s: char) -> Option<usize> {
    let num = s as i32 - 48;
    match num {
        0..=9 => {
            return Some(num as usize)
        }
        _ => {
            return None
        }
    }
}

fn str_to_num(code: &str, idx: &mut usize) -> Option<usize> {
    let mut number: Option<usize> = None;
    loop {
        let now_char = match get_nth_char(code, idx) {
            Some(x) => x,
            None => break
        };
        number = match _char_to_num_single(now_char) {
            Some(x) => {
                match number {
                    Some(y) => Some(y * 10 + x),
                    None => Some(x)
                }
            },
            None => return number,
        };
        *idx = *idx + 1;
    }
    number
}

fn get_nth_char(code: &str, idx: &usize) -> Option<char> {
    code.chars().nth(*idx)
}

fn parse_code(code: &str) -> Result<Vec<Parse>, CompilerError> {
    let mut parsed = Vec::new();
    let mut idx: usize = 0;
    match str_to_num(code, &mut idx) {
        Some(x) => parsed.push(Parse::Int(x)),
        None => return Err(CompilerError::SyntaxError),
    };
    loop {
        match get_nth_char(code, &idx) {
            Some('-') => {
                parsed.push(Parse::Minus);
                idx = idx + 1;
                let num = str_to_num(code, &mut idx);
                match num {
                    Some(x) => parsed.push(Parse::Int(x)),
                    None => return Err(CompilerError::SyntaxError)
                }
            },
            Some('+') => {
                parsed.push(Parse::Plus);
                idx = idx + 1;
                let num = str_to_num(code, &mut idx);
                match num {
                    Some(x) => parsed.push(Parse::Int(x)),
                    None => return Err(CompilerError::SyntaxError)
                }
            },
            None => break,
            _ => return Err(CompilerError::SyntaxError)
        }
    }
    
    Ok(parsed)
}

fn parser_int_to_num(x: &Parse) -> Option<usize> {
    match x {
        Parse::Int(x) => Some(*x),
        _ => None
    }
}

fn assembry_string(code: &str) -> Result<String, CompilerError> {
    let code = parse_code(code)?;
    println!("{:?}", code);
    let mut assembry = String::new();

    assembry.push_str(".intel_syntax noprefix\n");
    assembry.push_str(".global main\n");
    assembry.push_str("main:\n");
    assembry.push_str(&format!("\tmov rax, {value}\n", value=parser_int_to_num(&code[0]).unwrap()));
    let mut idx = 1;
    loop {
        if idx == code.len() {
            break
        }
        match &code[idx] {
            Parse::Plus => {
                assembry.push_str(&format!("\tadd rax, {value}\n", value=parser_int_to_num(&code[idx+1]).unwrap()));
            }
            Parse::Minus => {
                assembry.push_str(&format!("\tadd rax, {value}\n", value=parser_int_to_num(&code[idx+1]).unwrap()));
            }
            _ => unreachable!()
        };
        idx = idx + 2;
        
    }
    assembry.push_str("\tret\n");
    Ok(assembry)
}

fn main() -> Result<(), CompilerError> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        return Err(CompilerError::ArgError(args.len())) 
    }

    let code = &args[1];

    let buf = assembry_string(&code)?;

    let mut file = File::create("add.S")?;
    file.write_all(buf.as_bytes())?;

    Ok(())
}
