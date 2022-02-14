use crate::token::Tokens;
use crate::error::CompilerError;

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

fn str_to_num(code: &Vec<char>, idx: &mut usize) -> Option<usize> {
    let mut number: Option<usize> = None;
    loop {
        if *idx == code.len() {
            break
        } 
        number = match _char_to_num_single(code[*idx]) {
            Some(x) => {
                match number {
                    Some(y) => Some(10 * y + x),
                    None => Some(x)
                }
            },
            None => return number
        };
        *idx = *idx + 1;
    };
    number
}

fn pass_space(code: &Vec<char>, idx: &mut usize) {
    loop {
        if code[*idx] == ' ' {
            *idx = *idx + 1;
        } else {
            break
        }
    };
}

pub struct Code {
    content: Vec<char>,
    length: usize
}

impl Code {
    pub fn new(code: String) -> Code {
        let code: Vec<char> = code.chars().collect();
        let length = &code.len();
        Code {
            content: code,
            length: *length,
        }
    }

    fn inner(&self) -> &Vec<char> {
        &self.content
    }

    fn parse_num(&self, tokens: &mut Tokens, idx: &mut usize) -> Result<(), CompilerError> {
        let inner = self.inner();
        pass_space(inner, idx);
        match str_to_num(&inner, idx) {
            Some(x) => {
                tokens.push_num(x)
            },
            None => {
                return Err(CompilerError::SyntaxError)
            }
        };
        Ok(())
    } 

    fn parse_reserved(&self, tokens: &mut Tokens, idx: &mut usize) -> Result<(), CompilerError> {
        let inner = self.inner();
        pass_space(inner, idx);
        if inner[*idx] == '+' {
            tokens.push_plus();
        } else if inner[*idx] == '-' {
            tokens.push_miuns();
        }
        *idx = *idx + 1;
        Ok(())
    }

    pub fn parse(&self) -> Result<Tokens, CompilerError> {
        let mut tokens = Tokens::new(); 
        let mut idx = 0; 
        self.parse_num(&mut tokens, &mut idx)?;
        loop {
            if idx == self.length { 
                tokens.push_eof();
                break 
            }
            self.parse_reserved(&mut tokens, &mut idx)?;
            self.parse_num(&mut tokens, &mut idx)?;
        }
        Ok(tokens)
    }

}

