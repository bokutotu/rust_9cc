use std::cell::{RefCell, Cell};

use crate::code::{Code, strtol, pass_space};

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Token<'a> {
    // +
    PLUS,
    // +=
    PLUSEQ,
    // ++
    PLUSPLUS,
    // -
    MINUS,
    // -=
    MINUSEQUAL,
    // --
    MINUSMINUS,
    // ->
    RIGHT,
    // *
    ASTARISK,
    // *=
    ASTARISKEQ,
    // /
    SLASH,
    // /=
    SLASHEQ,
    // //
    SLASHSLASH,
    // %
    PERCENT,
    // %=
    PERCENTEQ,
    // &
    AND,
    // &=
    ANDEQ,
    // &&
    ANDAND,
    // |
    PIPE,
    // |=
    PIPEQ,
    // ||
    PIPEPIPE,
    // ^
    HAT,
    // ^=
    HATEQ,
    // ~
    TILDE,
    // <
    LESS,
    // <=
    LESSEQ,
    // <<
    LLESS,
    // <<=
    LLESSEQ,
    // >
    GREATER,
    // >=
    GREATEREQ,
    // >>
    GGREATER,
    // >>=
    GGREATERRQ,
    // !
    EXCLAMATION,
    // !=
    EXCLAMATIONEQ,
    // =
    EQ,
    // ==
    EQEQ,
    // ,
    CONMA,
    // .
    DOT,
    // ...
    DOT3,
    // :
    COLON,
    // ;
    SEMICOLON,
    // (
    LPARENTHESIS,
    // )
    RPARENTHESIS,
    // [
    LSBRACKET,
    // ]
    RSBRACKET,
    // {
    LCBRACKET,
    // }
    RCBRACKET,

    // auto
    AUTO,
    // break
    BREAK,
    // case
    CASE,
    // char
    CHAR,
    // const
    CONST,
    // continue
    CONTINUE,
    // default
    DEFAULT,
    // do
    DO,
    // double
    DOUBLE(f64),
    // else
    ELSE,
    // enum
    ENUM,
    // extern
    EXTERN,
    // float
    FLOAT(f32),
    // for
    FOR,
    // goto
    GOTO,
    // if
    IF,
    // int
    INT(u64),
    // long
    LONG,
    // register
    REGISATER,
    // return
    RETURN,
    // signed
    SIGNED,
    // sizeof
    SIZEOF,
    // short
    SHORT,
    //swich
    SWITCH,
    // typedef
    TYPEDEF,
    // union
    UNION,
    // unsigned
    UNSIGNED,
    // void
    VOID,
    // volatile
    VOLATILE,
    // while
    WHILE,
    // name of identifier
    IDENTIFIER(&'a str, usize),
}

fn int(code: &Code) -> Option<Token<'_>> {
    if let Some(x) = strtol(code) {
        return Some(Token::INT(x));
    } else {
        return None
    }
}

macro_rules! impl_single_ident {
    ($($ident:expr,$token:expr),*) => {
        fn single_ident(code: &Code) -> Option<Token<'_>> {
            pass_space(code);
            match code.now() {
                $(
                    Some($ident) => {
                        code.inc_idx();
                        return Some($token)
                    },
                 )*
                _ => return None
            }
        }
    }
}
impl_single_ident!(
    '+', Token::PLUS,
    '-', Token::MINUS,
    '*', Token::ASTARISK,
    '=', Token::EQ,
    '>', Token::GREATER,
    '<', Token::LESS,
    '/', Token::SLASH,
    ';', Token::COLON,
    '(', Token::LPARENTHESIS,
    ')', Token::RPARENTHESIS,
    '!', Token::EXCLAMATION
);

macro_rules! impl_double_ident {
    ($($ident1:expr, $ident2:expr, $token:expr),*) => {
        fn double_ident(code: &Code) -> Option<Token<'_>> {
            pass_space(code);
            let now_2_char = code.now_2_char();
            $(
                if let Some(($ident1, $ident2)) = now_2_char {
                    code.inc_idx();
                    code.inc_idx();
                    return Some($token)
                }
            )*
            None
        }
    }
}
impl_double_ident!(
    '=', '=', Token::EQEQ, 
    '!', '=', Token::EXCLAMATIONEQ,
    '>', '=', Token::GREATEREQ,
    '<', '=', Token::LESSEQ
);

impl Token<'_> {
    fn new(code: &Code) -> Option<Token<'_>> {
        let int = int(code);
        if int.is_some() {
            return int
        }
        let double = double_ident(code);
        if double.is_some() {
            return double
        };
        let single = single_ident(code);
        if single.is_some() {
            return single
        }
        None
    }
}

impl<'a> Eq for Token<'a> {}

#[derive(PartialEq, Debug)]
pub struct Tokens<'a> {
    value: RefCell<Vec<Token<'a>>>,
    len: Cell<usize>
}

#[derive(Debug)]
pub struct TokensIter<'a> {
    value: Tokens<'a>,
    index: usize,
    length: usize,
}

impl<'a> Eq for Tokens<'a> {}

impl<'a> Tokens<'a> {
    /// new Tokens
    fn new() -> Self {
        Tokens {
            value: RefCell::new(Vec::new()),
            len: Cell::new(0),
        }
    }

    fn push(&self, token: Token<'a>) {
        self.value.borrow_mut().push(token);
        let _ = self.len.set(self.len.get() + 1);
    }

    pub fn parse(code: &Code) -> Tokens {
        let tokens = Tokens::new();
        loop {
            if let Some(x) = Token::new(code) {
                tokens.push(x);
            }
            if code.is_end() { break }
        }
        tokens
    }

    pub fn into_iter(self) -> TokensIter<'a> {
        let len = *&self.len.get();
        TokensIter { value: self, index: 0, length: len }
    }
}

impl<'a> TokensIter<'a> {
    pub fn back(&mut self) {
        self.index = self.index - 1;
    }
}

impl<'a> Iterator for TokensIter<'a> {
    type Item = Token<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.index == self.length {
            return None
        }
        self.index = self.index + 1;
        let borrow = self.value.value.borrow();
        Some(borrow[self.index - 1])
    }
}

#[test]
fn code_2_tokens() {
    let code_str = "(10+3-2)*3";
    let code = Code::new(code_str);
    let tokens = Tokens::parse(&code);
    let ans = Tokens {
        value: RefCell::new(vec![Token::LPARENTHESIS, Token::INT(10), Token::PLUS, Token::INT(3),
                            Token::MINUS, Token::INT(2), Token::RPARENTHESIS, 
                            Token::ASTARISK, Token::INT(3)]),
        len: Cell::new(9),
    };
    assert_eq!(ans, tokens);
}

#[test]
fn test_minus() {
    let code_str = "-1";
    let code = Code::new(code_str);
    let tokens = Tokens::parse(&code);
    let ans = Tokens {
        value: RefCell::new(
            vec![Token::MINUS, Token::INT(1)]
        ),
        len: Cell::new(2),
    };
    assert_eq!(ans, tokens);
}

#[test]
fn test_eqeq() {
    let code_str = "10 ==   10";
    let code = Code::new(code_str);
    let token = Tokens::parse(&code);
    let ans = Tokens {
        value: RefCell::new(vec![Token::INT(10), Token::EQEQ, Token::INT(10)]),
        len: Cell::new(3),
    };
    assert_eq!(ans, token);
}

#[test]
fn test_noteq() {
    let code = "10 != 10";
    let code = Code::new(code);
    let tokens = Tokens::parse(&code);
    let ans = Tokens {
        value: RefCell::new(vec![Token::INT(10), Token::EXCLAMATIONEQ, Token::INT(10)]),
        len: Cell::new(3),
    };
    assert_eq!(ans, tokens);
}

