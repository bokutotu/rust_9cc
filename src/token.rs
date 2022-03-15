use std::cell::{RefCell, Cell};

use crate::code::{Code, strtol, pass_space, ident as code_ident};

#[derive(PartialEq, Debug, Clone)]
pub enum Token {
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
    // String -> valable name, usize -> offset from rbp
    VARIABLE(String),
}

impl Token {
    pub fn variable(&self) -> Option<String> {
        let onwed = self.clone();
        if let Token::VARIABLE(name) = onwed {
            return Some(name)
        }
        None
    }
}

fn int(code: &Code) -> Option<Token> {
    if let Some(x) = strtol(code) {
        return Some(Token::INT(x));
    } else {
        return None
    }
}

macro_rules! impl_single_operator {
    ($($operator:expr,$token:expr),*) => {
        fn single_operator(code: &Code) -> Option<Token> {
            pass_space(code);
            match code.now() {
                $(
                    Some($operator) => {
                        code.inc_idx();
                        return Some($token)
                    },
                 )*
                _ => return None
            }
        }
    }
}
impl_single_operator!(
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

macro_rules! impl_double_operator {
    ($($operator1:expr, $operator2:expr, $token:expr),*) => {
        fn double_operator(code: &Code) -> Option<Token> {
            pass_space(code);
            let now_2_char = code.now_2_char();
            $(
                if let Some(($operator1, $operator2)) = now_2_char {
                    code.inc_idx();
                    code.inc_idx();
                    return Some($token)
                }
            )*
            None
        }
    }
}
impl_double_operator!(
    '=', '=', Token::EQEQ, 
    '!', '=', Token::EXCLAMATIONEQ,
    '>', '=', Token::GREATEREQ,
    '<', '=', Token::LESSEQ
);

fn ident(code: &Code) -> Option<Token> {
    match code_ident(code) {
        Some(x) => Some(Token::VARIABLE(x)),
        None => None,
    }
}

impl Token {
    fn new(code: &Code) -> Option<Token> {
        let int = int(code);
        if int.is_some() {
            return int
        }
        let double = double_operator(code);
        if double.is_some() {
            return double
        };
        let single = single_operator(code);
        if single.is_some() {
            return single
        }
        let ident = ident(code);
        if ident.is_some() {
            return ident
        }
        None
    }
}

impl<'a> Eq for Token {}

#[derive(PartialEq, Debug)]
pub struct Tokens {
    value: RefCell<Vec<Token>>,
    len: Cell<usize>
}

#[derive(Debug)]
pub struct TokensIter {
    value: Tokens,
    index: usize,
    length: usize,
}

impl Eq for Tokens {}

impl Tokens {
    /// new Tokens
    fn new() -> Self {
        Tokens {
            value: RefCell::new(Vec::new()),
            len: Cell::new(0),
        }
    }

    fn push(&self, token: Token) {
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

    pub fn into_iter(self) -> TokensIter {
        let len = *&self.len.get();
        TokensIter { value: self, index: 0, length: len }
    }
}

impl TokensIter {
    pub fn index_reset(&mut self) {
        self.index = 0;
    }

    pub fn back(&mut self) {
        self.index = self.index - 1;
    }

    pub fn is_end(&self) -> bool {
        if self.index >= self.length { return true } 
        else { return false }
    }
}

impl Iterator for TokensIter {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        if self.index == self.length {
            return None
        }
        self.index = self.index + 1;
        let borrow = self.value.value.borrow();
        let item = borrow[self.index - 1].clone();
        Some(item)
    }
}

#[test]
fn code_2_tokens() {
    let code_str = "(10+3-2)*3;";
    let code = Code::new(code_str);
    let tokens = Tokens::parse(&code);
    let ans = Tokens {
        value: RefCell::new(vec![Token::LPARENTHESIS, Token::INT(10), Token::PLUS, Token::INT(3),
                            Token::MINUS, Token::INT(2), Token::RPARENTHESIS, 
                            Token::ASTARISK, Token::INT(3), Token::COLON]),
        len: Cell::new(10),
    };
    assert_eq!(ans, tokens);
}

#[test]
fn test_minus() {
    let code_str = "-1;";
    let code = Code::new(code_str);
    let tokens = Tokens::parse(&code);
    let ans = Tokens {
        value: RefCell::new(
            vec![Token::MINUS, Token::INT(1), Token::COLON]
        ),
        len: Cell::new(3),
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

#[test]
fn test_ident() {
    let code_str = "value = 10";
    let code = Code::new(code_str);
    let tokens = Tokens::parse(&code);
    let ans = Tokens {
        value: RefCell::new(vec![
            Token::VARIABLE("value".to_string()), Token::EQ, Token::INT(10),
        ]),
        len: Cell::new(3),
    };
    assert_eq!(ans, tokens);
}
