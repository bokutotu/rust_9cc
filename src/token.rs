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
    ($fn_name:ident, $ident:expr, $token:expr) => {
        fn $fn_name(code: &Code) -> Option<Token<'_>> {
            pass_space(code);
            match code.now() {
                Some($ident) => { 
                    code.inc_idx();
                    return Some($token) 
                },
                _ => return None,
            };
        }
    }
}
impl_single_ident!(plus, '+', Token::PLUS);
impl_single_ident!(minus, '-', Token::MINUS);
impl_single_ident!(astarisk, '*', Token::ASTARISK);
impl_single_ident!(slash, '/', Token::SLASH);
impl_single_ident!(lparenthsis, '(', Token::LPARENTHESIS);
impl_single_ident!(rparenthsis, ')', Token::RPARENTHESIS);
impl_single_ident!(colon, ';', Token::COLON);

macro_rules! token_new_impl {
    ($($fn_name:expr ),*) => {
        impl Token<'_> {
            fn new(code: &Code) -> Option<Token<'_>> {
                $(
                    match $fn_name(code) {
                        Some(x) => return Some(x),
                        _ => (),
                    };
                 )*
                None
            }
        }
    }
}
token_new_impl!(plus, minus, astarisk, slash, lparenthsis, rparenthsis, colon, int);

impl<'a> Eq for Token<'a> {}

#[derive(PartialEq, Debug)]
pub struct Tokens<'a> {
    value: RefCell<Vec<Token<'a>>>,
    len: Cell<usize>
}

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

