//! コードのトークンのハンドリングに関するファイル

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // +
    Plus,
    // -
    Minus,
    // 数字
    Num(usize),
    // 入力の終わりを表す
    EOF,
}

macro_rules! impl_is_arm {
    ($method_name: ident, $method_arm: expr) => {
            pub fn $method_name(&self) -> bool {
                if *self == $method_arm {
                true
            } else {
                false
            }
        }
    }
}

impl Token {
    fn num(number: usize) -> Token {
        Token::Num(number)
    }
    
    fn plus() -> Token {
        Token::Plus
    }

    fn minus() -> Token {
        Token::Minus
    }

    fn eof() -> Token {
        Token::EOF
    }
    
    pub fn get_num(self) -> Option<usize> {
        match self {
            Token::Num(x) => Some(x),
            _ => None
        }
    }

    pub fn is_num(self) -> bool {
        match self {
            Token::Num(_) => true,
            _ => false
        }
    }

    impl_is_arm!(is_plus, Token::Plus);
    impl_is_arm!(is_mius, Token::Minus);
    impl_is_arm!(is_eof, Token::EOF);
}

#[derive(Debug)]
pub struct Tokens {
    tokens: Vec<Token>,
    length: usize
}

impl Tokens {
    pub fn new() -> Tokens {
        Tokens { 
            tokens: Vec::new(),
            length: 0
        }
    }

    fn push(&mut self, content: Token) {
        self.tokens.push(content);
        self.length += 1;
    }

    pub fn push_num(&mut self, num: usize) {
        self.push(Token::num(num));
    }

    pub fn push_plus(&mut self) {
        self.push(Token::plus());
    }

    pub fn push_miuns(&mut self) {
        self.push(Token::minus()) 
    }

    pub fn push_eof(&mut self) {
        self.push(Token::eof())
    }

    pub fn get_elem_by_id(&self, idx: usize) -> Option<Token> {
        if idx >= self.length {
            return None
        }
        Some(self.tokens[idx].clone())
    }

    pub fn iter(&self) -> TokensIter {
        TokensIter {
            tokens: self,
            now: 0
        }
    }
}

pub struct TokensIter<'a> {
    tokens: &'a Tokens,
    now: usize,
}

impl<'a> Iterator for TokensIter<'a> {
    type Item=Token;
    fn next(&mut self) -> Option<Self::Item> {
        self.now += 1;
        self.tokens.get_elem_by_id(self.now - 1)
    }
}
