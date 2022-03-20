use std::collections::HashMap;

use crate::token::{Token, TokensIter};

#[derive(Clone, Debug, PartialEq)]
pub struct Obj(HashMap<String, usize>);

impl Obj {
    fn new() -> Self {
        Obj(HashMap::new())
    }

    fn insert(&mut self, key: String) {
        match self.0.get(&key) {
            Some(_) => (),
            None => {
                let id = self.0.len();
                self.0.insert(key, id);
            }
        }
    }

    pub fn variable_offset(&self, variable: String) -> usize {
        self.0[&variable]
    }

    pub fn from_tokens(tokens_iter: &mut TokensIter) -> Self {
        let mut res = Self::new();
        for token in tokens_iter {
            if let Token::VARIABLE(variable) = token {
                res.insert(variable);
            }
        }
        res
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

#[test]
fn test_tokens_iter_to_obj() {
    use crate::code::Code;
    use crate::token::Tokens;
    let code_str = "oppai = 10; boyoyon = 10 + 33; oppai; boron;";
    let code = Code::new(code_str);
    let tokens = Tokens::parse(&code);
    let mut tokens_iter = tokens.into_iter();
    let obj = Obj::from_tokens(&mut tokens_iter);
    let ans = Obj(HashMap::from([
        ("oppai".to_string(), 0),
        ("boyoyon".to_string(), 1),
        ("boron".to_string(), 2),
    ]));
    assert_eq!(ans, obj)
}
