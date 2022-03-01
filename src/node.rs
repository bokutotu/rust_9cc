use crate::token::{Token, TokensIter};

#[derive(Debug, PartialEq)]
pub struct Node<'a> {
    kind: Token<'a>,
    lhs: Option<Box<Node<'a>>>,
    rhs: Option<Box<Node<'a>>>,
}

impl<'a> Node<'a> {
    pub fn new(kind_: Token<'a>, lhs: Option<Node<'a>>, rhs: Option<Node<'a>>) -> Self {
        if let (Some(lhs), Some(rhs)) = (lhs, rhs) {
            Node { 
                kind: kind_,
                lhs: Some(Box::new(lhs)),
                rhs: Some(Box::new(rhs)),
            }
        } else {
            Node {
                kind: kind_,
                lhs: None,
                rhs: None,
            }
        }
    }

    pub fn num_expect(&self) -> Option<u64> {
        if let Token::INT(x) = self.kind {
            Some(x)
        } else {
            None
        }
    }

    pub fn lhs(&self) -> &Box<Node<'a>> {
        if let Some(x) = &self.lhs {
            return x
        } else {
            panic!("error");
        }
    }

    pub fn rhs(&self) -> &Box<Node<'a>> {
        if let Some(x) = &self.rhs {
            return x
        } else {
            panic!("error");
        }
    }

    pub fn kind(&self) -> &Token<'a> {
        &self.kind
    } 
}

pub fn expr<'a>(tokens_iter: &mut TokensIter<'a>) -> Node<'a> {
    let mut node = mul(tokens_iter);
    loop {
        match tokens_iter.next() {
            Some(Token::PLUS) => {
                node = Node::new(Token::PLUS, Some(node), Some(mul(tokens_iter)));
            },
            Some(Token::MINUS) => {
                node = Node::new(Token::MINUS, Some(node), Some(mul(tokens_iter)));
            },
            _ => {
                tokens_iter.back();
                break
            },
        };
    }
    node
}

fn mul<'a>(tokens_iter: &mut TokensIter<'a>) -> Node<'a> {
    let mut node = unary(tokens_iter);
    loop {
        match tokens_iter.next() {
            Some(Token::ASTARISK) => {
                node = Node::new(Token::ASTARISK, Some(node), Some(unary(tokens_iter)));
            },
            Some(Token::SLASH) => {
                node = Node::new(Token::SLASH, Some(node), Some(unary(tokens_iter)));
            },
            _ => {
                tokens_iter.back();
                break
            },
        };
    }
    node
}

fn unary<'a>(tokens_iter: &mut TokensIter<'a>) -> Node<'a> {
    let token = tokens_iter.next().expect("error");
    if Token::MINUS == token {
        let zero = Token::INT(0);
        let zero_node = Node::new(zero, None, None);
        return Node::new(Token::MINUS, Some(zero_node), Some(primary(tokens_iter)));
    } else {
        tokens_iter.back();
        return primary(tokens_iter);
    }
}

fn primary<'a>(tokens_iter: &mut TokensIter<'a>) -> Node<'a> {
    let token = tokens_iter.next().expect("syntax error!");
    if let Token::INT(_) = token {
        return Node::new(token, None, None);
    };
    match token {
        Token::LPARENTHESIS => {
            let tmp = expr(tokens_iter);
            match tokens_iter.next() {
                Some(Token::RPARENTHESIS) => {
                    return tmp
                },
                _ => panic!("syatax error"),
            };
        },
        _ => panic!("syatax error"),
    };
}

#[cfg(test)]
mod expr_test {
    use crate::code::Code;
    use crate::token::{Token, Tokens};
    use super::{expr, Node};
    
    #[test]
    fn test_1() {
        let code_str = "1";
        let code = Code::new(code_str);
        let mut tokens_iter = Tokens::parse(&code).into_iter();
        let expr_ = expr(&mut tokens_iter);
        let ans = Node::new(Token::INT(1),None,None,);
        assert_eq!(ans, expr_);
    }

    #[test]
    fn test_2() {
        let code_str = "1 + 2";
        let code = Code::new(code_str);
        let mut tokens_iter = Tokens::parse(&code).into_iter();
        let expr_ = expr(&mut tokens_iter);
        let one = Node::new(Token::INT(1),None,None);
        let two = Node::new(Token::INT(2), None, None);
        let ans = Node::new(Token::PLUS, Some(one), Some(two),);
        assert_eq!(ans, expr_);
    }

    #[test]
    fn test_3() {
        let code_str = "1 * 2";
        let code = Code::new(code_str);
        let mut tokens_iter = Tokens::parse(&code).into_iter();
        let expr_ = expr(&mut tokens_iter);
        let one = Node::new(Token::INT(1), None, None);
        let two = Node::new(Token::INT(2), None, None);
        let ans = Node::new(Token::ASTARISK, Some(one), Some(two),);
        assert_eq!(ans, expr_);
    }

    #[test]
    fn test_4() {
        let code_str = "1 * 2 - (3 + 4)";
        let code = Code::new(code_str);
        let mut token_iter = Tokens::parse(&code).into_iter();
        let expr_ = expr(&mut token_iter);
        let one = Node::new(Token::INT(1), None, None);
        let two = Node::new(Token::INT(2), None, None);
        let three = Node::new(Token::INT(3), None, None);
        let four = Node::new(Token::INT(4), None, None);
        let one_ast_two = Node::new(Token::ASTARISK, Some(one), Some(two));
        let three_plus_four = Node::new(
            Token::PLUS, Some(three), Some(four));
        let ans = Node::new(Token::MINUS, Some(one_ast_two), Some(three_plus_four));
        assert_eq!(ans, expr_);
    }

    #[test]
    fn tesst_miuns() {
        let code_str = "-3";
        let code = Code::new(code_str);
        let mut tokens_iter = Tokens::parse(&code).into_iter();
        let expr = expr(&mut tokens_iter);
        let zero = Node::new(Token::INT(0), None, None);
        let three = Node::new(Token::INT(3), None, None);
        let ans = Node::new(Token::MINUS, Some(zero), Some(three));
        assert_eq!(ans, expr);
    }
}
