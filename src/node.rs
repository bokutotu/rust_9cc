use crate::token::{Token, TokensIter};

#[derive(Debug, PartialEq)]
pub struct Node<'a> {
    kind: Token<'a>,
    lhs: Box<Option<Node<'a>>>,
    rhs: Box<Option<Node<'a>>>,
}

impl<'a> Node<'a> {
    pub fn new(kind_: Token<'a>, lhs: Option<Node<'a>>, rhs: Option<Node<'a>>) -> Self {
        Node {
            kind: kind_,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    pub fn num_expect(&self) -> Option<u64> {
        if let Token::INT(x) = self.kind {
            Some(x)
        } else {
            None
        }
    }

    pub fn lhs(&self) -> &Option<Self> {
        &*self.lhs
    }

    pub fn rhs(&self) -> &Option<Self> {
        &*self.rhs
    }
}

fn expr<'a>(tokens_iter: &mut TokensIter<'a>) -> Node<'a> {
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
    let mut node = primary(tokens_iter);
    loop {
        match tokens_iter.next() {
            Some(Token::ASTARISK) => {
                node = Node::new(Token::ASTARISK, Some(node), Some(primary(tokens_iter)));
            },
            Some(Token::SLASH) => {
                node = Node::new(Token::SLASH, Some(node), Some(primary(tokens_iter)));
            },
            _ => {
                tokens_iter.back();
                break
            },
        };
    }
    node
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
        let ans = Node {
            kind: Token::INT(1),
            lhs: Box::new(None),
            rhs: Box::new(None)
        };
        assert_eq!(ans, expr_);
    }

    #[test]
    fn test_2() {
        let code_str = "1 + 2";
        let code = Code::new(code_str);
        let mut tokens_iter = Tokens::parse(&code).into_iter();
        let expr_ = expr(&mut tokens_iter);
        let one = Node {
            kind: Token::INT(1),
            lhs: Box::new(None),
            rhs: Box::new(None)
        };
        let two = Node {
            kind: Token::INT(2),
            lhs: Box::new(None),
            rhs: Box::new(None)
        };
        let ans = Node {
            kind: Token::PLUS,
            lhs: Box::new(Some(one)),
            rhs: Box::new(Some(two)),
        };
        assert_eq!(ans, expr_);
    }

    #[test]
    fn test_3() {
        let code_str = "1 * 2";
        let code = Code::new(code_str);
        let mut tokens_iter = Tokens::parse(&code).into_iter();
        let expr_ = expr(&mut tokens_iter);
        let one = Node {
            kind: Token::INT(1),
            lhs: Box::new(None),
            rhs: Box::new(None)
        };
        let two = Node {
            kind: Token::INT(2),
            lhs: Box::new(None),
            rhs: Box::new(None)
        };
        let ans = Node {
            kind: Token::ASTARISK,
            lhs: Box::new(Some(one)),
            rhs: Box::new(Some(two)),
        };
        assert_eq!(ans, expr_);
    }

    #[test]
    fn test_4() {
        let code_str = "1 * 2 + (3 + 4)";
        let code = Code::new(code_str);
        let mut token_iter = Tokens::parse(&code).into_iter();
        let expr_ = expr(&mut token_iter);
        let one_ast_two = Node {
            kind: Token::ASTARISK,
            lhs: Box::new(
                Some(Node { kind: Token::INT(1) , lhs: Box::new(None), rhs: Box::new(None)})
            ),
            rhs: Box::new(
                Some(Node { kind: Token::INT(2) , lhs: Box::new(None), rhs: Box::new(None)})
            )
        };
        let three_plus_four = Node { 
            kind: Token::PLUS, 
            lhs: Box::new(
                Some(Node { kind: Token::INT(3) , lhs: Box::new(None), rhs: Box::new(None)})
            ),
            rhs: Box::new(
                Some(Node { kind: Token::INT(4) , lhs: Box::new(None), rhs: Box::new(None)})
            )
        };
        let ans = Node {
            kind: Token::PLUS, 
            lhs: Box::new(Some(one_ast_two)), 
            rhs: Box::new(Some(three_plus_four)) 
        };
        assert_eq!(ans, expr_);
    }
}
