use std::rc::Rc;
use std::cell::{RefCell, Cell};

use crate::token::{Token, TokensIter};
use crate::objs::Obj;

#[derive(Debug, PartialEq)]
pub struct Node {
    kind: Token,
    lhs: Option<Box<Node>>,
    rhs: Option<Box<Node>>,
}

impl Node {
    pub fn new(kind_: Token, lhs: Option<Node>, rhs: Option<Node>) -> Self {
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
        if let Token::INT(x) = self.kind() {
            Some(*x)
        } else {
            None
        }
    }

    pub fn variable_offset_expect(&self, objs: &Obj) -> Option<usize> {
        if let Token::VARIABLE(_) = self.kind() {
            return Some(
                objs.variable_offset(
                    self
                        .kind()
                        .clone()
                        .variable()
                        .expect("error")
                )
            )
        }
        None
    }

    pub fn lhs(&self) -> &Box<Node> {
        if let Some(x) = &self.lhs {
            return x
        } else {
            panic!("error");
        }
    }

    pub fn rhs(&self) -> &Box<Node> {
        if let Some(x) = &self.rhs {
            return x
        } else {
            panic!("error");
        }
    }

    pub fn kind(&self) -> &Token {
        &self.kind
    } 
}

#[derive(Clone, Debug, PartialEq)]
pub struct Nodes {
    value: RefCell<Vec<Rc<Node>>>,
    len: Cell<usize>,
}

impl Nodes {
    pub fn new() -> Nodes {
        let value_ = RefCell::new(Vec::new());
        let len_ = Cell::new(0);
        Nodes { value: value_, len: len_ }
    }

    pub fn push(&self, item: Node) {
        self.value.borrow_mut().push(Rc::new(item));
        self.len.set(self.len.get() + 1);
    }

    pub fn into_iter(&self) -> NodeIter {
        let len_ = &self.len.get();
        NodeIter {
            value: self.clone(),
            len: *len_,
            idx: 0
        }
    }

    fn get_nth(&self, idx: usize) -> Rc<Node> {
        self.value.borrow()[idx].clone()
    }
}

pub struct NodeIter {
    value: Nodes,
    len: usize,
    idx: usize,
}

impl Iterator for NodeIter {
    type Item = Rc<Node>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.idx == self.len {
            return None
        }
        self.idx = self.idx + 1;
        Some(self.value.get_nth(self.idx - 1))
    }
}

// program = stmt*
pub fn program(tokens_iter: &mut TokensIter) -> Nodes {
    let nodes = Nodes::new();
    loop {
        if tokens_iter.is_end() { break }
        let stmt_ = stmt(tokens_iter);
        nodes.push(stmt_);
    }
    nodes
}

// stmt = expr ";"
fn stmt(tokens_iter: &mut TokensIter) -> Node {
    let expr = expr(tokens_iter);
    if let Some(Token::COLON) = tokens_iter.next() {
        return expr
    }
    panic!("syntax error");
}

// expr = assign
fn expr(tokens_iter: &mut TokensIter) -> Node {
    assign(tokens_iter)
}

// assign = equality("=" assign)?
fn assign(tokens_iter: &mut TokensIter) -> Node {
    let equality_ = equality(tokens_iter);
    if let Some(Token::EQ) = tokens_iter.next() {
        return Node::new(Token::EQ, Some(equality_), Some(assign(tokens_iter)))
    }
    tokens_iter.back();
    equality_
}

macro_rules! impl_node_function {
    ($fn_name:ident, $child_fn:ident, $($token:path),*) => {
        pub fn $fn_name<'a>(tokens_iter: &mut TokensIter) -> Node {
            let mut node = $child_fn(tokens_iter);
            loop {
                match tokens_iter.next() {
                    $(
                        Some($token) => {
                            node = Node::new($token, Some(node), Some($child_fn(tokens_iter)));
                        },
                    )*
                    _ => {
                        tokens_iter.back();
                        break;
                    },
                }
            }
            node
        }
    }
}
// equality = relational ('==' relational | '!=' relational) *
impl_node_function!(equality, relational, Token::EQEQ, Token::EXCLAMATIONEQ);
// relational = add('<' add | '<=' add | '>' add | '>=' add) *
impl_node_function!(
    relational, add, Token::GREATER, Token::GREATEREQ, Token::LESS, Token::LESSEQ);
// mul = mul('+' mul | '-' mul)
impl_node_function!(add, mul, Token::PLUS, Token::MINUS);
// mul = unary ('*' unary | '/' unary)*
impl_node_function!(mul, unary, Token::ASTARISK, Token::SLASH);

// unary = ('+' | '-')? primary
fn unary<'a>(tokens_iter: &mut TokensIter) -> Node {
    let token = tokens_iter.next().expect("error");
    if Token::MINUS == token {
        let zero = Token::INT(0);
        let zero_node = Node::new(zero, None, None);
        return Node::new(Token::MINUS, Some(zero_node), Some(unary(tokens_iter)));
    } else if Token::PLUS == token {
        return primary(tokens_iter);
    } else {
        tokens_iter.back();
        return primary(tokens_iter);
    }
}

// primary = num | ident | '(' expr ')'
fn primary<'a>(tokens_iter: &mut TokensIter) -> Node {
    let token = tokens_iter.next();
    if let Some(Token::INT(_)) = token {
        return Node::new(token.unwrap(), None, None);
    };
    if let Some(Token::LPARENTHESIS) = token {
        let expr_ = expr(tokens_iter);
        if let Some(Token::RPARENTHESIS) = tokens_iter.next() { return expr_ }
    }
    if let Some(Token::VARIABLE(_)) = token {
        return Node::new(token.unwrap(), None, None)
    }
    panic!("syatax error");
}

#[cfg(test)]
mod expr_test {
    use crate::code::Code;
    use crate::token::{Token, Tokens};
    use super::{expr, Node, Nodes, program};
    
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
        let code_str = "1 * 2 - (3 + 4);";
        let code = Code::new(code_str);
        let mut token_iter = Tokens::parse(&code).into_iter();
        let program = program(&mut token_iter);
        let one = Node::new(Token::INT(1), None, None);
        let two = Node::new(Token::INT(2), None, None);
        let three = Node::new(Token::INT(3), None, None);
        let four = Node::new(Token::INT(4), None, None);
        let one_ast_two = Node::new(Token::ASTARISK, Some(one), Some(two));
        let three_plus_four = Node::new(
            Token::PLUS, Some(three), Some(four));
        let ans = Node::new(Token::MINUS, Some(one_ast_two), Some(three_plus_four));
        let nodes = Nodes::new();
        nodes.push(ans);
        assert_eq!(nodes, program);
    }

    #[test]
    fn test_miuns() {
        let code_str = "-10";
        let code = Code::new(code_str);
        let mut tokens_iter = Tokens::parse(&code).into_iter();
        let expr = expr(&mut tokens_iter);
        let zero = Node::new(Token::INT(0), None, None);
        let three = Node::new(Token::INT(10), None, None);
        let ans = Node::new(Token::MINUS, Some(zero), Some(three));
        assert_eq!(ans, expr);
    }

    #[test]
    fn test_variable_1() {
        let code_str = "a;";
        let code = Code::new(&code_str);
        let mut tokens_iter = Tokens::parse(&code).into_iter();
        let nodes = program(&mut tokens_iter);
        let node_0 = nodes.get_nth(0);
        let ans = Node { kind: Token::VARIABLE("a".to_string()), lhs: None, rhs: None };
        assert_eq!(*node_0, ans);
    }

    #[test]
    fn test_variable_2() {
        let code_str = "a = 1; b = 2; a;";
        let code = Code::new(code_str);
        let mut tokens_iter = Tokens::parse(&code).into_iter();
        let nodes = program(&mut tokens_iter);
        let node_a = Node::new(Token::VARIABLE("a".to_string()), None, None);
        let node_b = Node::new(Token::VARIABLE("b".to_string()), None, None);
        let node_1 = Node::new(Token::INT(1), None, None);
        let node_2 = Node::new(Token::INT(2), None, None);
        let node_a_assign_1 = Node::new(Token::EQ, Some(node_a), Some(node_1));
        let node_b_assign_2 = Node::new(Token::EQ, Some(node_b), Some(node_2));
        assert_eq!(node_a_assign_1, *nodes.get_nth(0));
        assert_eq!(node_b_assign_2, *nodes.get_nth(1));
    }
}
