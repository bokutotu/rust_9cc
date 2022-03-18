use crate::token::{Token, TokensIter};
use crate::objs::Obj;

#[derive(Debug, PartialEq, Clone)]
pub struct Node {
    kind: Token,
    lhs: Option<Box<Node>>,
    rhs: Option<Box<Node>>,
    // if 
    if_condition: Option<Box<Node>>,
    if_content: Option<Box<Node>>,
    else_content: Option<Box<Node>>,
}

fn option_node_option_box(input: Option<Node>) -> Option<Box<Node>> {
    match input {
        Some(x) => Some(Box::new(x)),
        None => None
    }
}

impl Node {
    pub fn normal_init(kind_: Token, lhs: Option<Node>, rhs: Option<Node>) -> Self {
        Node {
            kind: kind_,
            lhs: option_node_option_box(lhs),
            rhs: option_node_option_box(rhs),
            if_condition: None,
            if_content: None,
            else_content: None,
        }
    }

    pub fn if_init(if_condition: Node, if_content: Node, else_content: Option<Node>) -> Self {
        Node {
            kind: Token::IF,
            lhs: None,
            rhs: None,
            if_condition: option_node_option_box(Some(if_condition)),
            if_content: option_node_option_box(Some(if_content)),
            else_content: option_node_option_box(else_content),
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

    pub fn lhs(&self) -> &Node {
        if let Some(x) = &self.lhs {
            return x
        } else {
            panic!("error");
        }
    }

    pub fn rhs(&self) -> &Node {
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
    value: Vec<Node>,
    len: usize,
}

impl Nodes {
    pub fn new() -> Nodes {
        let value_ = Vec::new();
        let len_ = 0;
        Nodes { value: value_, len: len_ }
    }

    pub fn push(&mut self, item: Node) {
        self.value.push(item);
        self.len = self.len + 1;
    }

    pub fn into_iter(&self) -> NodeIter {
        NodeIter {
            value: self.clone(),
            idx: 0
        }
    }

    fn get_nth(&self, idx: usize) -> Node {
        self.value[idx].clone()
    }

    fn len(&self) -> usize {
        self.len
    }
}

pub struct NodeIter {
    value: Nodes,
    idx: usize,
}

impl Iterator for NodeIter {
    type Item = Node;
    fn next(&mut self) -> Option<Self::Item> {
        if self.idx == self.value.len() {
            return None
        }
        self.idx = self.idx + 1;
        Some(self.value.get_nth(self.idx - 1))
    }
}

// program = stmt*
pub fn program(tokens_iter: &mut TokensIter) -> Nodes {
    let mut nodes = Nodes::new();
    loop {
        if tokens_iter.is_end() { break }
        let stmt_ = stmt(tokens_iter);
        nodes.push(stmt_);
    }
    nodes
}

fn return_expr(tokens_iter: &mut TokensIter) -> Option<Node> {
    if tokens_iter.consume(Token::RETURN) {
        let expr_node = expr_node(tokens_iter).expect("syntax error");
        return Some(Node::normal_init(Token::RETURN, Some(expr_node), None))
    }
    None
}

fn if_else_expr(tokens_iter: &mut TokensIter) -> Option<Node> {
    let (condition, if_content) = match tokens_iter.consume(Token::IF) {
        true => {
            tokens_iter.consume_or_panic(Token::LPARENTHESIS);
            let condition = expr(tokens_iter);
            tokens_iter.consume_or_panic(Token::RPARENTHESIS);
            let content = stmt(tokens_iter);
            (condition, content)
        },
        false => return None
    };
    let else_content = match tokens_iter.consume(Token::ELSE) {
        true => Some(stmt(tokens_iter)),
        _ => None
    };
    Some(Node::if_init(condition, if_content, else_content))
}

// fn while_expr(tokens_iter: &mut TokensIter) -> Option<Node> {
//     None
// }

// fn for_expr(tokens_iter: &mut TokensIter) -> Option<Node> {
//     None
// }

fn expr_node(tokens_iter: &mut TokensIter) -> Option<Node> {
    let expr = expr(tokens_iter);
    if tokens_iter.consume(Token::COLON) {
        return Some(expr)
    }
    None
}

/// stmt = expr ";" 
///          | "return" expr ";" 
///          | "if" "(" expr ")" stmt ("else" stmt)?
///          | "while" "(" expr ")" stmt
///          | "for" "("expr? ";" expr? ";" expr? ";" ")" stmt
fn stmt(tokens_iter: &mut TokensIter) -> Node {
    if let Some(x) = if_else_expr(tokens_iter)  { return x; }
    // if let Some(x) = while_expr(tokens_iter)    { return x; }
    // if let Some(x) = for_expr(tokens_iter)      { return x; }
    if let Some(x) = return_expr(tokens_iter)   { return x; }
    if let Some(x) = expr_node(tokens_iter)     { return x; }
    panic!("syntax error");
}

// expr = assign
fn expr(tokens_iter: &mut TokensIter) -> Node {
    assign(tokens_iter)
}

// assign = equality("=" assign)?
fn assign(tokens_iter: &mut TokensIter) -> Node {
    let equality_ = equality(tokens_iter);
    if tokens_iter.consume(Token::EQ) {
        return Node::normal_init(
            Token::EQ, 
            Some(equality_), 
            Some(assign(tokens_iter)), 
        )
    }
    equality_
}

macro_rules! impl_node_function {
    ($fn_name:ident, $child_fn:ident, $($token:path),*) => {
        pub fn $fn_name(tokens_iter: &mut TokensIter) -> Node {
            let mut node = $child_fn(tokens_iter);
            loop {
                match tokens_iter.next() {
                    $(
                        Some($token) => {
                            node = Node::normal_init(
                                $token, Some(node), Some($child_fn(tokens_iter)),
                            );
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
fn unary(tokens_iter: &mut TokensIter) -> Node {
    let token = tokens_iter.next().expect("error");
    if Token::MINUS == token {
        let zero = Token::INT(0);
        let zero_node = Node::normal_init(zero, None, None);
        return Node::normal_init(
            Token::MINUS, Some(zero_node), Some(unary(tokens_iter)));
    } else if Token::PLUS == token {
        return primary(tokens_iter);
    } else {
        tokens_iter.back();
        return primary(tokens_iter);
    }
}

// primary = num | ident | '(' expr ')'
fn primary(tokens_iter: &mut TokensIter) -> Node {
    let token = tokens_iter.next();
    if let Some(Token::INT(_)) = token {
        return Node::normal_init(token.unwrap(), None, None);
    };
    if let Some(Token::LPARENTHESIS) = token {
        let expr_ = expr(tokens_iter);
        if let Some(Token::RPARENTHESIS) = tokens_iter.next() { return expr_ }
    }
    if let Some(Token::VARIABLE(_)) = token {
        return Node::normal_init(token.unwrap(), None, None)
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
        let code_str = "1;";
        let code = Code::new(code_str);
        let mut tokens_iter = Tokens::parse(&code).into_iter();
        let expr_ = expr(&mut tokens_iter);
        let ans = Node::normal_init(Token::INT(1), None, None);
        assert_eq!(ans, expr_);
    }

    #[test]
    fn test_2() {
        let code_str = "1 + 2";
        let code = Code::new(code_str);
        let mut tokens_iter = Tokens::parse(&code).into_iter();
        let expr_ = expr(&mut tokens_iter);
        let one = Node::normal_init(Token::INT(1),None,None);
        let two = Node::normal_init(Token::INT(2), None, None);
        let ans = Node::normal_init(Token::PLUS, Some(one), Some(two));
        assert_eq!(ans, expr_);
    }

    #[test]
    fn test_3() {
        let code_str = "1 * 2";
        let code = Code::new(code_str);
        let mut tokens_iter = Tokens::parse(&code).into_iter();
        let expr_ = expr(&mut tokens_iter);
        let one = Node::normal_init(Token::INT(1), None, None);
        let two = Node::normal_init(Token::INT(2), None, None);
        let ans = Node::normal_init(Token::ASTARISK, Some(one), Some(two));
        assert_eq!(ans, expr_);
    }

    #[test]
    fn test_4() {
        let code_str = "1 * 2 - (3 + 4);";
        let code = Code::new(code_str);
        let mut token_iter = Tokens::parse(&code).into_iter();
        let program = program(&mut token_iter);
        let one = Node::normal_init(Token::INT(1), None, None);
        let two = Node::normal_init(Token::INT(2), None, None);
        let three = Node::normal_init(Token::INT(3), None, None);
        let four = Node::normal_init(Token::INT(4), None, None);
        let one_ast_two = Node::normal_init(Token::ASTARISK, Some(one), Some(two));
        let three_plus_four = Node::normal_init(
            Token::PLUS, Some(three), Some(four));
        let ans = Node::normal_init(Token::MINUS, Some(one_ast_two), Some(three_plus_four));
        let mut nodes = Nodes::new();
        nodes.push(ans);
        assert_eq!(nodes, program);
    }

    #[test]
    fn test_miuns() {
        let code_str = "-10";
        let code = Code::new(code_str);
        let mut tokens_iter = Tokens::parse(&code).into_iter();
        let expr = expr(&mut tokens_iter);
        let zero = Node::normal_init(Token::INT(0), None, None);
        let three = Node::normal_init(Token::INT(10), None, None);
        let ans = Node::normal_init(Token::MINUS, Some(zero), Some(three));
        assert_eq!(ans, expr);
    }

    #[test]
    fn test_variable_1() {
        let code_str = "a;";
        let code = Code::new(&code_str);
        let mut tokens_iter = Tokens::parse(&code).into_iter();
        let nodes = program(&mut tokens_iter);
        let node_0 = nodes.get_nth(0);
        let ans = Node::normal_init(Token::VARIABLE("a".to_string()), None, None);
        assert_eq!(node_0, ans);
    }

    #[test]
    fn test_variable_2() {
        let code_str = "a = 1; b = 2; a;";
        let code = Code::new(code_str);
        let mut tokens_iter = Tokens::parse(&code).into_iter();
        let nodes = program(&mut tokens_iter);
        let node_a = Node::normal_init(Token::VARIABLE("a".to_string()), None, None);
        let node_b = Node::normal_init(Token::VARIABLE("b".to_string()), None, None);
        let node_1 = Node::normal_init(Token::INT(1), None, None);
        let node_2 = Node::normal_init(Token::INT(2), None, None);
        let node_a_assign_1 = Node::normal_init(Token::EQ, Some(node_a), Some(node_1));
        let node_b_assign_2 = Node::normal_init(Token::EQ, Some(node_b), Some(node_2));
        assert_eq!(node_a_assign_1, nodes.get_nth(0));
        assert_eq!(node_b_assign_2, nodes.get_nth(1));
    }

    #[test]
    fn test_return() {
        let code_str = "a = 1; return a;";
        let code = Code::new(code_str);
        let mut tokens_iter = Tokens::parse(&code).into_iter();
        let nodes = program(&mut tokens_iter);
        let node_a = Node::normal_init(Token::VARIABLE("a".to_string()), None, None);
        let node_1 = Node::normal_init(Token::INT(1), None, None);
        let node_a_assign_1 = Node::normal_init(Token::EQ, Some(node_a), Some(node_1));
        assert_eq!(node_a_assign_1, nodes.get_nth(0));
        let node_a = Node::normal_init(Token::VARIABLE("a".to_string()), None, None);
        let return_a = Node::normal_init(Token::RETURN, Some(node_a), None);
        assert_eq!(return_a, nodes.get_nth(1));
    }

    #[test]
    fn test_if_block() {
        let code_str = "if (a == b) return 0; else return 1;";
        let code = Code::new(&code_str);
        let mut tokens_iter = Tokens::parse(&code).into_iter();
        let nodes = program(&mut tokens_iter);
        let a_node = Node::normal_init(Token::VARIABLE("a".to_string()), None, None);
        let b_node = Node::normal_init(Token::VARIABLE("b".to_string()), None, None);
        let if_condition = Node::normal_init(Token::EQEQ, Some(a_node), Some(b_node));
        let if_content = Node::normal_init( 
            Token::RETURN, Some(Node::normal_init(Token::INT(0), None, None)), None);
        let else_content = Node::normal_init(
            Token::RETURN, Some(Node::normal_init(Token::INT(1), None, None)), None);
        let ans_node = Node::if_init(if_condition, if_content, Some(else_content));
        assert_eq!(ans_node, nodes.get_nth(0));
    }
}
