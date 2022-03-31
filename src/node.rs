use crate::objs::Obj;
use crate::token::{Token, TokensIter};

#[derive(Debug, PartialEq, Clone)]
pub struct Node {
    kind: Token,
    lhs: Option<Box<Node>>,
    rhs: Option<Box<Node>>,
    // if
    if_condition: Option<Box<Node>>,
    if_content: Option<Box<Node>>,
    else_content: Option<Box<Node>>,
    // block
    block: Option<Box<Nodes>>,
    // while
    while_condition: Option<Box<Node>>,
    while_content: Option<Box<Node>>,
}

macro_rules! impl_return_reference {
    ($method_name: ident, $try_method_name: ident, $member: ident, $return_type: ty) => {
        pub fn $method_name(&self) -> $return_type {
            self.$try_method_name().expect("syntax error")
        }

        pub fn $try_method_name(&self) -> Option<$return_type> {
            self.$member.as_ref().map(|box_item| *box_item.clone())
        }
    };
}

impl Node {
    pub fn normal_init(kind_: Token, lhs: Option<Node>, rhs: Option<Node>) -> Self {
        Node {
            kind: kind_,
            lhs: lhs.map(Box::new),
            rhs: rhs.map(Box::new),
            if_condition: None,
            if_content: None,
            else_content: None,
            block: None,
            while_condition: None,
            while_content: None,
        }
    }

    pub fn if_init(if_condition: Node, if_content: Node, else_content: Option<Node>) -> Self {
        Node {
            kind: Token::IF,
            lhs: None,
            rhs: None,
            if_condition: Some(Box::new(if_condition)),
            if_content: Some(Box::new(if_content)),
            else_content: else_content.map(Box::new),
            block: None,
            while_content: None,
            while_condition: None,
        }
    }

    pub fn block_init(block: Nodes) -> Self {
        Node {
            kind: Token::BLOCK,
            lhs: None,
            rhs: None,
            if_condition: None,
            if_content: None,
            else_content: None,
            block: Some(Box::new(block)),
            while_content: None,
            while_condition: None,
        }
    }

    pub fn while_init(while_condition: Node, while_content: Node) -> Node {
        Node {
            kind: Token::WHILE,
            lhs: None,
            rhs: None,
            if_condition: None,
            if_content: None,
            else_content: None,
            block: None,
            while_condition: Some(Box::new(while_condition)),
            while_content: Some(Box::new(while_content)),
        }
    }

    pub fn num_expect(&self) -> Option<u64> {
        if let Token::INT(x) = self.kind() {
            Some(*x)
        } else {
            None
        }
    }

    pub fn variable_offset_expect(&self, objs: &Obj) -> Option<(usize, usize)> {
        if let Token::VARIABLE(..) = self.kind() {
            return Some(objs.variable_offset(self.kind().clone().variable().expect("error").0));
        }
        None
    }

    pub fn kind(&self) -> &Token {
        &self.kind
    }

    pub fn is_else(&self) -> bool {
        if self.kind != Token::IF {
            panic!("invalid call is_else method! this method is kind == IF only!");
        }
        self.else_content.is_some()
    }

    impl_return_reference!(lhs, try_lhs, lhs, Node);
    impl_return_reference!(rhs, try_rhs, rhs, Node);
    impl_return_reference!(if_condition, try_if_condition, if_condition, Node);
    impl_return_reference!(if_content, try_if_content, if_content, Node);
    impl_return_reference!(else_content, try_else_content, else_content, Node);
    impl_return_reference!(block, try_block, block, Nodes);
    impl_return_reference!(while_condition, try_while_condtion, while_condition, Node);
    impl_return_reference!(while_content, try_while_content, while_content, Node);
}

#[derive(Clone, Debug, PartialEq)]
pub struct Nodes {
    value: Vec<Node>,
    len: usize,
    // depth: usize,
}

impl Default for Nodes {
    fn default() -> Self {
        Self::new()
    }
}

impl Nodes {
    pub fn new() -> Nodes {
        let value_ = Vec::new();
        let len_ = 0;
        Nodes {
            value: value_,
            len: len_,
        }
    }

    pub fn push(&mut self, item: Node) {
        self.value.push(item);
        self.len += 1;
    }

    fn get_nth(&self, idx: usize) -> Node {
        self.value[idx].clone()
    }

    fn len(&self) -> usize {
        self.len
    }
}

impl<'a> IntoIterator for &'a Nodes {
    type Item = Node;
    type IntoIter = NodesIter;
    fn into_iter(self) -> Self::IntoIter {
        NodesIter {
            value: self.clone(),
            idx: 0,
        }
    }
}

pub struct NodesIter {
    value: Nodes,
    idx: usize,
}

impl Iterator for NodesIter {
    type Item = Node;
    fn next(&mut self) -> Option<Self::Item> {
        if self.idx == self.value.len() {
            return None;
        }
        self.idx += 1;
        Some(self.value.get_nth(self.idx - 1))
    }
}

/// program = stmt*
pub fn program(tokens_iter: &mut TokensIter) -> Nodes {
    let mut nodes = Nodes::new();
    loop {
        if tokens_iter.is_end() {
            break;
        }
        let stmt_ = match stmt(tokens_iter) {
            Some(x) => x,
            None => continue,
        };
        nodes.push(stmt_);
    }
    nodes
}

fn return_expr(tokens_iter: &mut TokensIter) -> Option<Node> {
    if tokens_iter.consume(Token::RETURN) {
        let expr_node = expr_node(tokens_iter).expect("syntax error");
        return Some(Node::normal_init(Token::RETURN, Some(expr_node), None));
    }
    None
}

fn if_else_expr(tokens_iter: &mut TokensIter) -> Option<Node> {
    let (condition, if_content) = match tokens_iter.consume(Token::IF) {
        true => {
            tokens_iter.consume_or_panic(Token::LPARENTHESIS);
            // DEPTH.with(|depth| *depth.borrow_mut() += 1);
            let condition = expr(tokens_iter);
            tokens_iter.consume_or_panic(Token::RPARENTHESIS);
            // DEPTH.with(|depth| *depth.borrow_mut() -= 1);
            let content = stmt(tokens_iter).expect("syntax error");
            (condition, content)
        }
        false => return None,
    };
    let else_content = match tokens_iter.consume(Token::ELSE) {
        true => stmt(tokens_iter),
        _ => None,
    };
    Some(Node::if_init(condition, if_content, else_content))
}

fn expr_node(tokens_iter: &mut TokensIter) -> Option<Node> {
    // 初手 ";"だった場合None
    if tokens_iter.consume(Token::COLON) {
        return None;
    }
    let expr = expr(tokens_iter);
    if tokens_iter.consume(Token::COLON) {
        return Some(expr);
    }
    None
}

fn block(tokens_iter: &mut TokensIter) -> Option<Node> {
    if tokens_iter.consume(Token::LCBRACKET) {
        // DEPTH.with(|depth| *depth.borrow_mut() += 1);
        let mut nodes = Nodes::new();
        loop {
            let block = match stmt(tokens_iter) {
                Some(x) => x,
                None => continue,
            };
            nodes.push(block);
            if tokens_iter.consume(Token::RCBRACKET) {
                // DEPTH.with(|depth| *depth.borrow_mut() -= 1);
                break;
            }
        }
        return Some(Node::block_init(nodes));
    }
    None
}

fn while_node(tokens_iter: &mut TokensIter) -> Option<Node> {
    if tokens_iter.consume(Token::WHILE) {
        tokens_iter.consume_or_panic(Token::LPARENTHESIS);
        let while_condition = expr(tokens_iter);
        tokens_iter.consume_or_panic(Token::RPARENTHESIS);
        let while_content = stmt(tokens_iter).expect("syntax error");
        return Some(Node::while_init(while_condition, while_content));
    }
    None
}

/// stmt = expr? ";"
///          | "return" expr ";"
///          | "if" "(" expr ")" stmt ("else" stmt)?
///          | "while" "(" expr ")" stmt
///          | "for" "("expr? ";" expr? ";" expr? ";" ")" stmt
///          | "{" stmt * "}"
fn stmt(tokens_iter: &mut TokensIter) -> Option<Node> {
    if let Some(x) = if_else_expr(tokens_iter) {
        return Some(x);
    }
    if let Some(x) = block(tokens_iter) {
        return Some(x);
    }
    if let Some(x) = return_expr(tokens_iter) {
        return Some(x);
    }
    if let Some(x) = while_node(tokens_iter) {
        return Some(x);
    }
    if let Some(x) = expr_node(tokens_iter) {
        return Some(x);
    }
    None
}

/// expr = assign
fn expr(tokens_iter: &mut TokensIter) -> Node {
    assign(tokens_iter)
}

/// assign = equality("=" assign)?
fn assign(tokens_iter: &mut TokensIter) -> Node {
    let equality_ = equality(tokens_iter);
    if tokens_iter.consume(Token::EQ) {
        return Node::normal_init(Token::EQ, Some(equality_), Some(assign(tokens_iter)));
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
    relational,
    add,
    Token::GREATER,
    Token::GREATEREQ,
    Token::LESS,
    Token::LESSEQ
);
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
        Node::normal_init(Token::MINUS, Some(zero_node), Some(unary(tokens_iter)))
    } else if Token::PLUS == token {
        primary(tokens_iter)
    } else {
        tokens_iter.back();
        primary(tokens_iter)
    }
}

/// primary = num | ident | '(' expr ')'
fn primary(tokens_iter: &mut TokensIter) -> Node {
    let token = tokens_iter.next();
    if let Some(Token::INT(_)) = token {
        return Node::normal_init(token.unwrap(), None, None);
    };
    if let Some(Token::LPARENTHESIS) = token {
        // DEPTH.with(|depth| *depth.borrow_mut() += 1);
        let expr_ = expr(tokens_iter);
        if let Some(Token::RPARENTHESIS) = tokens_iter.next() {
            // DEPTH.with(|depth| *depth.borrow_mut() -= 1);
            return expr_;
        }
    }
    if let Some(Token::VARIABLE(..)) = token {
        return Node::normal_init(token.unwrap(), None, None);
    }
    panic!("syatax error");
}

#[cfg(test)]
mod expr_test {
    use super::{expr, program, Node, Nodes};
    use crate::code::Code;
    use crate::token::{Token, Tokens};

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
        let one = Node::normal_init(Token::INT(1), None, None);
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
        let three_plus_four = Node::normal_init(Token::PLUS, Some(three), Some(four));
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
        let ans = Node::normal_init(Token::VARIABLE("a".to_string(), 0), None, None);
        assert_eq!(node_0, ans);
    }

    #[test]
    fn test_variable_2() {
        let code_str = "a = 1; b = 2; a;";
        let code = Code::new(code_str);
        let mut tokens_iter = Tokens::parse(&code).into_iter();
        let nodes = program(&mut tokens_iter);
        let node_a = Node::normal_init(Token::VARIABLE("a".to_string(), 0), None, None);
        let node_b = Node::normal_init(Token::VARIABLE("b".to_string(), 0), None, None);
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
        let node_a = Node::normal_init(Token::VARIABLE("a".to_string(), 0), None, None);
        let node_1 = Node::normal_init(Token::INT(1), None, None);
        let node_a_assign_1 = Node::normal_init(Token::EQ, Some(node_a), Some(node_1));
        assert_eq!(node_a_assign_1, nodes.get_nth(0));
        let node_a = Node::normal_init(Token::VARIABLE("a".to_string(), 0), None, None);
        let return_a = Node::normal_init(Token::RETURN, Some(node_a), None);
        assert_eq!(return_a, nodes.get_nth(1));
    }

    #[test]
    fn test_if_block() {
        let code_str = "a = 1; b = 2; if (a == b) return 0; else return 1;";
        let code = Code::new(&code_str);
        let mut tokens_iter = Tokens::parse(&code).into_iter();
        let nodes = program(&mut tokens_iter);
        let a_node = Node::normal_init(Token::VARIABLE("a".to_string(), 0), None, None);
        let b_node = Node::normal_init(Token::VARIABLE("b".to_string(), 0), None, None);
        let if_condition = Node::normal_init(Token::EQEQ, Some(a_node), Some(b_node));
        let if_content = Node::normal_init(
            Token::RETURN,
            Some(Node::normal_init(Token::INT(0), None, None)),
            None,
        );
        let else_content = Node::normal_init(
            Token::RETURN,
            Some(Node::normal_init(Token::INT(1), None, None)),
            None,
        );
        let ans_node = Node::if_init(if_condition, if_content, Some(else_content));
        assert_eq!(ans_node, nodes.get_nth(2));
    }

    #[test]
    fn while_test() {
        let code_str = "while (a == 10) { a = a + 1; }";
        let code = Code::new(code_str);
        let mut tokens_iter = Tokens::parse(&code).into_iter();
        let nodes = program(&mut tokens_iter);
        let condtion_code = "a == 10";
        let code = Code::new(condtion_code);
        let mut tokens_iter = Tokens::parse(&code).into_iter();
        let condtion_node = expr(&mut tokens_iter);
        let content_code = "{ a = a + 1; }";
        let code = Code::new(content_code);
        let mut tokens_iter = Tokens::parse(&code).into_iter();
        let content_node = program(&mut tokens_iter);
        let ans = Node::while_init(condtion_node, content_node.get_nth(0));
        assert_eq!(ans, nodes.get_nth(0));
    }
}
