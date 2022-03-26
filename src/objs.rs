use std::collections::HashMap;

use crate::node::{Node, Nodes};
use crate::token::{Token, TokensIter};

#[derive(Clone, Debug, PartialEq)]
pub struct Obj(HashMap<String, (usize, usize)>);

impl Default for Obj {
    fn default() -> Self {
        Self::new()
    }
}

impl Obj {
    pub fn new() -> Self {
        Obj(HashMap::new())
    }

    fn insert(&mut self, key: String, depth: usize) {
        match self.0.get(&key) {
            Some(_) => (),
            None => {
                let id = self.0.len();
                self.0.insert(key, (id, depth));
            }
        }
    }

    pub fn variable_offset(&self, variable: String) -> (usize, usize) {
        self.0[&variable]
    }

    pub fn from_tokens(tokens_iter: &mut TokensIter) -> Self {
        let mut res = Self::new();
        for token in tokens_iter {
            if let Token::VARIABLE(variable, depth) = token {
                res.insert(variable, depth);
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

fn node_check(node: &Node, objs: &Obj) {
    if let Some((variable_name, node_depth)) = node.kind().variable() {
        let (_, variable_depth) = objs.variable_offset(variable_name);
        if node_depth < variable_depth {
            panic!("variable depth error");
        }
    }
    if let Some(rhs) = node.try_rhs() {
        node_check(&rhs, objs)
    }
    if let Some(lhs) = node.try_lhs() {
        node_check(&lhs, objs)
    }
    if let Some(if_condition) = node.try_if_condition() {
        node_check(&if_condition, objs)
    }
    if let Some(if_content) = node.try_if_content() {
        if let Some(if_content_block) = if_content.try_block() {
            nodes_check(&if_content_block, objs);
        } else {
            node_check(&if_content, objs);
        }
    }
    if let Some(block) = node.try_block() {
        nodes_check(&block, objs)
    }
    if let Some(else_content) = node.try_else_content() {
        if let Some(else_content_block) = else_content.try_block() {
            nodes_check(&else_content_block, objs);
        } else {
            node_check(&else_content, objs);
        }
    }
}

fn nodes_check(nodes: &Nodes, objs: &Obj) {
    let nodes_iter = nodes.into_iter();
    for node in nodes_iter {
        node_check(&node, objs);
    }
}

pub fn variable_block_check(nodes: &Nodes, objs: &Obj) {
    nodes_check(nodes, objs);
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
        ("oppai".to_string(), (0, 0)),
        ("boyoyon".to_string(), (1, 0)),
        ("boron".to_string(), (2, 0)),
    ]));
    assert_eq!(ans, obj)
}

#[test]
fn test_scope() {
    use crate::code::Code;
    use crate::node::program;
    use crate::token::Tokens;
    let code_str = "{a = 10; { b = 2; a = 20; }}";
    let code = Code::new(code_str);
    let mut tokens_iter = Tokens::parse(&code).into_iter();
    let obj = Obj::from_tokens(&mut tokens_iter);
    tokens_iter.index_reset();
    let nodes = program(&mut tokens_iter);
    variable_block_check(&nodes, &obj);
}

#[test]
#[should_panic]
fn test_scope_should_panic() {
    use crate::code::Code;
    use crate::node::program;
    use crate::token::Tokens;
    let code_str = "{ a = 10; { b = 20; } return b; }";
    let code = Code::new(code_str);
    let mut tokens_iter = Tokens::parse(&code).into_iter();
    let objs = Obj::from_tokens(&mut tokens_iter);
    println!("{:?}", objs);
    println!("{:?}", tokens_iter);
    tokens_iter.index_reset();
    let nodes = program(&mut tokens_iter);
    variable_block_check(&nodes, &objs);
}
