use crate::objs::Obj;
use crate::token::Token;

#[derive(PartialEq, Clone)]
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
    // for
    for_first: Option<Box<Node>>,
    for_second: Option<Box<Node>>,
    for_third: Option<Box<Node>>,
    for_content: Option<Box<Node>>,
}

impl std::fmt::Debug for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.kind() {
            Token::VARIABLE(..) | Token::INT(..) | Token::DOUBLE(..) | Token::FLOAT(..) => {
                write!(f, "Node  kind:{:?}", self.kind())?
            }
            _ => writeln!(f, "Node\nkind:{:?}", self.kind())?,
        };
        if let Some(x) = self.try_lhs() {
            write!(f, "lhs: {:?}", x)?;
        }

        if let Some(x) = self.try_rhs() {
            write!(f, "rhs: {:?}", x)?;
        }

        if let Some(x) = self.try_if_condition() {
            write!(f, "if_condition: {:?}", x)?;
        }

        if let Some(x) = self.try_if_content() {
            write!(f, "if_content: {:?}", x)?;
        }

        if let Some(x) = self.try_else_content() {
            write!(f, "else_content: {:?}", x)?;
        }

        if let Some(x) = self.try_block() {
            write!(f, "block: {:?}", x)?;
        }

        if let Some(x) = self.try_while_condition() {
            write!(f, "while_condition: {:?}", x)?;
        }

        if let Some(x) = self.try_while_content() {
            write!(f, "while_content: {:?}", x)?;
        }

        if let Some(x) = self.try_for_first() {
            writeln!(f, "for_first: {:?}", x)?;
        }

        if let Some(x) = self.try_for_second() {
            writeln!(f, "for_second: {:?}", x)?;
        }

        if let Some(x) = self.try_for_third() {
            writeln!(f, "for_third: {:?}", x)?;
        }

        if let Some(x) = self.try_for_content() {
            write!(f, "for_content: {:?}", x)?;
        }

        Ok(())
    }
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

fn inc_depth(node: &mut Node) {
    if let Some((variable, depth)) = node.kind().variable() {
        node.kind = Token::VARIABLE(variable, depth + 1);
    }

    node.lhs = node.try_lhs().map(|mut inner| {
        inc_depth(&mut inner);
        Box::new(inner)
    });
    node.rhs = node.try_rhs().map(|mut inner| {
        inc_depth(&mut inner);
        Box::new(inner)
    });

    // node.block = node.try_block().map(|nodes| {
    //     let nodes = nodes
    //         .into_iter()
    //         .map(|mut node| {
    //             inc_depth(&mut node);
    //             Box::new(node)
    //         }).collect();
    //     Box::new(nodes)
    // });
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
            for_first: None,
            for_second: None,
            for_third: None,
            for_content: None,
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
            for_first: None,
            for_second: None,
            for_third: None,
            for_content: None,
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
            for_first: None,
            for_second: None,
            for_third: None,
            for_content: None,
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
            for_first: None,
            for_second: None,
            for_third: None,
            for_content: None,
        }
    }

    pub fn for_init(
        first: Option<Node>,
        second: Option<Node>,
        third: Option<Node>,
        content: Option<Node>,
    ) -> Node {
        Node {
            kind: Token::FOR,
            lhs: None,
            rhs: None,
            if_condition: None,
            if_content: None,
            else_content: None,
            block: None,
            while_condition: None,
            while_content: None,
            for_first: first.map(Box::new),
            for_second: second.map(Box::new),
            for_third: third.map(Box::new),
            for_content: content.map(Box::new),
        }
    }

    pub fn break_init() -> Node {
        Node {
            kind: Token::BREAK,
            lhs: None,
            rhs: None,
            if_condition: None,
            if_content: None,
            else_content: None,
            block: None,
            while_condition: None,
            while_content: None,
            for_first: None,
            for_second: None,
            for_third: None,
            for_content: None,
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

    /// inc depth when for condtion like for (int i = 0; i <= 10; i ++)
    /// in this condition it is usefull for inciment depth to check variable scope
    pub fn inc_depth(&mut self) {
        inc_depth(self);
    }

    impl_return_reference!(lhs, try_lhs, lhs, Node);
    impl_return_reference!(rhs, try_rhs, rhs, Node);
    impl_return_reference!(if_condition, try_if_condition, if_condition, Node);
    impl_return_reference!(if_content, try_if_content, if_content, Node);
    impl_return_reference!(else_content, try_else_content, else_content, Node);
    impl_return_reference!(block, try_block, block, Nodes);
    impl_return_reference!(while_condition, try_while_condition, while_condition, Node);
    impl_return_reference!(while_content, try_while_content, while_content, Node);
    impl_return_reference!(for_first, try_for_first, for_first, Node);
    impl_return_reference!(for_second, try_for_second, for_second, Node);
    impl_return_reference!(for_third, try_for_third, for_third, Node);
    impl_return_reference!(for_content, try_for_content, for_content, Node);
}

#[derive(Clone, PartialEq)]
pub struct Nodes {
    value: Vec<Node>,
    len: usize,
}

impl std::fmt::Debug for Nodes {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f, "Nodes")?;
        writeln!(f, "content:[")?;
        for content in self.into_iter() {
            writeln!(f, "{:?},", content)?;
        }
        writeln!(f, "]")?;
        writeln!(f, "len: {:?}", self.len)?;
        Ok(())
    }
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

    pub fn get_nth(&self, idx: usize) -> Node {
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

// impl FromIterator<Box<Node>> for Nodes {
//     fn from_iter<I: IntoIterator<Item=Box<Node>>>(iter: I) -> Self {
//         let mut vec = Vec::new();
//         for item in iter {
//             vec.push(*item);
//         }
//         let len = vec.len();
//         let nodes = Nodes {
//             value: vec,
//             len,
//         };
//         nodes
//     }
// }
