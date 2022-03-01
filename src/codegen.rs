use crate::token::Token;
use crate::node::Node;

pub fn gen(node: &Node) -> String {
    let mut assembly = String::new();

    assembly.push_str(".intel_syntax noprefix\n");
    assembly.push_str(".globl main\n");
    assembly.push_str("main:\n");
    
    let inner = gen_inner(node);

    assembly.push_str(&inner);
    assembly.push_str("\tret\n");
    assembly
}

fn gen_inner(node: &Node) -> String {
    let mut assembly = String::new();
    if let Some(num) = node.num_expect() {
        let tmp = format!("\tpush {}\n", num);
        assembly.push_str(&tmp);
        return assembly
    }
    
    let lhs = node.lhs();
    let rhs = node.rhs();
    assembly.push_str(&gen_inner(&*lhs));
    assembly.push_str(&gen_inner(&*rhs));

    assembly.push_str("\tpop rdi\n");
    assembly.push_str("\tpop rax\n");
    
    match node.kind() {
        &Token::PLUS => {
            assembly.push_str("\tadd rax, rdi\n");
        },
        &Token::MINUS => {
            assembly.push_str("\tsub rax, rdi\n");
        },
        &Token::ASTARISK => {
            assembly.push_str("\timul rax, rdi\n");
        },
        &Token::SLASH => {
            assembly.push_str("\tcpo\n");
            assembly.push_str("\tidiv rdi\n");
        },
        _ => {unreachable!()},
    }
    assembly.push_str("\tpush rax\n");
    assembly
}

#[cfg(test)]
mod test_gen_code {
    use crate::node::Node;
    use crate::token::Token;
    use super::gen_inner;
    #[test]
    fn one_plus_two() {
        let two = Node::new(Token::INT(2), None, None);
        let three = Node::new(Token::INT(3), None, None);
        let four = Node::new(Token::INT(4), None, None);
        let five = Node::new(Token::INT(5), None, None);
        let two_ast_three = Node::new(Token::ASTARISK, Some(two), Some(three));
        let four_ast_five = Node::new(Token::ASTARISK, Some(four), Some(five));
        let node = Node::new(Token::ASTARISK, Some(two_ast_three), Some(four_ast_five));
        let assembly = gen_inner(&node);
        println!("{:?}", assembly);
    }
}
