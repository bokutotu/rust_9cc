use crate::token::Token;
use crate::node::Node;

pub fn gen(node: &Node) -> String {
    let mut assembly = String::new();

    assembly.push_str(".intel_syntax noprefix\n");
    assembly.push_str(".globl main\n");
    assembly.push_str("main:\n");
    
    let inner = gen_inner(node);

    assembly.push_str(&inner);
    assembly.push_str("    pop rax\n");
    assembly.push_str("    ret\n");
    assembly
}

fn gen_inner(node: &Node) -> String {
    let mut assembly = String::new();
    if let Some(num) = node.num_expect() {
        let tmp = format!("    push {}\n", num);
        assembly.push_str(&tmp);
        return assembly
    }
    
    let lhs = node.lhs();
    let rhs = node.rhs();
    assembly.push_str(&gen_inner(&*lhs));
    assembly.push_str(&gen_inner(&*rhs));

    assembly.push_str("    pop rdi\n");
    assembly.push_str("    pop rax\n");
    
    match *node.kind() {
        Token::PLUS => {
            assembly.push_str("    add rax, rdi\n");
        },
        Token::MINUS => {
            assembly.push_str("    sub rax, rdi\n");
        },
        Token::ASTARISK => {
            assembly.push_str("    imul rax, rdi\n");
        },
        Token::SLASH => {
            assembly.push_str("    cqo\n");
            assembly.push_str("    idiv rdi\n");
        },
        Token::EQEQ => {
            assembly.push_str("    cmp rax, rdi\n");
            assembly.push_str("    sete al\n");
            assembly.push_str("    movzb rax, al\n");
        },
        Token::EXCLAMATIONEQ => {
            assembly.push_str("    cmp rax, rdi\n");
            assembly.push_str("    setne al\n");
            assembly.push_str("    movzb rax, al\n");
        },
        Token::GREATER => {
            assembly.push_str("    cmp rdi, rax\n");
            assembly.push_str("    setl al\n");
            assembly.push_str("    movzb rax, al\n");
        },
        Token::GREATEREQ => {
            assembly.push_str("    cmp rdi, rax\n");
            assembly.push_str("    setle al\n");
            assembly.push_str("    movzb rax, al\n");
        },
        Token::LESS => {
            assembly.push_str("    cmp rax, rdi\n");
            assembly.push_str("    setl al\n");
            assembly.push_str("    movzb rax, al\n");
        },
        Token::LESSEQ => {
            assembly.push_str("    cmp rax, rdi\n");
            assembly.push_str("    setle al\n");
            assembly.push_str("    movzb rax, al\n");
        },
        _ => {unreachable!()},
    }
    assembly.push_str("    push rax\n");
    assembly
}

