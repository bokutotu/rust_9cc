use crate::token::Token;
use crate::node::{Nodes, Node};
use crate::objs::Obj;

fn prologue(offset: usize) -> String {
    format!("    push rbp\n    mov rbp, rsp\n    sub rsp, {:?}\n", offset)
}

fn epiloge() -> String {
    "    mov rsp, rbp\n    pop rbp\n".to_string()
}

fn load_from_stack(offset: usize, target_register: &str) -> String {
    format!("    mov {:}, qword ptr [rbp - {:}]\n", target_register, offset)
}

fn store_to_stack(offset: usize, target_register: &str) -> String {
    format!("    mov qword ptr [rbp - {:}], {:}\n", offset, target_register)
}

pub fn gen(nodes: &Nodes, objs: &Obj) -> String {
    let mut assembly = String::new();

    assembly.push_str(".intel_syntax noprefix\n");
    assembly.push_str(".globl main\n");
    assembly.push_str("main:\n");

    assembly.push_str(&prologue((objs.len() + 1) * 8));

    for node in nodes.into_iter() {
        match node.variable_offset_expect(objs) {
            Some(x) => {
                assembly.push_str(&load_from_stack((x+1) * 8, "rax"));
                // assembly.push_str("    push rax\n");
            },
            None => {
                let node_code = gen_node(&*node, objs);
                assembly.push_str(&node_code);
            },
        }
    }

    // assembly.push_str("    pop rax\n");
    assembly.push_str(&epiloge());
    assembly.push_str("    ret\n");
    assembly
}

fn gen_node(node: &Node, objs: &Obj) -> String {
    let mut assembly = String::new();
    if let Some(num) = node.num_expect() {
        assembly.push_str(&format!("    mov rax, {}\n", num));
        return assembly
    }

    if let Some(offset) = node.variable_offset_expect(objs) {
        let offset = (offset + 1) * 8;
        assembly.push_str(&load_from_stack(offset, "rax"));
        // assembly.push_str("    push rax\n");
        return assembly
    }


    let lhs = node.lhs();
    let rhs = node.rhs();

    if Token::EQ == *node.kind() {
        let variable_offset = lhs
            .variable_offset_expect(objs)
            .expect("error");
        assembly.push_str(&gen_node(rhs, objs));
        assembly.push_str(&store_to_stack((variable_offset + 1) * 8, "rax"));
        return assembly
    }

    assembly.push_str(&gen_node(&*rhs, objs));
    assembly.push_str("    push rax\n");

    assembly.push_str(&gen_node(&*lhs, objs));
    assembly.push_str("    pop rdi\n");
    
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
    // assembly.push_str("    push rax\n");
    assembly
}

