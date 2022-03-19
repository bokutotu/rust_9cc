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

fn gen_jump(block_num: &mut usize) -> String {
    *block_num = *block_num + 1;
    format!(".Lblock_{:}", block_num)
}

pub fn gen(nodes: &Nodes, objs: &Obj, block_num: &mut usize) -> String {
    let mut assembly = String::new();

    assembly.push_str(".intel_syntax noprefix\n");
    assembly.push_str(".globl _main\n");
    assembly.push_str("_main:\n");

    assembly.push_str(&prologue((objs.len() + 1) * 8));
    
    assembly.push_str(&gen_nodes(nodes, objs, block_num));

    assembly.push_str(".L_return:\n");
    assembly.push_str(&epiloge());
    assembly.push_str("    ret\n");
    assembly
}

fn gen_nodes(nodes: &Nodes, objs: &Obj, block_num: &mut usize) -> String {
    let mut assembly = String::new();

    for node in nodes.into_iter() {
        assembly.push_str(&gen_node(&node, objs, block_num));
    }
    assembly
}

fn gen_node(node: &Node, objs: &Obj, block_num: &mut usize) -> String {
    let mut assembly = String::new();
    if let Some(num) = node.num_expect() {
        assembly.push_str(&format!("    mov rax, {}\n", num));
        return assembly
    }
    if let Some(offset) = node.variable_offset_expect(objs) {
        let offset = (offset + 1) * 8;
        assembly.push_str(&load_from_stack(offset, "rax"));
        return assembly
    }

    if &Token::RETURN == node.kind() {
        let node_code = gen_node(node.lhs(), objs, block_num);
        assembly.push_str(&node_code);
        assembly.push_str("    jmp .L_return\n");
        return assembly
    }

    //        if condtion assembly
    //        jump-1-source
    //        if content assembly
    //
    //      ||  without          \\   with
    //      ||  else              \\  else
    //     \  /                  \  /
    //      \/                    \/
    //
    //     jump-1-direction     jump-2-source
    //                          jump-1-direction
    //                          else content assembly
    //                          jump-2-direction
    if &Token::IF == node.kind() {
        let if_condition_assembly = gen_node(node.if_condition(), objs, block_num);
        assembly.push_str(&if_condition_assembly);
        assembly.push_str("    cmp rax, 1\n");

        let jump_1_string = gen_jump(block_num);
        let jump_1_source_assembly = format!("    jne {:}\n", jump_1_string);
        assembly.push_str(&jump_1_source_assembly);

        let mut nodes = Nodes::new();
        nodes.push(node.if_content().clone());
        let if_content_assembly = gen_nodes(&nodes, objs, block_num);
        assembly.push_str(&if_content_assembly);

        // with else
        if node.is_else() {
            let jump_2_string = gen_jump(block_num);
            let jump_2_source = format!("    jmp {:}\n", jump_2_string);
            assembly.push_str(&jump_2_source);
            
            let jump_1_direction = format!("{:}:\n", jump_1_string);
            assembly.push_str(&jump_1_direction);

            let mut nodes = Nodes::new();
            nodes.push(node.else_content().clone());
            let else_content_assembly = gen_nodes(&nodes, objs, block_num);
            assembly.push_str(&else_content_assembly);

            let jump_2_string_assembly = format!("{:}:\n", jump_2_string);
            assembly.push_str(&jump_2_string_assembly);
        } else {
            let jump_1_direction = format!("{:}:\n", jump_1_string);
            assembly.push_str(&jump_1_direction);
        }
        return assembly
    }

    let lhs = node.lhs();
    let rhs = node.rhs();

    if &Token::EQ == node.kind() {
        let variable_offset = lhs
            .variable_offset_expect(objs)
            .expect("error");
        assembly.push_str(&gen_node(rhs, objs, block_num));
        assembly.push_str(&store_to_stack((variable_offset + 1) * 8, "rax"));
        return assembly
    }

    assembly.push_str(&gen_node(rhs, objs, block_num));
    assembly.push_str("    push rax\n");

    assembly.push_str(&gen_node(lhs, objs, block_num));
    assembly.push_str("    pop rdi\n");
    
    match node.kind() {
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
            assembly.push_str("    movzx eax, al\n");
        },
        Token::EXCLAMATIONEQ => {
            assembly.push_str("    cmp rax, rdi\n");
            assembly.push_str("    setne al\n");
            assembly.push_str("    movzx eax, al\n");
        },
        Token::GREATER => {
            assembly.push_str("    cmp rdi, rax\n");
            assembly.push_str("    setl al\n");
            assembly.push_str("    movzx eax, al\n");
        },
        Token::GREATEREQ => {
            assembly.push_str("    cmp rdi, rax\n");
            assembly.push_str("    setle al\n");
            assembly.push_str("    movzx eax, al\n");
        },
        Token::LESS => {
            assembly.push_str("    cmp rax, rdi\n");
            assembly.push_str("    setl al\n");
            assembly.push_str("    movzx eax, al\n");
        },
        Token::LESSEQ => {
            assembly.push_str("    cmp rax, rdi\n");
            assembly.push_str("    setle al\n");
            assembly.push_str("    movzx eax, al\n");
        },
        _ => {unreachable!()},
    }
    assembly
}

