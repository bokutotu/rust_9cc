use crate::token::{Tokens, TokensIter};

fn hedder_x86_64() -> String {
    ".intel_syntax noprefix\n.globl main\nmain:\n".to_string()
}

fn expect_number(iter: &mut TokensIter) -> Option<usize> {
    let now_token = iter.next();
    match now_token {
        Some(x) => x.get_num(),
        None => None,
    }
}

impl Tokens {

    pub fn assembry(&self) -> String {
        let mut assembry = hedder_x86_64();
        let mut iter = self.iter();
        assembry.push_str(&format!("\tmov rax, {value}\n", value=expect_number(&mut iter).unwrap()));

        loop {
            let now_token = match iter.next() {
                Some(x) => x,
                None => { break }
            };
            if now_token.is_plus() {
                assembry.push_str(&format!("\tadd rax, {value}\n", 
                                           value=expect_number(&mut iter).unwrap()));

            }
            else if now_token.is_plus() {
                assembry.push_str(&format!("\tsub rax, {value}\n", 
                                           value=expect_number(&mut iter).unwrap()));
            }
            else {
                break
            }
        }
        assembry.push_str("\tret\n");
        assembry
    }
}
