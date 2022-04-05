use crate::node::{Node, Nodes};
use crate::token::{Token, TokensIter};

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
            let condition = expr(tokens_iter);
            tokens_iter.consume_or_panic(Token::RPARENTHESIS);
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
    tokens_iter.consume_or_panic(Token::COLON);
    Some(expr)
}

fn block(tokens_iter: &mut TokensIter) -> Option<Node> {
    if tokens_iter.consume(Token::LCBRACKET) {
        let mut nodes = Nodes::new();
        loop {
            let block = match stmt(tokens_iter) {
                Some(x) => x,
                None => continue,
            };
            nodes.push(block);
            if tokens_iter.consume(Token::RCBRACKET) {
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

fn for_node(tokens_iter: &mut TokensIter) -> Option<Node> {
    if tokens_iter.consume(Token::FOR) {
        tokens_iter.consume_or_panic(Token::LPARENTHESIS);
        let first = match tokens_iter.consume(Token::COLON) {
            false => {
                let mut res = expr(tokens_iter);
                res.inc_depth();
                tokens_iter.consume_or_panic(Token::COLON);
                Some(res)
            }
            true => None,
        };
        let second = match tokens_iter.consume(Token::COLON) {
            false => {
                let mut res = expr(tokens_iter);
                res.inc_depth();
                tokens_iter.consume_or_panic(Token::COLON);
                Some(res)
            }
            true => None,
        };

        let third = match tokens_iter.consume(Token::RPARENTHESIS) {
            false => {
                let mut res = expr(tokens_iter);
                res.inc_depth();
                tokens_iter.consume_or_panic(Token::RPARENTHESIS);
                Some(res)
            }
            true => None,
        };

        let content = stmt(tokens_iter);
        let node = Node::for_init(first, second, third, content);
        return Some(node);
    }
    None
}

/// stmt = expr? ";"
///          | "return" expr ";"
///          | "if" "(" expr ")" stmt ("else" stmt)?
///          | "while" "(" expr ")" stmt
///          | "for" "("expr? ";" expr? ";" expr? ")" stmt
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
    if let Some(x) = for_node(tokens_iter) {
        return Some(x);
    }
    if let Some(x) = expr_node(tokens_iter) {
        return Some(x);
    }
    None
}

/// expr = assign
///       | break
fn expr(tokens_iter: &mut TokensIter) -> Node {
    if tokens_iter.consume(Token::BREAK) {
        return Node::break_init();
    }
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

    #[test]
    fn for_test() {
        let code_str = "{ j = 0; for (i = 0; i <= 10; i = i + 1) { j = j + i; } }";
        let code = Code::new(&code_str);
        let mut tokens_iter = Tokens::parse(&code).into_iter();
        let nodes = program(&mut tokens_iter);
        let first = "{{i = 0;}}";
        let first = Code::new(&first);
        let mut first = Tokens::parse(&first).into_iter();
        let first = program(&mut first)
            .get_nth(0)
            .block()
            .get_nth(0)
            .block()
            .get_nth(0);
        let second = "{{i <= 10;}}";
        let second = Code::new(&second);
        let mut second = Tokens::parse(&second).into_iter();
        let second = program(&mut second)
            .get_nth(0)
            .block()
            .get_nth(0)
            .block()
            .get_nth(0);
        let third = "{{i = i + 1;}}";
        let third = Code::new(third);
        let mut third = Tokens::parse(&third).into_iter();
        let third = program(&mut third)
            .get_nth(0)
            .block()
            .get_nth(0)
            .block()
            .get_nth(0);
        let content = "{{ j = j + i; }}";
        let content = Code::new(content);
        let mut content = Tokens::parse(&content).into_iter();
        let content = program(&mut content).get_nth(0).block().get_nth(0);
        let ans = Node::for_init(Some(first), Some(second), Some(third), Some(content));
        let res_2nd = nodes.get_nth(0).block().get_nth(1);
        assert_eq!(ans, res_2nd);
    }
}
