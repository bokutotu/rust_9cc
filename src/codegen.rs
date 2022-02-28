use crate::node::Node;

fn gen(node: &Node) -> String {
    let mut string = String::new();
    if let Some(num) = node.num_expect() {
        let tmp = format!("\tpush {}\n", num);
        string.push_str(&tmp);
        return string
    }
    
    let lhs = node.lhs();
    let rhs = node.rhs();

    string
}
