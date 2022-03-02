use std::cell::Cell;

#[derive(Debug)]
pub struct Code {
    code: Vec<char>,
    len: usize,
    idx: Cell<usize>
}

impl Code {
    pub fn new(code: &str) -> Self {
        let str_len = code.len();
        let str_vec = code.chars().collect();
        Self {
            code: str_vec,
            len: str_len,
            idx: Cell::new(0),
        }
    }

    pub fn now(&self) -> Option<char> {
        if self.is_end() { return None }
        Some(self.code[self.idx.get()])
    }

    pub fn now_2_char(&self) -> Option<(char, char)> {
        if self.idx.get() + 2 > self.len { return None }
        Some((self.code[self.idx.get()], self.code[self.idx.get() + 1]))
    }

    pub fn now_3_char(&self) -> Option<(char, char, char)> {
        if self.idx.get() + 3 > self.len { return None }
        Some((
            self.code[self.idx.get()], self.code[self.idx.get() + 1], self.code[self.idx.get() + 2]
        ))
    }

    pub fn inc_idx(&self) {
        self.idx.set(self.idx.get() + 1);
    }

    pub fn is_end(&self) -> bool {
        self.len == self.idx.get()
    }
}

#[test]
fn test_now() {
    let code_str = "103 + 12";
    let code = Code::new(code_str);
    let now_1 = code.now().unwrap();
    assert_eq!('1', now_1);
    let now_2 = code.now_2_char().unwrap();
    assert_eq!(('1', '0'),now_2);
    let now_3 = code.now_3_char().unwrap();
    assert_eq!(('1', '0', '3'),now_3);
}

fn char_to_num(value: char) -> Option<u64> {
    match value.to_digit(10) {
        Some(x) => Some(x as u64),
        None => None
    }
}

pub fn strtol(code: &Code) -> Option<u64> {
    let now_char = code.now().expect("idx overflow");
    if char_to_num(now_char) == None {
        return None
    }
    let mut num = 0;
    loop {
        let value = match code.now() {
            Some(x) => x,
            None => break
        };
        match char_to_num(value) {
            Some(x) => { 
                num = num * 10 + x;
                code.inc_idx();
            },
            None => break
        }
    }
    Some(num)
}

#[test]
fn int_parse() {
    let code_str = "20 > 594";
    let code = Code::new(code_str);
    let num_first = strtol(&code).unwrap();
    assert_eq!(num_first, 20);
    assert_eq!(code.now().unwrap(), ' ');
}

#[test]
fn not_num() {
    let code_str = "> 594";
    let code = Code::new(code_str);
    let num_first = strtol(&code);
    assert_eq!(num_first, None);
    assert_eq!(code.now().unwrap(), '>');
}

fn is_alphabet(value: &char) -> bool {
    let value = *value as u32 as usize;
    // A-Z
    if 65 <= value && value <= 90 {
        return true;
    } 
    // a-z
    else if 97 <= value && value <= 122 {
        return true;
    }
    false
}

#[test]
fn alphabet_edge() {
    let test = '@';
    assert_eq!(is_alphabet(&test), false);
    let test = 'A';
    assert_eq!(is_alphabet(&test), true);
    let test = 'Z';
    assert_eq!(is_alphabet(&test), true);
    let test = '[';
    assert_eq!(is_alphabet(&test), false);
}

fn is_number(value: &char) -> bool {
    let value = *value as u32 as usize;
    if 48 <= value && value <= 57 {
        return true;
    }
    false
}

#[test]
fn number_edge() {
    let test = '/';
    assert_eq!(is_number(&test), false);
    let test = '0';
    assert_eq!(is_number(&test), true);
    let test = '9';
    assert_eq!(is_number(&test), true);
    let test = ':';
    assert_eq!(is_number(&test), false);
}

fn is_first(value: &char) -> bool {
    if is_alphabet(value) || *value == '_' {
        return true;
    } else {
        return false;
    }
}

#[test]
fn first_egde() {
    let test = '@';
    assert_eq!(is_first(&test), false);
    let test = 'A';
    assert_eq!(is_first(&test), true);
    let test = 'Z';
    assert_eq!(is_first(&test), true);
    let test = '[';
    assert_eq!(is_first(&test), false);
    let test = '_';
    assert_eq!(is_first(&test), true);

    let test = '\'';
    assert_eq!(is_first(&test), false);
    let test = 'a';
    assert_eq!(is_first(&test), true);
    let test = 'z';
    assert_eq!(is_first(&test), true);
    let test = '{';
    assert_eq!(is_first(&test), false);
}

fn is_ident_element(value: &char) -> bool {
    if is_alphabet(value) || *value == '_' || is_number(value) {
        return true;
    }
    false
}

pub fn ident<'a>(code: &Code) -> Option<String> {
    let now = code.now().expect("index out of bound");
    if !is_first(&now) {
        return None
    }
    code.inc_idx();
    let mut string = String::new();
    string.push(now);
    loop {
        let now = match code.now() {
            Some(x) => x,
            None => return Some(string),
        };
        if is_ident_element(&now) {
            string.push(now);
            code.inc_idx();
        } else {
            return Some(string);
        }
    }
}

#[test]
fn ident_test() {
    let code_str = "oppai = 10";
    let code = Code::new(&code_str);
    let first_oppai = ident(&code).unwrap();
    assert_eq!(first_oppai, "oppai".to_string());
    assert_eq!(code.now().unwrap(), ' ');
    code.inc_idx();
    let none = ident(&code);
    assert_eq!(none, None);
    assert_eq!(code.now().unwrap(), '=');
    code.inc_idx();
    code.inc_idx();
    assert_eq!(strtol(&code).unwrap(), 10);
}

pub fn pass_space(code: &Code) {
    loop {
        let now = match code.now() {
            Some(x) => x,
            None => break
        };
        if now == ' ' {
            code.inc_idx();
            continue;
        } 
        else {break}
    }
}
