use super::interpreter::Value;
use super::token::Token;
use std::collections::HashMap;
#[derive(Debug)]
pub struct Environment {
    globals: HashMap<String, Value>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            globals: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.globals.insert(name, value);
    }

    pub fn get(&self, t: Token) -> Value {
        self.globals.get(&t.lexeme).unwrap().clone()
    }
}
