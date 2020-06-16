use super::interpreter::Value;
use super::token::Token;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum RuntimeErr {
    UndefinedSymbol(String),
    AssignmentToUndefined(String),
    UndefinedOperatorOnType(String),
}
#[derive(Debug, Clone)]
pub struct Environment {
    globals: HashMap<String, Value>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new(enclosing: Option<Rc<RefCell<Environment>>>) -> Self {
        Environment {
            globals: HashMap::new(),
            enclosing: enclosing,
        }
    }

    pub fn get(&self, t: &Token) -> Result<Value, RuntimeErr> {
        if let Some(val) = self.globals.get(&t.lexeme) {
            Ok(val.clone())
        } else if let Some(env) = &self.enclosing {
            env.borrow().get(t)
        } else {
            Err(RuntimeErr::UndefinedSymbol(t.lexeme.to_owned()))
        }
    }

    pub fn define(&mut self, name: &str, value: Value) {
        self.globals.insert(name.to_owned(), value);
    }

    pub fn assign(&mut self, name: &str, value: Value) -> Result<Value, RuntimeErr> {
        if let Some(val) = self.globals.get_mut(name) {
            *val = value;
            Ok(val.clone())
        } else if let Some(enc) = &self.enclosing {
            enc.borrow_mut().assign(name, value)
        } else {
            Err(RuntimeErr::AssignmentToUndefined(name.to_owned()))
        }
    }
}
