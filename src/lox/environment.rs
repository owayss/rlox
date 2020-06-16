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

#[cfg(test)]
mod tests {
    use super::{super::token::TokenKind, Environment, Rc, RefCell, RuntimeErr, Token, Value};
    #[test]
    fn test_define() {
        let mut env = Environment::new(None);

        let t = Token::new(TokenKind::Identifier, "x".to_owned(), None, 1);
        env.define(&t.lexeme, Value::Number(3.0));
        assert_eq!(env.get(&t).unwrap(), Value::Number(3.0));

        let t = Token::new(TokenKind::Identifier, "b".to_owned(), None, 1);
        env.define(&t.lexeme, Value::Bool(false));
        assert_eq!(env.get(&t).unwrap(), Value::Bool(false));

        let t = Token::new(TokenKind::Identifier, "s".to_owned(), None, 1);
        env.define(&t.lexeme, Value::String("3.0".to_owned()));
        assert_eq!(env.get(&t).unwrap(), Value::String("3.0".to_owned()));
    }

    #[test]
    fn test_assign() {
        let mut env = Environment::new(None);

        // Assignment to unbound symbol
        let t = Token::new(TokenKind::Identifier, "x".to_owned(), None, 1);
        match env.assign("x", Value::Bool(false)).unwrap_err() {
            RuntimeErr::AssignmentToUndefined(s) => assert_eq!(&s, "x"),
            _ => assert!(false),
        }
        // Define and do valid assignment
        env.define(t.lexeme.clone().as_str(), Value::Number(3.0));
        assert_eq!(env.get(&t).unwrap(), Value::Number(3.0));
        // Assign a value of different type, also valid
        assert_eq!(
            env.assign("x", Value::Bool(false)).unwrap(),
            Value::Bool(false)
        );
    }
    #[test]
    fn test_get() {
        let mut env = Environment::new(None);
        let t = Token::new(TokenKind::Identifier, "x".to_owned(), None, 1);
        match env.get(&t) {
            Err(RuntimeErr::UndefinedSymbol(s)) => assert_eq!(&s, &t.lexeme),
            _ => assert!(false),
        }

        env.define(&t.lexeme, Value::Number(7.0));
        assert_eq!(env.get(&t).unwrap(), Value::Number(7.0));

        // Test block scope
        let outter = Rc::new(RefCell::new(env));
        // Variable from outer scope
        let mut inner = Environment::new(Some(Rc::clone(&outter)));
        assert_eq!(inner.get(&t).unwrap(), Value::Number(7.0));
        // Assign a different value within inner scope
        inner.define(&t.lexeme, Value::Number(9.0));
        assert_eq!(inner.get(&t).unwrap(), Value::Number(9.0));
        // Value in outter scope should remain unchanged
        assert_eq!(outter.borrow().get(&t).unwrap(), Value::Number(7.0));
    }
}
