use super::environment::Environment;
use super::interpreter::{Interpreter, RuntimeErr, Value};
use super::stmt::FnDeclaration;
use std::cell::RefCell;
use std::rc::Rc;
pub trait Callable {
    fn arity(&self) -> usize;
    fn call(&self, args: Vec<Value>) -> Result<Option<Value>, RuntimeErr>;
}

pub struct Function {
    declaration: FnDeclaration,
    environment: Rc<RefCell<Environment>>,
}

impl Function {
    pub fn new(declaration: FnDeclaration, environment: Rc<RefCell<Environment>>) -> Self {
        Function {
            declaration,
            environment,
        }
    }
}

impl Callable for Function {
    fn arity(&self) -> usize {
        self.declaration.params.len()
    }
    fn call(&self, args: Vec<Value>) -> Result<Option<Value>, RuntimeErr> {
        let mut interpreter = Interpreter::new();
        let mut env = Environment::new(Some(Rc::clone(&self.environment)));
        for i in 0..self.arity() {
            env.define(&self.declaration.params[i], args[i].clone());
        }
        interpreter.environment = Rc::new(RefCell::new(env));
        interpreter.interpret(vec![self.declaration.body.clone()])
    }
}
