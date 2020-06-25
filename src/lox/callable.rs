use super::environment::{Environment, RuntimeErr};
use super::interpreter::{Interpreter, Value};
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
        interpreter.environment = Rc::clone(&self.environment);
        for i in 0..self.arity() {
            interpreter
                .environment
                .borrow_mut()
                .define(&self.declaration.params[i], args[i].clone());
        }
        interpreter.interpret(vec![self.declaration.body.clone()])
    }
}
