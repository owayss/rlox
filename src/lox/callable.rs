use super::environment::Environment;
use super::interpreter::{Interpreter, RuntimeErr, Value};
use super::stmt::FnDeclaration;
use std::cell::RefCell;
use std::rc::Rc;
pub trait Callable {
    fn arity(&self) -> usize;
    fn call(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<Value>,
    ) -> Result<Option<Value>, RuntimeErr>;
}

pub struct Function {
    declaration: FnDeclaration,
}

impl Function {
    pub fn new(declaration: FnDeclaration) -> Self {
        Function { declaration }
    }
}

impl Callable for Function {
    fn arity(&self) -> usize {
        self.declaration.params.len()
    }
    fn call(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<Value>,
    ) -> Result<Option<Value>, RuntimeErr> {
        let mut env = Environment::new(Some(Rc::clone(&interpreter.environment)));
        for i in 0..self.arity() {
            env.define(&self.declaration.params[i], args[i].clone());
        }
        interpreter.environment = Rc::new(RefCell::new(env));
        interpreter.interpret(vec![self.declaration.body.clone()])
    }
}
