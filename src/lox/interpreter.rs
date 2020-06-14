use super::environment::{Environment, RuntimeErr};
use super::expr::Expr;
use super::stmt::Stmt;
use super::token::{Literal, TokenKind};
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug)]
pub struct Interpreter {
    environment: Rc<RefCell<Environment>>,
}
impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            environment: Rc::new(RefCell::new(Environment::new(None))),
        }
    }

    pub fn interpret(&mut self, stmts: Vec<Stmt>) -> Result<Option<Value>, RuntimeErr> {
        let mut val: Result<Option<Value>, RuntimeErr> = Ok(None);
        for s in &stmts {
            match s {
                Stmt::Expr(e) => {
                    val = self.eval(&e);
                }
                Stmt::Print(e) => {
                    val = self.eval(&e);
                    println!("{:#?}", val);
                }
                Stmt::Var(t, e) => {
                    val = self.eval(&e);
                    self.environment
                        .borrow_mut()
                        .define(t.lexeme.to_owned(), val.clone().unwrap().unwrap());
                }
                Stmt::Block(s) => {
                    let previous = Rc::clone(&self.environment);
                    {
                        self.environment = Rc::new(RefCell::new(Environment::new(Some(
                            Rc::clone(&self.environment),
                        ))));
                        val = self.interpret(s.to_vec());
                    }
                    self.environment = previous;
                }
            }
        }
        val
    }

    fn is_truthy(v: Option<&Value>) -> bool {
        if let Some(val) = v {
            match val {
                Value::Bool(b) => *b,
                _ => true,
            }
        } else {
            false
        }
    }
    // is_equal defines equality on all Value types, including mixed ones.
    // TODO: I had not thought of this initially. Do we want to define equality
    // on mixed types? Or should we disallow it?
    fn is_equal(v1: &Value, v2: &Value) -> bool {
        v1 == v2
    }
    fn eval_literal(l: &Literal) -> Option<Value> {
        match l {
            Literal::Identifier(s) => Some(Value::String(s.to_owned())),
            Literal::String(s) => Some(Value::String(s.to_owned())),
            Literal::Number(n) => Some(Value::Number(*n)),
            Literal::Bool(b) => Some(Value::Bool(*b)),
            Literal::Nil => None,
        }
    }
    // eval evaluates the different types of expressions that lox supports
    // using pattern matching on types. This looks OK now, but it might be a
    // good idea to take advantage of Rust's type system and define the
    // different operations as traits on our Value type.
    // TODO: come back to see this and change it to be based on traits, see how
    // that would look like.
    fn eval(&mut self, e: &Expr) -> Result<Option<Value>, RuntimeErr> {
        let mut ret: Option<Value> = None;
        match e {
            Expr::Literal(l) => {
                ret = Interpreter::eval_literal(l);
            }
            Expr::Grouping(g) => {
                ret = self.eval(g)?;
            }
            Expr::Unary(t, e) => {
                let r_val = self.eval(e)?;
                match t.kind {
                    TokenKind::Minus => {
                        if let Some(Value::Number(n)) = r_val {
                            ret = Some(Value::Number(-n));
                        } else {
                            return Err(RuntimeErr::UndefinedOperatorOnType(format!(
                                "Operator {:?} not defined on type {:?}",
                                t.kind, r_val
                            )));
                        }
                    }
                    TokenKind::Bang => {
                        ret = Some(Value::Bool(!Interpreter::is_truthy(r_val.as_ref())));
                    }
                    _ => {}
                }
            }
            Expr::Binary(t, e1, e2) => {
                let left = self.eval(e1)?;
                let right = self.eval(e2)?;
                match (left, right) {
                    (None, _) | (_, None) => ret = None,
                    // FIXME: do we want some operations to be defined on null?
                    // Specifically, the equality operator seem to make sense.
                    (Some(v1), Some(v2)) => match (&v1, &v2) {
                        (Value::Number(n1), Value::Number(n2)) => match t.kind {
                            TokenKind::Slash => {
                                ret = Some(Value::Number(n1 / n2));
                            }
                            TokenKind::Star => {
                                ret = Some(Value::Number(n1 * n2));
                            }
                            TokenKind::Minus => {
                                ret = Some(Value::Number(n1 - n2));
                            }
                            TokenKind::Plus => {
                                ret = Some(Value::Number(n1 + n2));
                            }
                            TokenKind::Less => {
                                ret = Some(Value::Bool(n1 < n2));
                            }
                            TokenKind::LessEqual => {
                                ret = Some(Value::Bool(n1 <= n2));
                            }
                            TokenKind::Greater => {
                                ret = Some(Value::Bool(n1 > n2));
                            }
                            TokenKind::GreaterEqual => {
                                ret = Some(Value::Bool(n1 >= n2));
                            }
                            TokenKind::EqualEqual => {
                                ret = Some(Value::Bool(Interpreter::is_equal(&v1, &v2)));
                            }
                            TokenKind::BangEqual => {
                                ret = Some(Value::Bool(!Interpreter::is_equal(&v1, &v2)));
                            }
                            _ => {
                                return Err(RuntimeErr::UndefinedOperatorOnType(format!(
                                    "Operator {:?} not defined on type Number",
                                    t.kind
                                )))
                            }
                        },

                        (Value::String(s1), Value::String(s2)) => match t.kind {
                            TokenKind::Plus => {
                                ret = Some(Value::String(format!("{}{}", s1, s2)));
                            }
                            TokenKind::EqualEqual => {
                                ret = Some(Value::Bool(Interpreter::is_equal(&v1, &v2)));
                            }
                            TokenKind::BangEqual => {
                                ret = Some(Value::Bool(!Interpreter::is_equal(&v1, &v2)));
                            }
                            _ => {
                                return Err(RuntimeErr::UndefinedOperatorOnType(format!(
                                    "Operator {:?} not defined on type String",
                                    t.kind
                                )))
                            }
                        },

                        (Value::Bool(b1), Value::Bool(b2)) => match t.kind {
                            TokenKind::And => {
                                ret = Some(Value::Bool(*b1 && *b2));
                            }
                            TokenKind::Or => {
                                ret = Some(Value::Bool(*b1 || *b2));
                            }
                            TokenKind::EqualEqual => {
                                ret = Some(Value::Bool(Interpreter::is_equal(&v1, &v2)));
                            }
                            TokenKind::BangEqual => {
                                ret = Some(Value::Bool(!Interpreter::is_equal(&v1, &v2)));
                            }
                            _ => {
                                return Err(RuntimeErr::UndefinedOperatorOnType(format!(
                                    "Operator {:?} not defined on type Bool",
                                    t.kind
                                )))
                            }
                        },
                        // type(v1) != type(v2)
                        (x, y) => match t.kind {
                            TokenKind::EqualEqual => {
                                ret = Some(Value::Bool(Interpreter::is_equal(&v1, &v2)));
                            }
                            TokenKind::BangEqual => {
                                ret = Some(Value::Bool(!Interpreter::is_equal(&v1, &v2)));
                            }
                            _ => {
                                return Err(RuntimeErr::UndefinedOperatorOnType(format!(
                                    "Operator {:#?} not defined on types ({:?}, {:?})",
                                    t.kind, x, y
                                )))
                            }
                        },
                    },
                }
            }
            Expr::Variable(t) => match self.environment.borrow().get(t.clone()) {
                Ok(val) => ret = Some(val),
                Err(err) => return Err(err),
            },
            Expr::Assignment(t, r_val) => {
                ret = self.eval(r_val)?;
                if let Err(err) = self
                    .environment
                    .borrow_mut()
                    .assign(t.lexeme.to_owned(), ret.clone().unwrap())
                {
                    return Err(err);
                }
            }
        }
        Ok(ret)
    }
}

#[derive(Debug, PartialEq, Clone)]
// We are effectively defining equality on any two values, even mixed types.
// TODO: Do we want this?
pub enum Value {
    String(std::string::String),
    Number(f64),
    Bool(bool),
    // TODO: do we want our own null type? or should we just take Rust's None?
}
