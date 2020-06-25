use super::callable::{Callable, Function};
use super::environment::{Environment, RuntimeErr};
use super::expr::Expr;
use super::stmt::{FnDeclaration, Stmt};
use super::token::{Literal, TokenKind};
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
// We are effectively defining equality on any two values, even mixed types.
// TODO: Do we want this?
pub enum Value {
    String(std::string::String),
    Number(f64),
    Bool(bool),
    Fn(FnDeclaration),
    // TODO: do we want our own null type? or should we just take Rust's None?
}
impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::String(s) => write!(f, "{}", s),
            Value::Number(n) => write!(f, "{}", n),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Fn(declaration) => write!(
                f,
                "<fn {}({})>",
                declaration.name,
                declaration.params.join(", ")
            ),
        }
    }
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
#[derive(Debug)]
pub struct Interpreter {
    pub environment: Rc<RefCell<Environment>>,
}
impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            environment: Rc::new(RefCell::new(Environment::new(None))),
        }
    }

    pub fn interpret(&mut self, stmts: Vec<Stmt>) -> Result<Option<Value>, RuntimeErr> {
        let mut val: Option<Value> = None;
        for s in stmts {
            match s {
                Stmt::Expr(e) => {
                    val = self.eval(&e)?;
                }
                Stmt::Print(e) => {
                    val = self.eval(&e)?;
                    match &val {
                        Some(val) => println!("{}", val),
                        // FIXME: this is really still in an exploration phase.
                        // The key thing to decide for our interpreter here is
                        // whether the use of None to represent the absence of
                        // a value is providing any benefit to the programmer.
                        None => println!(""),
                    }
                }
                Stmt::If(e, then_branch, else_branch) => {
                    if let Ok(v) = self.eval(&e) {
                        if is_truthy(v.as_ref()) {
                            val = self.interpret(vec![*then_branch])?;
                        } else if let Some(else_branch) = else_branch {
                            val = self.interpret(vec![*else_branch])?;
                        }
                    }
                }
                Stmt::Var(t, e) => {
                    val = self.eval(&e)?;
                    if let Some(val) = &val {
                        self.environment.borrow_mut().define(&t.lexeme, val.clone());
                    }
                }
                Stmt::Block(s) => {
                    let previous = Rc::clone(&self.environment);
                    {
                        self.environment = Rc::new(RefCell::new(Environment::new(Some(
                            Rc::clone(&self.environment),
                        ))));
                        val = self.interpret(s)?;
                    }
                    self.environment = previous;
                }
                Stmt::While(cond, s) => {
                    while is_truthy(self.eval(&cond)?.as_ref()) {
                        val = self.interpret(vec![*s.clone()])?;
                    }
                }
                Stmt::Fn(declaration) => self.environment.borrow_mut().define(
                    &declaration.as_ref().name.lexeme,
                    Value::Fn(declaration.as_ref().clone()),
                ),
                Stmt::Return(e) => {
                    val = self.eval(&e)?;
                    return Err(RuntimeErr::Return(val.clone()));
                }
            }
        }
        Ok(val)
    }

    // eval evaluates the different types of expressions that lox supports
    // using pattern matching on types. This looks OK now, but it might be a
    // good idea to take advantage of Rust's type system and define the
    // different operations as traits on our Value type.
    // TODO: come back to see this and change it to be based on traits, see how
    // that would look like.
    fn eval(&mut self, e: &Expr) -> Result<Option<Value>, RuntimeErr> {
        let mut ret: Result<Option<Value>, RuntimeErr> = Ok(None);
        match e {
            Expr::Literal(l) => {
                ret = Ok(eval_literal(l));
            }
            Expr::Grouping(g) => {
                ret = self.eval(g);
            }
            Expr::Unary(t, e) => {
                let r_val = self.eval(e)?;
                match t.kind {
                    TokenKind::Minus => {
                        ret = if let Some(Value::Number(n)) = r_val {
                            Ok(Some(Value::Number(-n)))
                        } else {
                            Err(RuntimeErr::UndefinedOperatorOnType(format!(
                                "Operator {:?} not defined on type {:?}",
                                t.kind, r_val
                            )))
                        };
                    }
                    TokenKind::Bang => {
                        ret = Ok(Some(Value::Bool(!is_truthy(r_val.as_ref()))));
                    }
                    _ => {}
                }
            }
            Expr::Binary(t, e1, e2) => {
                let left = self.eval(e1)?;
                let right = self.eval(e2)?;
                ret = match (left, right) {
                    (None, _) | (_, None) => Ok(None),
                    // FIXME: do we want some operations to be defined on null?
                    // Specifically, the equality operator seem to make sense.
                    (Some(v1), Some(v2)) => match (&v1, &v2) {
                        (Value::Number(n1), Value::Number(n2)) => match t.kind {
                            TokenKind::Slash => Ok(Some(Value::Number(n1 / n2))),
                            TokenKind::Star => Ok(Some(Value::Number(n1 * n2))),
                            TokenKind::Minus => Ok(Some(Value::Number(n1 - n2))),
                            TokenKind::Plus => Ok(Some(Value::Number(n1 + n2))),
                            TokenKind::Less => Ok(Some(Value::Bool(n1 < n2))),
                            TokenKind::LessEqual => Ok(Some(Value::Bool(n1 <= n2))),
                            TokenKind::Greater => Ok(Some(Value::Bool(n1 > n2))),
                            TokenKind::GreaterEqual => Ok(Some(Value::Bool(n1 >= n2))),
                            TokenKind::EqualEqual => Ok(Some(Value::Bool(is_equal(&v1, &v2)))),
                            TokenKind::BangEqual => Ok(Some(Value::Bool(!is_equal(&v1, &v2)))),
                            _ => Err(RuntimeErr::UndefinedOperatorOnType(format!(
                                "Operator {:?} not defined on type Number",
                                t.kind
                            ))),
                        },

                        (Value::String(s1), Value::String(s2)) => match t.kind {
                            TokenKind::Plus => Ok(Some(Value::String(format!("{}{}", s1, s2)))),
                            TokenKind::EqualEqual => Ok(Some(Value::Bool(is_equal(&v1, &v2)))),
                            TokenKind::BangEqual => Ok(Some(Value::Bool(!is_equal(&v1, &v2)))),
                            _ => Err(RuntimeErr::UndefinedOperatorOnType(format!(
                                "Operator {:?} not defined on type String",
                                t.kind
                            ))),
                        },

                        (Value::Bool(b1), Value::Bool(b2)) => match t.kind {
                            TokenKind::And => Ok(Some(Value::Bool(*b1 && *b2))),
                            TokenKind::Or => Ok(Some(Value::Bool(*b1 || *b2))),
                            TokenKind::EqualEqual => Ok(Some(Value::Bool(is_equal(&v1, &v2)))),
                            TokenKind::BangEqual => Ok(Some(Value::Bool(!is_equal(&v1, &v2)))),
                            _ => Err(RuntimeErr::UndefinedOperatorOnType(format!(
                                "Operator {:?} not defined on type Bool",
                                t.kind
                            ))),
                        },
                        // type(v1) != type(v2)
                        (x, y) => match t.kind {
                            TokenKind::EqualEqual => Ok(Some(Value::Bool(is_equal(&v1, &v2)))),
                            TokenKind::BangEqual => Ok(Some(Value::Bool(!is_equal(&v1, &v2)))),
                            _ => Err(RuntimeErr::UndefinedOperatorOnType(format!(
                                "Operator {:#?} not defined on types ({:?}, {:?})",
                                t.kind, x, y
                            ))),
                        },
                    },
                }
            }
            Expr::Logical(t, l, r) => {
                let l = self.eval(l)?;
                ret = match t.kind {
                    TokenKind::Or => {
                        if is_truthy(l.as_ref()) {
                            Ok(l)
                        } else {
                            self.eval(r)
                        }
                    }
                    _ => {
                        if !is_truthy(l.as_ref()) {
                            Ok(l)
                        } else {
                            self.eval(r)
                        }
                    }
                }
            }
            Expr::Variable(t) => {
                ret = match self.environment.borrow().get(&t) {
                    Ok(val) => Ok(Some(val)),
                    Err(err) => Err(err),
                }
            }
            Expr::Assignment(t, r_val) => {
                ret = self.eval(r_val);
                if let Ok(val) = &ret {
                    if let Err(err) = self
                        .environment
                        .borrow_mut()
                        .assign(&t.lexeme, val.clone().unwrap())
                    {
                        ret = Err(err);
                    }
                }
            }
            Expr::Call(e, _, args) => {
                if let Expr::Variable(t) = e.as_ref() {
                    let mut evaluated_args = Vec::<Value>::with_capacity(args.len());
                    for a in args {
                        if let Some(a) = self.eval(a)? {
                            evaluated_args.push(a);
                        } else {
                            return Err(RuntimeErr::AssignmentToUndefined(format!(
                                "Failed to evaluate argument {}",
                                a
                            )));
                        }
                    }
                    // FIXME: re-think the Token structure: for identifiers, we
                    // are storing two copies of the same string, as the field
                    // lexeme and inside Literal's Identifier variant.
                    let declaration = self.environment.borrow().get(t)?;

                    if let Value::Fn(declaration) = declaration {
                        let func = Function::new(declaration, Rc::clone(&self.environment));
                        if evaluated_args.len() != func.arity() {
                            return Err(RuntimeErr::CallableArityMismatch(format!(
                                "Expected {} arguments but received {}",
                                func.arity(),
                                evaluated_args.len()
                            )));
                        }
                        ret = func.call(evaluated_args);
                        // Capture the returned value if the call returned with
                        // an explicit RETURN statement.
                        if let Err(RuntimeErr::Return(val)) = ret {
                            ret = Ok(val);
                        }
                    } else {
                        return Err(RuntimeErr::NotCallable(format!("{} is not callable.", e)));
                    }
                } else {
                    return Err(RuntimeErr::NotCallable(format!("{} is not callable.", e)));
                }
            }
        }
        ret
    }
}

#[cfg(test)]
mod tests {
    use super::Value;
    #[test]
    fn test_is_truthy() {
        use super::is_truthy;
        assert_eq!(is_truthy(None), false);
        assert_eq!(is_truthy(Some(&Value::Bool(false))), false);
        assert_eq!(is_truthy(Some(&Value::Bool(true))), true);
        assert_eq!(is_truthy(Some(&Value::Number(7.0))), true);
        assert_eq!(is_truthy(Some(&Value::String("".to_owned()))), true);
    }
    #[test]
    fn test_is_equal() {
        use super::is_equal;
        assert_eq!(is_equal(&Value::Bool(false), &Value::Bool(false)), true);
        assert_eq!(is_equal(&Value::Number(7.0), &Value::Number(7.0)), true);
        assert_eq!(is_equal(&Value::Number(7.0), &Value::Number(7.0)), true);
        assert_eq!(
            is_equal(&Value::Number(7.0), &Value::String("7.0".to_owned())),
            false
        );
        assert_eq!(is_equal(&Value::Bool(false), &Value::Number(7.1)), false);
    }

    #[test]
    fn test_eval() {
        use super::{
            super::token::Token, Expr, FnDeclaration, Literal, RuntimeErr, Stmt, TokenKind,
        };
        let mut sh = super::Interpreter::new();
        assert_eq!(
            sh.interpret(vec![Stmt::Expr(Expr::Unary(
                Token::new(TokenKind::Bang, "!".to_owned(), None, 1),
                Box::new(Expr::Literal(Literal::Bool(false))),
            ))])
            .unwrap()
            .unwrap(),
            Value::Bool(true)
        );

        if let Err(RuntimeErr::UndefinedOperatorOnType(_)) = sh.eval(&Expr::Binary(
            Token::new(TokenKind::Plus, "+".to_owned(), None, 1),
            Box::new(Expr::Literal(Literal::Number(123.0))),
            Box::new(Expr::Literal(Literal::String("abc".to_owned()))),
        )) {
            assert!(true)
        } else {
            assert!(false)
        }

        assert_eq!(
            sh.eval(&Expr::Binary(
                Token::new(TokenKind::Plus, "+".to_owned(), None, 1),
                Box::new(Expr::Literal(Literal::String("123".to_owned()))),
                Box::new(Expr::Literal(Literal::String("abc".to_owned()))),
            ))
            .unwrap()
            .unwrap(),
            Value::String("123abc".to_owned())
        );
        assert_eq!(
            sh.eval(&Expr::Logical(
                Token::new(TokenKind::Or, "or".to_owned(), None, 1),
                Box::new(Expr::Literal(Literal::Bool(false))),
                Box::new(Expr::Literal(Literal::Bool(true))),
            ))
            .unwrap()
            .unwrap(),
            Value::Bool(true)
        );

        // Calling a non-function
        if let Err(RuntimeErr::NotCallable(_)) = sh.eval(&Expr::Call(
            Box::new(Expr::Literal(Literal::Number(123.0))),
            Token::new(TokenKind::RightParen, ")".to_owned(), None, 1),
            vec![],
        )) {
            assert!(true)
        } else {
            assert!(false)
        }

        // Arity mismatch
        let mut sh = super::Interpreter::new();
        sh.interpret(vec![Stmt::Fn(Box::new(FnDeclaration {
            name: Token::new(TokenKind::Identifier, "identity".to_owned(), None, 1),
            params: vec!["n".to_owned()],
            body: Stmt::Expr(Expr::Variable(Token::new(
                TokenKind::Identifier,
                "n".to_owned(),
                None,
                1,
            ))),
        }))])
        .unwrap();
        if let Err(RuntimeErr::CallableArityMismatch(_)) = sh.eval(&Expr::Call(
            Box::new(Expr::Variable(Token::new(
                TokenKind::Identifier,
                "identity".to_owned(),
                None,
                1,
            ))),
            Token::new(TokenKind::RightParen, ")".to_owned(), None, 1),
            Vec::<Box<Expr>>::with_capacity(0),
        )) {
            assert!(true)
        } else {
            assert!(false)
        }

        // A valid call to a unary function - returns correct value
        assert_eq!(
            sh.eval(&Expr::Call(
                Box::new(Expr::Variable(Token::new(
                    TokenKind::Identifier,
                    "identity".to_owned(),
                    None,
                    1,
                ))),
                Token::new(TokenKind::RightParen, ")".to_owned(), None, 1),
                vec![Box::new(Expr::Literal(Literal::Number(7.0)))],
            ))
            .unwrap()
            .unwrap(),
            Value::Number(7.0)
        );
    }
}
