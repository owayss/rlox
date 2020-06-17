use super::expr::Expr;
use super::token::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Expr(Expr),
    Print(Expr),
    Var(Token, Expr),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    Block(Vec<Stmt>),
}
