use super::expr::Expr;
use super::token::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Expr(Expr),
    Print(Expr),
    Var(Token, Expr),
    Block(Vec<Stmt>),
}
