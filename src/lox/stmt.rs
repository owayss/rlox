use super::expr::Expr;
use super::token::Token;

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Expr(Expr),
    Print(Expr),
    Var(Token, Expr),
}
