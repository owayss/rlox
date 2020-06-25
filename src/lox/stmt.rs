use super::expr::Expr;
use super::token::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Expr(Expr),
    Print(Expr),
    Var(Token, Expr),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
    Block(Vec<Stmt>),
    Fn(Box<FnDeclaration>),
    Return(Expr),
}

#[derive(Debug, PartialEq, Clone)]
pub struct FnDeclaration {
    pub name: Token,
    pub params: Vec<String>,
    pub body: Stmt,
}
