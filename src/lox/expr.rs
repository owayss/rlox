use super::token;
use std::fmt;

pub enum Expr {
    Literal(token::Literal),
    Unary(token::Token, Box<Expr>),
    Binary(Box<Expr>, token::Token, Box<Expr>),
    Grouping(Box<Expr>),
}
// Lisp-like printer for AST
impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Literal(l) => write!(f, "{}", l),
            Expr::Unary(t, e) => write!(f, "({} {})", t, e),
            Expr::Binary(e1, t, e2) => write!(f, "({} {} {})", t, e1, e2),
            Expr::Grouping(e) => write!(f, "({})", e),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{
        token::{Literal, Token, TokenKind},
        Expr,
    };
    #[test]
    fn test_ast_printer() {
        let sub_e = Expr::Unary(
            Token::new(TokenKind::Minus, "-".to_owned(), None, 1),
            Box::new(Expr::Literal(Literal::Number(123.0))),
        );
        let e = Expr::Binary(
            Box::new(sub_e),
            Token::new(TokenKind::Star, "*".to_owned(), None, 1),
            Box::new(Expr::Grouping(Box::new(Expr::Literal(Literal::Number(
                45.67,
            ))))),
        );
        assert_eq!("(* (- 123) (group 45.67))", e.to_string())
    }
}
