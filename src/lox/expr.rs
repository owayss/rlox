use super::token;
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Literal(token::Literal),
    Unary(token::Token, Box<Expr>),
    Binary(token::Token, Box<Expr>, Box<Expr>),
    Logical(token::Token, Box<Expr>, Box<Expr>),
    Grouping(Box<Expr>),
    Variable(token::Token),
    Assignment(token::Token, Box<Expr>),
}
// Lisp-like printer for AST
impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Literal(l) => write!(f, "{}", l),
            Expr::Unary(t, e) => write!(f, "({} {})", t, e),
            Expr::Binary(t, e1, e2) => write!(f, "({} {} {})", t, e1, e2),
            Expr::Logical(t, e1, e2) => write!(f, "({} {} {})", t, e1, e2),
            Expr::Grouping(e) => write!(f, "(group {})", e),
            Expr::Variable(t) => write!(f, "{}", t),
            Expr::Assignment(t, val) => write!(f, "{} {}", t, val),
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
        assert_eq!(
            "(* (- 123) (group 45.67))",
            Expr::Binary(
                Token::new(TokenKind::Star, "*".to_owned(), None, 1),
                Box::new(Expr::Unary(
                    Token::new(TokenKind::Minus, "-".to_owned(), None, 1),
                    Box::new(Expr::Literal(Literal::Number(123.0))),
                )),
                Box::new(Expr::Grouping(Box::new(Expr::Literal(Literal::Number(
                    45.67,
                ))))),
            )
            .to_string()
        );

        assert_eq!(
            "(+ 123 abc)",
            Expr::Binary(
                Token::new(TokenKind::Plus, "+".to_owned(), None, 1),
                Box::new(Expr::Literal(Literal::String("123".to_owned()))),
                Box::new(Expr::Literal(Literal::String("abc".to_owned()))),
            )
            .to_string()
        );
    }
}
