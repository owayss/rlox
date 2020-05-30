use super::expr::Expr;
use super::token::{Literal, Token, TokenKind, TokenKind::*};

#[derive(Debug)]
pub enum ParseErr {
    UnclosedGrouping,
    UnknownProduction,
}
pub struct Parser {
    current: usize,
    tokens: Vec<Token>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { current: 0, tokens }
    }
    pub fn parse(&mut self) -> Result<Expr, ParseErr> {
        self.expression()
    }
    fn expression(&mut self) -> Result<Expr, ParseErr> {
        self.equality()
    }
    fn equality(&mut self) -> Result<Expr, ParseErr> {
        let mut expr = self.comparison()?;
        while self.matches(&[BangEqual, EqualEqual]) {
            let operator = self.previous();
            let right = self.comparison()?;
            expr = Expr::Binary(operator, Box::new(expr), Box::new(right));
        }
        Ok(expr)
    }
    fn comparison(&mut self) -> Result<Expr, ParseErr> {
        let mut expr = self.addition()?;
        while self.matches(&[Less, LessEqual, Greater, GreaterEqual]) {
            let operator = self.previous();
            let right = self.addition()?;
            expr = Expr::Binary(operator, Box::new(expr), Box::new(right));
        }
        Ok(expr)
    }
    fn addition(&mut self) -> Result<Expr, ParseErr> {
        let mut expr = self.multiplication()?;
        while self.matches(&[Plus, Minus]) {
            let operator = self.previous();
            let right = self.multiplication()?;
            expr = Expr::Binary(operator, Box::new(expr), Box::new(right));
        }
        Ok(expr)
    }
    fn multiplication(&mut self) -> Result<Expr, ParseErr> {
        let mut expr = self.unary()?;
        while self.matches(&[Star, Slash]) {
            let operator = self.previous();
            let right = self.unary()?;
            expr = Expr::Binary(operator, Box::new(expr), Box::new(right));
        }
        Ok(expr)
    }
    fn unary(&mut self) -> Result<Expr, ParseErr> {
        if self.matches(&[Bang, Minus]) {
            let operator = self.previous();
            let right = self.unary()?;
            Ok(Expr::Unary(operator, Box::new(right)))
        } else {
            self.primary()
        }
    }
    fn primary(&mut self) -> Result<Expr, ParseErr> {
        if self.matches(&[False]) {
            Ok(Expr::Literal(Literal::Bool(false)))
        } else if self.matches(&[True]) {
            Ok(Expr::Literal(Literal::Bool(true)))
        } else if self.matches(&[Nil]) {
            Ok(Expr::Literal(Literal::Nil))
        } else if self.matches(&[String, Number]) {
            Ok(Expr::Literal(self.previous().literal.unwrap()))
        } else if self.matches(&[LeftParen]) {
            let e = self.expression()?;
            if let Err(err) = self.consume(RightParen, "Expect ')' after expression.") {
                Err(err)
            } else {
                Ok(Expr::Grouping(Box::new(e)))
            }
        } else {
            Err(self.error(
                ParseErr::UnknownProduction,
                self.peek(),
                "Unknown production",
            ))
        }
    }

    fn consume(&mut self, t: TokenKind, err: &str) -> Result<(), ParseErr> {
        if self.tokens[self.current].kind == t {
            self.advance();
            Ok(())
        } else {
            Err(self.error(ParseErr::UnclosedGrouping, self.peek(), err))
        }
    }

    fn error(&self, err: ParseErr, t: Token, msg: &str) -> ParseErr {
        if t.kind == EOF {
            super::report(t.line, "at end", msg);
        } else {
            super::report(t.line, &format!("at '{}'", t.lexeme), msg);
        }
        err
    }

    fn matches(&mut self, kinds: &[TokenKind]) -> bool {
        for t in kinds {
            if self.tokens[self.current].kind == *t {
                self.advance();
                return true;
            }
        }
        false
    }
    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }
    fn previous(&self) -> Token {
        self.tokens[self.current - 1].clone()
    }

    fn is_at_end(&self) -> bool {
        self.peek().kind == TokenKind::EOF
    }

    fn peek(&self) -> Token {
        self.tokens[self.current].clone()
    }
}
