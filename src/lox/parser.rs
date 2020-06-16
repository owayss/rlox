use super::expr::Expr;
use super::stmt::Stmt;
use super::token::{Literal, Token, TokenKind, TokenKind::*};

#[derive(Debug)]
pub enum ParseErr {
    MissingLiteral,
    UnclosedGrouping,
    UnknownProduction,
    InvalidAssignmentTarget,
}
pub struct Parser {
    current: usize,
    tokens: Vec<Token>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { current: 0, tokens }
    }
    pub fn parse(&mut self) -> Result<Vec<Stmt>, ParseErr> {
        let mut stmts = Vec::<Stmt>::new();
        while !self.is_at_end() {
            match self.declaration() {
                Ok(stmt) => {
                    stmts.push(stmt);
                }
                Err(err) => {
                    self.error(err, self.tokens[self.current].clone(), "");
                }
            }
        }
        Ok(stmts)
    }
    fn expression(&mut self) -> Result<Expr, ParseErr> {
        self.assignment()
    }
    fn declaration(&mut self) -> Result<Stmt, ParseErr> {
        let res = if self.matches(&[Var]) {
            self.var_declaration()
        } else {
            self.statement()
        };
        if let Err(_) = res {
            self.synchronize();
        }
        res
    }
    fn var_declaration(&mut self) -> Result<Stmt, ParseErr> {
        let t = self.consume(Identifier, "Expect variable name.")?;
        let initializer = if self.matches(&[Equal]) {
            self.expression()?
        } else {
            Expr::Literal(Literal::Nil)
        };
        self.consume(Semicolon, "Expect ';' after variable declaration.")?;
        Ok(Stmt::Var(t, initializer))
    }
    fn statement(&mut self) -> Result<Stmt, ParseErr> {
        if self.matches(&[Print]) {
            return self.print_stmt();
        }
        if self.matches(&[LeftBrace]) {
            return Ok(Stmt::Block(self.block()?));
        }
        self.expr_stmt()
    }
    fn block(&mut self) -> Result<Vec<Stmt>, ParseErr> {
        let mut stmts: Vec<Stmt> = Vec::new();
        while !self.is_at_end() && self.peek().kind != RightBrace {
            match self.declaration() {
                Ok(stmt) => {
                    stmts.push(stmt);
                }
                Err(err) => {
                    self.error(err, self.tokens[self.current].clone(), "");
                }
            }
        }
        self.consume(RightBrace, "Expect closing right bracket '}' after block.")?;
        Ok(stmts)
    }
    fn print_stmt(&mut self) -> Result<Stmt, ParseErr> {
        let e = self.expression();
        self.consume(Semicolon, "Expect ';' after value.")?;
        match e {
            Ok(e) => Ok(Stmt::Print(e)),
            Err(err) => Err(err),
        }
    }
    fn expr_stmt(&mut self) -> Result<Stmt, ParseErr> {
        let e = self.expression();
        self.consume(Semicolon, "Expect ';' after value.")?;
        match e {
            Ok(e) => Ok(Stmt::Expr(e)),
            Err(err) => Err(err),
        }
    }
    fn assignment(&mut self) -> Result<Expr, ParseErr> {
        let l_val = self.equality()?;
        if self.matches(&[Equal]) {
            let target = self.previous();
            let e = self.expression()?;
            if let Expr::Variable(t) = l_val {
                return Ok(Expr::Assignment(t, Box::new(e)));
            } else {
                return Err(self.error(
                    ParseErr::InvalidAssignmentTarget,
                    target,
                    "Invalid assignment target.",
                ));
            }
        }
        Ok(l_val)
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
            match self.previous().literal {
                Some(l) => Ok(Expr::Literal(l)),
                None => Err(ParseErr::MissingLiteral),
            }
        } else if self.matches(&[LeftParen]) {
            let e = self.expression()?;
            if let Err(err) = self.consume(RightParen, "Expect ')' after expression.") {
                Err(err)
            } else {
                Ok(Expr::Grouping(Box::new(e)))
            }
        } else if self.matches(&[Identifier]) {
            Ok(Expr::Variable(self.previous()))
        } else {
            Err(self.error(
                ParseErr::UnknownProduction,
                self.peek(),
                "Unknown production",
            ))
        }
    }

    fn consume(&mut self, t: TokenKind, err: &str) -> Result<Token, ParseErr> {
        let token = self.tokens[self.current].clone();
        if token.kind == t {
            self.advance();
            Ok(token)
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

    fn synchronize(&mut self) {
        self.advance();
        while !self.is_at_end() {
            if self.previous().kind == Semicolon {
                return;
            }
            match self.peek().kind {
                Class | Fun | Var | For | If | While | Print | Return => return,
                _ => {}
            }

            self.advance();
        }
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

#[cfg(test)]
mod tests {
    use super::{Expr, Literal, Parser, Stmt, Token, TokenKind};
    #[test]
    fn test_statement() {
        assert_eq!(
            Parser::new(vec![
                Token::new(TokenKind::Var, "var".to_owned(), None, 1),
                Token::new(TokenKind::Identifier, "x".to_owned(), None, 1),
                Token::new(TokenKind::Equal, "=".to_owned(), None, 1),
                Token::new(
                    TokenKind::Number,
                    "7.0".to_owned(),
                    Some(Literal::Number(7.0)),
                    1
                ),
                Token::new(TokenKind::Semicolon, ";".to_owned(), None, 1),
                Token::new(TokenKind::EOF, "".to_owned(), None, 1),
            ])
            .parse()
            .unwrap(),
            vec![Stmt::Var(
                Token::new(TokenKind::Identifier, "x".to_owned(), None, 1),
                Expr::Literal(Literal::Number(7.0))
            )]
        );
    }
}
