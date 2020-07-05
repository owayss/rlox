use super::expr::Expr;
use super::stmt::{FnDeclaration, Stmt};
use super::token::{Literal, Token, TokenKind, TokenKind::*};

#[derive(Debug)]
pub enum ParseErr {
    MissingLiteral,
    UnclosedGrouping,
    UnknownProduction,
    InvalidAssignmentTarget,
    FuncMaxArgs,
}
pub struct Parser {
    current: usize,
    tokens: Vec<Token>,
}

impl Parser {
    // Maximum number of arguments a function is allowed to have.
    const MAX_ARGS: usize = 255;

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
        let res = if self.matches(&[Fn]) {
            self.fn_declaration()
        } else if self.matches(&[Var]) {
            self.var_declaration()
        } else {
            self.statement()
        };
        if let Err(_) = res {
            self.synchronize();
        }
        res
    }
    fn fn_declaration(&mut self) -> Result<Stmt, ParseErr> {
        let t = self.consume(Identifier, "Expect function name.")?;
        self.consume(LeftParen, "Expect '(' after function name in declaration.")?;
        let params: Vec<std::string::String> = if self.matches(&[RightParen]) {
            Vec::with_capacity(0)
        } else {
            let mut params: Vec<std::string::String> = vec![
                self.consume(Identifier, "Expect identifier as function parameter")?
                    .lexeme,
            ];
            loop {
                if !self.matches(&[Comma]) || params.len() >= Parser::MAX_ARGS {
                    break;
                }
                params.push(
                    self.consume(Identifier, "Expect identifier as function parameter")?
                        .lexeme,
                );
            }
            self.consume(RightParen, "Expect ')' after function declaration")?;
            params
        };
        self.consume(LeftBrace, "Expect '{' before function definition")?;
        Ok(Stmt::Fn(Box::new(FnDeclaration {
            name: t,
            params: params,
            body: Stmt::Block(self.block()?),
        })))
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
    fn if_stmt(&mut self) -> Result<Stmt, ParseErr> {
        self.consume(LeftParen, "Expect '(' after conditional if")?;
        let e = self.expression()?;
        self.consume(RightParen, "Expect ')' after condition")?;
        let then_branch = self.declaration()?;
        if self.matches(&[Else]) {
            let else_branch = self.declaration()?;
            Ok(Stmt::If(
                e,
                Box::new(then_branch),
                Some(Box::new(else_branch)),
            ))
        } else {
            Ok(Stmt::If(e, Box::new(then_branch), None))
        }
    }
    fn while_stmt(&mut self) -> Result<Stmt, ParseErr> {
        self.consume(LeftParen, "Expect '(' after while")?;
        let cond = self.expression()?;
        self.consume(RightParen, "Expect ')' after condition")?;
        let body = self.declaration()?;
        Ok(Stmt::While(cond, Box::new(body)))
    }
    // for_loop de-construct's the for loop's syntactic sugar to return an
    // equivalent block using a while construct.
    fn for_loop(&mut self) -> Result<Stmt, ParseErr> {
        self.consume(LeftParen, "Expect '(' after for")?;
        let initializer: Option<Stmt> = if !self.matches(&[Semicolon]) {
            Some(self.declaration()?)
        } else {
            None
        };

        let cond = self.expression()?;
        let increment: Option<Stmt> = if self.matches(&[Semicolon]) {
            Some(Stmt::Expr(self.expression()?))
        } else {
            None
        };
        self.consume(RightParen, "Expect ')' after for loop definition")?;

        let body = if let Some(increment) = increment {
            Stmt::Block(vec![self.declaration()?, increment])
        } else {
            self.declaration()?
        };
        // de-constructed while loop
        if let Some(initializer) = initializer {
            Ok(Stmt::Block(vec![
                initializer,
                Stmt::While(cond, Box::new(body)),
            ]))
        } else {
            Ok(Stmt::While(cond, Box::new(body)))
        }
    }

    fn statement(&mut self) -> Result<Stmt, ParseErr> {
        if self.matches(&[Print]) {
            return self.print_stmt();
        }
        if self.matches(&[If]) {
            return self.if_stmt();
        }
        if self.matches(&[LeftBrace]) {
            return Ok(Stmt::Block(self.block()?));
        }
        if self.matches(&[While]) {
            return self.while_stmt();
        }
        if self.matches(&[For]) {
            return self.for_loop();
        }
        if self.matches(&[Return]) {
            return self.return_stmt();
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
    fn return_stmt(&mut self) -> Result<Stmt, ParseErr> {
        let e = self.expression();
        self.consume(Semicolon, "Expect ';' after value.")?;
        match e {
            Ok(e) => Ok(Stmt::Return(e)),
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
        let l_val = self.or()?;
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
    fn or(&mut self) -> Result<Expr, ParseErr> {
        let mut expr = self.and()?;
        while self.matches(&[Or]) {
            let operator = self.previous();
            let right = self.and()?;
            expr = Expr::Logical(operator, Box::new(expr), Box::new(right));
        }
        Ok(expr)
    }
    fn and(&mut self) -> Result<Expr, ParseErr> {
        let mut expr = self.equality()?;
        while self.matches(&[And]) {
            let operator = self.previous();
            let right = self.equality()?;
            expr = Expr::Logical(operator, Box::new(expr), Box::new(right));
        }
        Ok(expr)
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
            self.call()
        }
    }

    fn call(&mut self) -> Result<Expr, ParseErr> {
        let mut expr = self.primary()?;
        while self.matches(&[LeftParen]) {
            expr = self.finish_call(expr)?;
        }
        Ok(expr)
    }

    fn finish_call(&mut self, e: Expr) -> Result<Expr, ParseErr> {
        let func_call = if self.matches(&[RightParen]) {
            Ok(Expr::Call(Box::new(e), self.previous(), vec![]))
        } else {
            let mut args: Vec<Box<Expr>> = vec![];
            args.push(Box::new(self.expression()?));
            let ret = loop {
                if !self.matches(&[Comma]) {
                    break Ok(Expr::Call(Box::new(e), self.peek(), args));
                }
                if args.len() >= Parser::MAX_ARGS {
                    break Err(ParseErr::FuncMaxArgs);
                }
                args.push(Box::new(self.expression()?));
            };
            self.consume(RightParen, "Expect ')' after function call")?;
            ret
        };
        func_call
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
                Class | Fn | Var | For | If | While | Print | Return => return,
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
                Token::new(TokenKind::Var, "var".to_owned(), None, 1, 1),
                Token::new(TokenKind::Identifier, "x".to_owned(), None, 1, 2),
                Token::new(TokenKind::Equal, "=".to_owned(), None, 1, 3),
                Token::new(
                    TokenKind::Number,
                    "7.0".to_owned(),
                    Some(Literal::Number(7.0)),
                    1,
                    4
                ),
                Token::new(TokenKind::Semicolon, ";".to_owned(), None, 1, 4),
                Token::new(TokenKind::EOF, "".to_owned(), None, 1, 5),
            ])
            .parse()
            .unwrap(),
            vec![Stmt::Var(
                Token::new(TokenKind::Identifier, "x".to_owned(), None, 1, 2),
                Expr::Literal(Literal::Number(7.0))
            )]
        );
        assert_eq!(
            Parser::new(vec![
                Token::new(TokenKind::If, "if".to_owned(), None, 1, 1),
                Token::new(TokenKind::LeftParen, "(".to_owned(), None, 1, 2),
                Token::new(TokenKind::False, "false".to_owned(), None, 1, 3),
                Token::new(TokenKind::RightParen, ")".to_owned(), None, 1, 4),
                Token::new(TokenKind::Print, "print".to_owned(), None, 1, 5),
                Token::new(
                    TokenKind::Number,
                    "0.0".to_owned(),
                    Some(Literal::Number(0.0)),
                    1,
                    6
                ),
                Token::new(TokenKind::Semicolon, ";".to_owned(), None, 1, 7),
                Token::new(TokenKind::Else, "else".to_owned(), None, 1, 8),
                Token::new(TokenKind::Print, "print".to_owned(), None, 1, 9),
                Token::new(
                    TokenKind::Number,
                    "1.0".to_owned(),
                    Some(Literal::Number(1.0)),
                    1,
                    10
                ),
                Token::new(TokenKind::Semicolon, ";".to_owned(), None, 1, 11),
                Token::new(TokenKind::EOF, "".to_owned(), None, 1, 12),
            ])
            .parse()
            .unwrap(),
            vec![Stmt::If(
                Expr::Literal(Literal::Bool(false)),
                Box::new(Stmt::Print(Expr::Literal(Literal::Number(0.0)))),
                Some(Box::new(Stmt::Print(Expr::Literal(Literal::Number(1.0)))))
            )]
        );
        assert_eq!(
            Parser::new(vec![
                Token::new(TokenKind::While, "while".to_owned(), None, 1, 1),
                Token::new(TokenKind::LeftParen, "(".to_owned(), None, 1, 2),
                Token::new(
                    TokenKind::Identifier,
                    "i".to_owned(),
                    Some(Literal::Identifier("i".to_owned())),
                    1,
                    3
                ),
                Token::new(TokenKind::Less, "<".to_owned(), None, 1, 4),
                Token::new(
                    TokenKind::Number,
                    "10.0".to_owned(),
                    Some(Literal::Number(10.0)),
                    1,
                    4
                ),
                Token::new(TokenKind::RightParen, ")".to_owned(), None, 1, 5),
                Token::new(TokenKind::Print, "print".to_owned(), None, 1, 6),
                Token::new(
                    TokenKind::Identifier,
                    "i".to_owned(),
                    Some(Literal::Identifier("i".to_owned())),
                    1,
                    7
                ),
                Token::new(TokenKind::Semicolon, ";".to_owned(), None, 1, 8),
                Token::new(TokenKind::EOF, "".to_owned(), None, 1, 9),
            ])
            .parse()
            .unwrap(),
            vec![Stmt::While(
                Expr::Binary(
                    Token::new(TokenKind::Less, "<".to_owned(), None, 1, 4),
                    Box::new(Expr::Variable(Token::new(
                        TokenKind::Identifier,
                        "i".to_owned(),
                        Some(Literal::Identifier("i".to_owned())),
                        1,
                        3
                    ))),
                    Box::new(Expr::Literal(Literal::Number(10.0))),
                ),
                Box::new(Stmt::Print(Expr::Variable(Token::new(
                    TokenKind::Identifier,
                    "i".to_owned(),
                    Some(Literal::Identifier("i".to_owned())),
                    1,
                    7
                )))),
            )]
        );
        assert_eq!(
            Parser::new(vec![
                Token::new(TokenKind::For, "for".to_owned(), None, 1, 1),
                Token::new(TokenKind::LeftParen, "(".to_owned(), None, 1, 2),
                Token::new(TokenKind::Semicolon, ";".to_owned(), None, 1, 3),
                Token::new(
                    TokenKind::Identifier,
                    "i".to_owned(),
                    Some(Literal::Identifier("i".to_owned())),
                    1,
                    4
                ),
                Token::new(TokenKind::Less, "<".to_owned(), None, 1, 5),
                Token::new(
                    TokenKind::Number,
                    "10.0".to_owned(),
                    Some(Literal::Number(10.0)),
                    1,
                    6
                ),
                Token::new(TokenKind::RightParen, ")".to_owned(), None, 1, 7),
                Token::new(TokenKind::Print, "print".to_owned(), None, 1, 8),
                Token::new(
                    TokenKind::Identifier,
                    "i".to_owned(),
                    Some(Literal::Identifier("i".to_owned())),
                    1,
                    9
                ),
                Token::new(TokenKind::Semicolon, ";".to_owned(), None, 1, 10),
                Token::new(TokenKind::EOF, "".to_owned(), None, 1, 11),
            ])
            .parse()
            .unwrap(),
            vec![Stmt::While(
                Expr::Binary(
                    Token::new(TokenKind::Less, "<".to_owned(), None, 1, 5),
                    Box::new(Expr::Variable(Token::new(
                        TokenKind::Identifier,
                        "i".to_owned(),
                        Some(Literal::Identifier("i".to_owned())),
                        1,
                        4
                    ))),
                    Box::new(Expr::Literal(Literal::Number(10.0))),
                ),
                Box::new(Stmt::Print(Expr::Variable(Token::new(
                    TokenKind::Identifier,
                    "i".to_owned(),
                    Some(Literal::Identifier("i".to_owned())),
                    1,
                    9
                )))),
            )]
        );
    }
}
