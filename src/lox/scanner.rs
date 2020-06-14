use super::token::{Literal, Token, TokenKind};
pub struct Scanner {
    source: Vec<char>,
    pub tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
    had_error: bool,
}
impl Scanner {
    pub fn new(source: &str) -> Self {
        Scanner {
            source: source.chars().collect(),
            tokens: Vec::<Token>::new(),
            start: 0,
            current: 0,
            line: 1,
            had_error: false,
        }
    }

    pub fn scan_tokens(&mut self) -> Vec<Token> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }
        self.tokens
            .push(Token::new(TokenKind::EOF, "".to_owned(), None, self.line));
        self.tokens.clone()
    }

    fn scan_token(&mut self) {
        let c: char = self.advance();
        match c {
            '(' => self.add_token(TokenKind::LeftParen, None),
            ')' => self.add_token(TokenKind::RightParen, None),
            '{' => self.add_token(TokenKind::LeftBrace, None),
            '}' => self.add_token(TokenKind::RightBrace, None),
            ',' => self.add_token(TokenKind::Comma, None),
            '.' => self.add_token(TokenKind::Dot, None),
            '-' => self.add_token(TokenKind::Minus, None),
            '+' => self.add_token(TokenKind::Plus, None),
            ';' => self.add_token(TokenKind::Semicolon, None),
            '*' => self.add_token(TokenKind::Star, None),

            '!' => {
                let t = if self.expected('=') {
                    TokenKind::BangEqual
                } else {
                    TokenKind::Bang
                };
                self.add_token(t, None);
            }
            '=' => {
                let t = if self.expected('=') {
                    TokenKind::EqualEqual
                } else {
                    TokenKind::Equal
                };
                self.add_token(t, None);
            }
            '>' => {
                let t = if self.expected('=') {
                    TokenKind::GreaterEqual
                } else {
                    TokenKind::Greater
                };
                self.add_token(t, None);
            }
            '<' => {
                let t = if self.expected('=') {
                    TokenKind::LessEqual
                } else {
                    TokenKind::Less
                };
                self.add_token(t, None);
            }
            '/' => {
                if self.expected('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(TokenKind::Slash, None);
                };
            }

            '"' => self.string(),

            ' ' | '\r' | '\t' => {}
            '\n' => {
                self.line += 1;
            }
            // We could have used range matching here, but having
            // the functions is_digit(), is_alpha(), and so on is more
            // handy as they can be used inside other functions (in loop
            // conditions for instance).
            c => {
                if self.is_digit(c) {
                    self.number()
                } else if self.is_alpha(c) {
                    self.identifier()
                } else {
                    self.had_error = true;
                    super::error(self.line, &format!("Unexpected character '{}'", c))
                }
            }
        }
    }

    fn string(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }
        if self.is_at_end() {
            self.had_error = true;
            super::error(self.line, "Unterminated string");
            return;
        }
        // The closing '""
        self.advance();
        // Trim surrounding quotes.
        let val: String = self.source[self.start + 1..self.current - 1]
            .iter()
            .collect();
        self.add_token(TokenKind::String, Some(Literal::String(val)));
    }
    fn number(&mut self) {
        while self.is_digit(self.peek()) {
            self.current += 1;
        }
        if self.peek() == '.' && self.is_digit(self.peek_next()) {
            self.current += 1;
            while self.is_digit(self.peek()) {
                self.current += 1;
            }
        }
        let val: f64 = self.source[self.start..self.current]
            .iter()
            .collect::<String>()
            .parse()
            .unwrap();
        self.add_token(TokenKind::Number, Some(Literal::Number(val)));
    }

    fn identifier(&mut self) {
        while self.is_alphanumeric(self.peek()) {
            self.current += 1;
        }
        let val: String = self.source[self.start..self.current].iter().collect();
        if let Some(keyword) = self.reserved_keyword(&val) {
            self.add_token(keyword, None);
        } else {
            self.add_token(TokenKind::Identifier, Some(Literal::Identifier(val)));
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }
    fn expected(&mut self, c: char) -> bool {
        if self.is_at_end() || self.source[self.current] != c {
            false
        } else {
            self.current += 1;
            true
        }
    }
    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.source[self.current]
        }
    }
    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            '\0'
        } else {
            self.source[self.current + 1]
        }
    }
    fn advance(&mut self) -> char {
        self.current += 1;
        self.source[self.current - 1]
    }

    fn reserved_keyword(&self, identifier: &str) -> Option<TokenKind> {
        match identifier {
            "and" => Some(TokenKind::And),
            "class" => Some(TokenKind::Class),
            "else" => Some(TokenKind::Else),
            "false" => Some(TokenKind::False),
            "for" => Some(TokenKind::For),
            "fun" => Some(TokenKind::Fun),
            "if" => Some(TokenKind::If),
            "nil" => Some(TokenKind::Nil),
            "or" => Some(TokenKind::Or),
            "print" => Some(TokenKind::Print),
            "return" => Some(TokenKind::Return),
            "super" => Some(TokenKind::Super),
            "this" => Some(TokenKind::This),
            "true" => Some(TokenKind::True),
            "var" => Some(TokenKind::Var),
            "while" => Some(TokenKind::While),
            _ => None,
        }
    }
    fn is_digit(&self, c: char) -> bool {
        c >= '0' && c <= '9'
    }
    fn is_alpha(&self, c: char) -> bool {
        (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
    }
    fn is_alphanumeric(&self, c: char) -> bool {
        self.is_alpha(c) || self.is_digit(c)
    }

    fn add_token(&mut self, kind: TokenKind, literal: Option<Literal>) {
        let text: String = self.source[self.start..self.current].iter().collect();
        self.tokens.push(Token::new(kind, text, literal, self.line));
    }
}
