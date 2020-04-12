use std::fmt;
#[derive(Debug)]
pub enum TokenKind {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier,
    String,
    Number,

    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    EOF,
}

#[derive(Debug)]
pub enum Literal {
    Identifier(String),
    String(String),
    Number(f64),
}
#[derive(Debug)]
pub struct Token {
    kind: TokenKind,
    lexeme: String,
    literal: Option<Literal>,
    line: usize,
}
impl Token {
    pub fn new(kind: TokenKind, lexeme: String, literal: Option<Literal>, line: usize) -> Self {
        Token {
            kind,
            lexeme,
            literal,
            line,
        }
    }
}
impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} {} {:?}", self.kind, self.lexeme, self.literal)
    }
}
