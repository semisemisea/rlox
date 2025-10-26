use std::{collections::HashMap, ops::Range, sync::OnceLock};

use bytes::Bytes;
use memchr::memchr;

#[derive(Debug)]
pub struct Scanner<'byte> {
    contents: &'byte Bytes,
    prev: usize,
    curr: usize,
    line: usize,
}

impl<'byte> Scanner<'byte> {
    pub fn new(bytes_ref: &'byte Bytes) -> Scanner<'byte> {
        Scanner {
            contents: bytes_ref,
            prev: 0,
            curr: 0,
            line: 1,
        }
    }

    pub fn make_str(&self, token: Token) -> &str {
        unsafe { str::from_utf8_unchecked(&self.contents[token.range()]) }
    }

    #[inline(always)]
    fn cursor_point_to_end(&self) -> bool {
        self.curr == self.contents.len()
    }

    #[inline(always)]
    fn consume(&mut self) -> u8 {
        let ret = self.contents[self.curr];
        self.curr += 1;
        ret
    }

    #[inline(always)]
    fn advance(&mut self) {
        self.curr += 1;
    }

    #[inline(always)]
    fn peek(&self) -> u8 {
        self.contents[self.curr]
    }

    #[inline(always)]
    fn peek_next(&self) -> u8 {
        self.contents[self.curr + 1]
    }

    #[inline(always)]
    fn can_match_next(&mut self, chr: u8) -> bool {
        if self.cursor_point_to_end() {
            return false;
        }
        if self.peek() == chr {
            self.curr += 1;
            true
        } else {
            false
        }
    }

    fn skip_whitespace(&mut self) {
        while !self.cursor_point_to_end() {
            match self.peek() {
                b' ' | b'\t' | b'\r' => {
                    self.consume();
                }
                b'\n' => {
                    self.line += 1;
                    self.consume();
                }
                b'/' => {
                    if self.peek_next() == b'/' {
                        self.curr = memchr(b'\n', &self.contents[self.curr..])
                            .unwrap_or(self.contents.len());
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }
    }

    pub fn scan_all(&mut self) -> Result<Vec<Token>, TokenError> {
        let mut tokens = Vec::new();
        loop {
            let next_token = self.scan_token()?;
            if matches!(next_token.token_type, TokenType::Eof) {
                break;
            }
            tokens.push(next_token);
        }
        Ok(tokens)
    }

    pub fn scan_token(&mut self) -> Result<Token, TokenError> {
        self.skip_whitespace();
        self.prev = self.curr;
        if self.cursor_point_to_end() {
            return Ok(Token::new(self, TokenType::Eof));
        }
        let tk = match self.consume() {
            b'(' => Token::new(self, TokenType::LParen),
            b')' => Token::new(self, TokenType::RParen),
            b'{' => Token::new(self, TokenType::LBrace),
            b'}' => Token::new(self, TokenType::RBrace),
            b',' => Token::new(self, TokenType::Comma),
            b'.' => Token::new(self, TokenType::Dot),
            b'-' => Token::new(self, TokenType::Minus),
            b'+' => Token::new(self, TokenType::Plus),
            b';' => Token::new(self, TokenType::Semicolon),
            b'*' => Token::new(self, TokenType::Star),
            b'/' => Token::new(self, TokenType::Slash),
            b'=' => {
                if self.can_match_next(b'=') {
                    Token::new(self, TokenType::Equal)
                } else {
                    Token::new(self, TokenType::Assign)
                }
            }
            b'!' => {
                if self.can_match_next(b'=') {
                    Token::new(self, TokenType::Inequal)
                } else {
                    Token::new(self, TokenType::Negation)
                }
            }
            b'<' => {
                if self.can_match_next(b'=') {
                    Token::new(self, TokenType::LessThanOrEqual)
                } else {
                    Token::new(self, TokenType::LessThan)
                }
            }
            b'>' => {
                if self.can_match_next(b'=') {
                    Token::new(self, TokenType::GreaterThanOrEqual)
                } else {
                    Token::new(self, TokenType::GreaterThan)
                }
            }
            b'\"' => {
                self.prev += 1;
                let Some(mut terminator) = memchr(b'\"', &self.contents[self.curr..]) else {
                    return Err(TokenError::UnterminatedString);
                };
                loop {
                    let Some(eol_pos) = memchr(b'\n', &self.contents[self.curr..]) else {
                        break;
                    };
                    if eol_pos < terminator {
                        terminator -= eol_pos + 1 - self.curr;
                        self.curr += eol_pos + 1;
                        self.line += 1;
                    } else {
                        self.curr += terminator;
                        break;
                    }
                }
                let ret = Token::new(self, TokenType::StringLiteral);
                self.curr += 1;
                ret
            }
            b'0'..=b'9' => {
                while self.peek().is_ascii_digit() {
                    self.advance();
                }
                if self.peek() == b'.' && self.peek_next().is_ascii_digit() {
                    self.advance();
                    while self.peek().is_ascii_digit() {
                        self.advance();
                    }
                }
                Token::new(self, TokenType::NumberLiteral)
            }
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                while self.peek().is_ascii_alphanumeric() || self.peek() == b'_' {
                    self.advance();
                }
                if let Some(keyword) = keyword_match().get(&self.contents[self.prev..self.curr]) {
                    Token::new(self, *keyword)
                } else {
                    Token::new(self, TokenType::Identifier)
                }
            }
            _ => Token::new(self, TokenType::Unexpected),
        };
        println!("{tk:?}");
        Ok(tk)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub token_type: TokenType,
    val: TokenValSplit,
    pub line: usize,
}

impl Default for Token {
    fn default() -> Self {
        Token::placeholder()
    }
}

impl Token {
    pub fn new(scanner: &Scanner, token_type: TokenType) -> Token {
        Token {
            token_type,
            val: (scanner.prev, scanner.curr),
            line: scanner.line,
        }
    }

    pub fn placeholder() -> Token {
        Token {
            token_type: TokenType::Eof,
            val: (0, 0),
            line: 0,
        }
    }

    pub fn range(&self) -> Range<usize> {
        self.val.0..self.val.1
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenType {
    LParen = 0,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Negation,
    Inequal,
    Assign,
    Equal,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    Identifier,
    StringLiteral,
    NumberLiteral,
    Comment,
    // Tab,
    Whitespace,
    // Newline,
    Quote,
    // Number,
    Unexpected,
    #[default]
    Eof,
    // Reserved Word
    And,
    Class,
    Else,
    False,
    For,
    Fun,
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
}

fn keyword_match() -> &'static HashMap<Bytes, TokenType> {
    static MAP: OnceLock<HashMap<Bytes, TokenType>> = OnceLock::new();
    MAP.get_or_init(|| {
        let mut m = HashMap::new();
        m.insert(Bytes::from("and"), TokenType::And);
        m.insert(Bytes::from("class"), TokenType::Class);
        m.insert(Bytes::from("else"), TokenType::Else);
        m.insert(Bytes::from("false"), TokenType::False);
        m.insert(Bytes::from("for"), TokenType::For);
        m.insert(Bytes::from("fun"), TokenType::Fun);
        m.insert(Bytes::from("if"), TokenType::If);
        m.insert(Bytes::from("nil"), TokenType::Nil);
        m.insert(Bytes::from("or"), TokenType::Or);
        m.insert(Bytes::from("print"), TokenType::Print);
        m.insert(Bytes::from("return"), TokenType::Return);
        m.insert(Bytes::from("super"), TokenType::Super);
        m.insert(Bytes::from("this"), TokenType::This);
        m.insert(Bytes::from("true"), TokenType::True);
        m.insert(Bytes::from("var"), TokenType::Var);
        m.insert(Bytes::from("while"), TokenType::While);
        m
    })
}

#[derive(Debug)]
pub enum TokenError {
    // UnexpectedToken,
    UnterminatedString,
}

pub type TokenValSplit = (usize, usize);
