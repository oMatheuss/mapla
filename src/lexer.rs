use crate::error::{Error, PositionResult, Result};
use crate::position::Position;
use crate::token::Token;
use std::iter::Peekable;
use std::str::Chars;

pub struct Lexer<'a> {
    input: &'a str,
    chars: Peekable<Chars<'a>>,
    ended: bool,
    position: Position,
}

#[derive(Debug)]
pub struct LexItem<'a> {
    token: Token<'a>,
    position: Position,
}

impl<'a> LexItem<'a> {
    #[inline]
    pub fn token(&self) -> &Token {
        &self.token
    }

    #[inline]
    pub fn position(&self) -> &Position {
        &self.position
    }

    #[inline]
    pub fn new(token: Token<'a>, position: Position) -> Self {
        Self { token, position }
    }

    #[inline]
    pub fn eof(position: Position) -> Self {
        let token = Token::Eof;
        Self { token, position }
    }

    #[inline]
    pub fn ok(self) -> Result<Self> {
        Result::Ok(self)
    }
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let chars = input.chars().peekable();
        Self {
            input,
            chars,
            ended: false,
            position: Position::new(),
        }
    }

    #[inline]
    fn next(&mut self) -> Option<char> {
        let ch = self.chars.next()?;
        self.position.next(ch);
        Some(ch)
    }

    #[inline]
    fn peek(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    #[inline]
    fn index(&mut self) -> usize {
        self.position.index()
    }

    #[inline]
    fn skip_whitespace(&mut self) -> bool {
        let is = self.peek().is_some_and(|ch| ch.is_whitespace());
        if is {
            self.next();
        }
        is
    }

    fn next_ident(&mut self) -> Result<Token<'a>> {
        let start = self.index();
        while let Some(ch) = self.peek() {
            if !ch.is_alphanumeric() && *ch != '_' {
                break;
            }
            self.next();
        }
        let end = self.index();
        let idt = Token::from_str(&self.input[start..end]);
        Ok(idt)
    }

    fn next_number(&mut self) -> Result<Token<'a>> {
        let start = self.index();
        let mut state = 0;
        while let Some(ch) = self.peek() {
            state = match (state, ch) {
                (0, '0') => 1, // final integer 0
                (0, '1'..='9') => 4,
                (1, 'x') => 2,
                (1, '0'..='9') => 4,
                (2, '0'..='9' | 'a'..='f' | 'A'..='F') => 3,
                (3, '0'..='9' | 'a'..='f' | 'A'..='F') => 3, // final hex
                (4, '0'..='9') => 4,                         // final integer
                (4, '.') => 5,
                (5, '0'..='9') => 6,
                (6, '0'..='9') => 6, // final float
                (_, _) => break,
            };
            self.next();
        }

        let slice = &self.input[start..self.index()];
        match state {
            3 => {
                let slice = &self.input[start + 2..self.index()];
                let num = u32::from_str_radix(slice, 16).with_position(self.position)?;
                Ok(Token::IntLiteral(num))
            }
            1 | 4 => {
                let num = u32::from_str_radix(slice, 10).with_position(self.position)?;
                Ok(Token::IntLiteral(num))
            }
            6 => {
                let num = slice.parse().with_position(self.position)?;
                Ok(Token::FloatLiteral(num))
            }
            _ => Error::lexical("invalid number or hexadecimal", self.position),
        }
    }

    fn next_string(&mut self) -> Result<Token<'a>> {
        self.next(); // discard quotation mark
        let start = self.index();
        while let Some(ch) = self.peek() {
            if ch.is_control() {
                return Error::lexical("control characters are not allowed", self.position);
            }
            if '"'.eq(ch) {
                break;
            }
            self.next();
        }
        let end = self.index();
        if let Some('"') = self.next() {
            let string = &self.input[start..end];
            Ok(Token::StrLiteral(string))
        } else {
            Error::lexical("unexpected end of file", self.position)
        }
    }

    fn next_symbol(&mut self) -> Result<Token<'a>> {
        let first = self.next().unwrap();
        let second = self.peek();

        let token = match (first, second) {
            ('(', _) => Token::OpenParen,
            (')', _) => Token::CloseParen,
            ('[', _) => Token::OpenBracket,
            (']', _) => Token::CloseBracket,
            ('=', Some('=')) => {
                self.next();
                Token::Equal
            }
            ('=', _) => Token::Assign,
            ('!', Some('=')) => {
                self.next();
                Token::NotEqual
            }
            ('>', Some('=')) => {
                self.next();
                Token::GreaterEqual
            }
            ('>', _) => Token::Greater,
            ('<', Some('=')) => {
                self.next();
                Token::LessEqual
            }
            ('<', _) => Token::Less,
            ('&', Some('&')) => {
                self.next();
                Token::And
            }
            ('&', _) => Token::Ampersand,
            ('|', Some('|')) => {
                self.next();
                Token::Or
            }
            ('%', _) => Token::Mod,
            ('+', Some('=')) => {
                self.next();
                Token::AddAssign
            }
            ('+', _) => Token::Add,
            ('-', Some('=')) => {
                self.next();
                Token::SubAssign
            }
            ('-', _) => Token::Sub,
            ('*', Some('=')) => {
                self.next();
                Token::MulAssign
            }
            ('*', _) => Token::Mul,
            ('/', Some('=')) => {
                self.next();
                Token::DivAssign
            }
            ('/', _) => Token::Div,
            (',', _) => Token::Comma,
            (':', _) => Token::Colon,
            (';', _) => Token::SemiColon,
            _ => Error::lexical("invalid token", self.position)?,
        };

        Ok(token)
    }

    pub fn next_token(&mut self) -> Result<LexItem<'a>> {
        while let Some(&ch) = self.peek() {
            if self.skip_whitespace() {
                continue;
            }

            let position = self.position;
            let token = match ch {
                'a'..='z' | 'A'..='Z' | '_' => self.next_ident(),
                '0'..='9' | '.' => self.next_number(),
                '"' => self.next_string(),
                _ => self.next_symbol(),
            }?;

            return LexItem::new(token, position).ok();
        }

        self.ended = true;
        LexItem::eof(self.position).ok()
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<LexItem<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.ended {
            Some(self.next_token())
        } else {
            None
        }
    }
}
