use crate::error::{Error, Result};
use crate::position::Position;
use crate::token::Token;
use std::iter::Peekable;
use std::str::Chars;

pub struct Lexer<'a> {
    input: &'a str,
    chars: Peekable<Chars<'a>>,
    position: Position,
}

impl<'a> Lexer<'a> {
    pub fn from_source(input: &'a str) -> Self {
        let chars = input.chars().peekable();
        Self {
            input,
            chars,
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

    fn next_ident(&mut self) -> Result<Token<'a>> {
        let start = self.index();
        while let Some(ch) = self.peek() {
            if !ch.is_alphanumeric() {
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
        let mut state = 1;
        while let Some(ch) = self.peek() {
            match (state, ch) {
                (1, '0'..='9') => {}
                (1, '.') => state = 2,
                (2, '0'..='9') => {}
                (_, _) => break,
            }
            self.next();
        }
        let end = self.index();
        if state == 1 {
            let num = self.input[start..end].parse()?;
            Ok(Token::IntLiteral(num))
        } else {
            let num = self.input[start..end].parse()?;
            Ok(Token::FloatLiteral(num))
        }
    }

    fn next_string(&mut self) -> Result<Token<'a>> {
        todo!()
    }

    fn next_symbol(&mut self) -> Result<Token<'a>> {
        let first = self.next().unwrap();
        let second = self.peek();

        let token = match (first, second) {
            ('(', _) => Token::OpenParen,
            (')', _) => Token::CloseParen,
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

    pub fn next_token(&mut self) -> Result<Token<'a>> {
        while let Some(ch) = self.peek() {
            return match ch {
                'a'..'z' | 'A'..'Z' => self.next_ident(),
                '0'..='9' | '.' => self.next_number(),
                '(' | ')' | '=' | '!' | '>' | '<' | '&' | '|' | '%' | '+' | '-' | '*' | '/'
                | ':' | ',' | ';' => self.next_symbol(),
                '"' => self.next_string(),
                _ if ch.is_whitespace() => {
                    self.next();
                    continue;
                }
                _ => Error::lexical("invalid token", self.position)?,
            };
        }

        Ok(Token::Eof)
    }

    pub fn collect(mut self) -> Result<Vec<Token<'a>>> {
        let mut items = Vec::new();
        loop {
            let next = self.next_token()?;

            if let Token::Eof = next {
                items.push(next);
                break Ok(items);
            }

            items.push(next);
        }
    }
}
