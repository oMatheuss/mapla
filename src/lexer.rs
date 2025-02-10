use crate::error::{Error, Result};
use crate::token::Token;
use std::iter::Peekable;
use std::str::Chars;

pub struct Lexer<'a> {
    input: &'a str,
    chars: Peekable<Chars<'a>>,
    pos: usize,
    col: usize,
    line: usize,
}

impl<'a> Lexer<'a> {
    pub fn from_source(input: &'a str) -> Self {
        let chars = input.chars().peekable();
        Self {
            input,
            chars,
            pos: 0,
            col: 1,
            line: 1,
        }
    }

    #[inline]
    fn next(&mut self) -> Option<char> {
        let ch = self.chars.next()?;
        self.pos += ch.len_utf8();

        if matches!(ch, '\n') {
            self.line += 1;
        }

        if matches!(ch, '\n' | '\r') {
            self.col = 1
        } else {
            self.col += 1
        }

        Some(ch)
    }

    #[inline]
    fn peek(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    fn next_ident(&mut self) -> Result<Token<'a>> {
        let start = self.pos;
        while let Some(ch) = self.peek() {
            if !ch.is_alphanumeric() {
                break;
            }
            self.next();
        }
        let len = self.pos;
        let idt = Token::from_str(&self.input[start..len]);
        Ok(idt)
    }

    fn next_number(&mut self) -> Result<Token<'a>> {
        let start = self.pos;
        while let Some(ch) = self.peek() {
            if !ch.is_numeric() {
                break;
            }
            self.next();
        }
        let len = self.pos;
        let num = self.input[start..len].parse()?;
        Ok(Token::IntLiteral(num))
    }

    fn next_string(&mut self) -> Result<Token<'a>> {
        todo!()
    }

    fn next_symbol(&mut self) -> Result<Token<'a>> {
        let ch = self.next().unwrap();
        Ok(Token::Symbol(ch))
    }

    pub fn next_token(&mut self) -> Result<Token<'a>> {
        while let Some(ch) = self.peek() {
            return match ch {
                'a'..'z' | 'A'..'Z' => self.next_ident(),
                '0'..'9' => self.next_number(),
                '(' | ')' | '=' | '!' | '>' | '<' | '&' | '|' | '%' | '+' | '-' | '*' | '/' => {
                    self.next_symbol()
                }
                '"' => self.next_string(),
                _ if ch.is_whitespace() => {
                    self.next();
                    continue;
                }
                _ => Error::lexical()?,
            };
        }

        Ok(Token::Eof)
    }

    pub fn collect_all(self) -> Result<Vec<Token<'a>>> {
        self.collect::<Result<Vec<_>>>()
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok(Token::Eof) => None,
            result => Some(result),
        }
    }
}
