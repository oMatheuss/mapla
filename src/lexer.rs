use crate::error::{Error, Result};
use crate::token::Token;
use std::iter::{Enumerate, Peekable};
use std::str::Chars;

pub struct Lexer<'a> {
    input: &'a str,
    chars: Peekable<Enumerate<Chars<'a>>>,
    position: usize,
}

impl<'a> Lexer<'a> {
    pub fn from_source(input: &'a str) -> Self {
        let chars = input.chars().enumerate().peekable();
        Self {
            input,
            chars,
            position: 0,
        }
    }

    fn next_ident(&mut self) -> Result<Token<'a>> {
        let mut end = 0;
        while let Some((i, ch)) = self.chars.peek() {
            if !ch.is_alphanumeric() {
                break;
            }
            end = *i;
            self.chars.next();
        }
        end += 1;
        let token = Token::from_str(&self.input[self.position..end]);
        Ok(token)
    }

    fn next_number(&mut self) -> Result<Token<'a>> {
        let mut end = 0;
        while let Some((i, ch)) = self.chars.peek() {
            if !ch.is_numeric() {
                break;
            }
            end = *i;
            self.chars.next();
        }
        end += 1;
        let s = self.input[self.position..end].parse()?;
        Ok(Token::IntLiteral(s))
    }

    fn next_string(&mut self) -> Result<Token<'a>> {
        todo!()
    }

    fn next_symbol(&mut self) -> Result<Token<'a>> {
        let (_, symb) = self.chars.next().unwrap();
        Ok(Token::Symbol(symb))
    }

    pub fn next_token(&mut self) -> Result<Token<'a>> {
        while let Some((i, ch)) = self.chars.peek() {
            let ch = *ch;
            self.position = *i;

            if matches!(ch, 'a'..'z' | 'A'..'Z') {
                return self.next_ident();
            }

            if matches!(ch, '0'..'9') {
                return self.next_number();
            }

            if matches!(
                ch,
                '(' | ')' | '=' | '!' | '>' | '<' | '&' | '|' | '%' | '+' | '-' | '*' | '/'
            ) {
                return self.next_symbol();
            }

            if ch == '"' {
                return self.next_string();
            }

            if ch.is_whitespace() {
                self.chars.next();
                continue;
            }

            return Error::lexical();
        }

        Ok(Token::Eof)
    }

    pub fn colect_all(self) -> Result<Vec<Token<'a>>> {
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
