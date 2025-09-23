use crate::error::{Error, PositionResult, Result};
use crate::position::Position;
use crate::source::Source;
use crate::token::{Token, TokenInfo};
use std::iter::Peekable;
use std::str::Chars;

pub struct Lexer<'a> {
    src: &'a str,
    chs: Peekable<Chars<'a>>,
    pos: Position<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a Source) -> Self {
        Self {
            src: input.src.as_str(),
            chs: input.src.chars().peekable(),
            pos: Position::new(input.file),
        }
    }

    #[inline]
    fn next(&mut self) -> Option<char> {
        let ch = self.chs.next()?;
        self.pos.next(ch);
        Some(ch)
    }

    #[inline]
    fn peek(&mut self) -> Option<&char> {
        self.chs.peek()
    }

    #[inline]
    fn index(&mut self) -> usize {
        self.pos.index()
    }

    #[inline]
    fn skip_whitespace(&mut self) -> bool {
        if let Some(ch) = self.peek() {
            if ch.is_whitespace() {
                self.next();
                return true;
            }
        }
        return false;
    }

    #[inline]
    fn skip_comment(&mut self) -> bool {
        if let Some('#') = self.peek() {
            while let Some(ch) = self.next() {
                if ch == '\n' {
                    break;
                }
            }
            true
        } else {
            false
        }
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
        let idt = Token::from_str(&self.src[start..end]);
        Ok(idt)
    }

    fn next_number(&mut self) -> Result<Token<'a>> {
        let num_pos = self.pos;
        let start = self.index();
        let mut state = 0;
        while let Some(ch) = self.peek() {
            state = match (state, ch) {
                (0, '0') => 1, // final integer 0
                (0, '1'..='9') => 4,
                (1, 'x') => 2,
                (1, '.') => 5,
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

        let slice = &self.src[start..self.index()];
        match state {
            3 => {
                let slice = &self.src[start + 2..self.index()];
                let num = u32::from_str_radix(slice, 16).with_position(num_pos)?;
                Ok(Token::IntLiteral(num))
            }
            1 | 4 => {
                let num = u32::from_str_radix(slice, 10).with_position(num_pos)?;
                Ok(Token::IntLiteral(num))
            }
            6 => {
                let num = slice.parse().with_position(num_pos)?;
                Ok(Token::FloatLiteral(num))
            }
            _ => Error::lexical("invalid number or hexadecimal", num_pos),
        }
    }

    fn next_string(&mut self) -> Result<Token<'a>> {
        let str_pos = self.pos;
        self.next(); // discard quotation mark
        let start = self.index();
        while let Some(ch) = self.next() {
            match ch {
                '"' => {
                    let end = self.index() - 1;
                    let string = &self.src[start..end];
                    return Ok(Token::StrLiteral(string));
                }
                '\\' if matches!(self.peek(), Some('"')) => {
                    self.next();
                }
                _ if ch.is_control() => {
                    return Error::lexical("control characters are not allowed", str_pos)
                }
                _ => (),
            }
        }
        Error::lexical("expected trailing `\"`", str_pos)
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
            ('!', _) => Token::Not,
            ('>', Some('=')) => {
                self.next();
                Token::GreaterEqual
            }
            ('>', Some('>')) => {
                self.next();
                Token::Shr
            }
            ('>', _) => Token::Greater,
            ('<', Some('=')) => {
                self.next();
                Token::LessEqual
            }
            ('<', Some('<')) => {
                self.next();
                Token::Shl
            }
            ('<', _) => Token::Less,
            ('&', Some('&')) => {
                self.next();
                Token::And
            }
            ('&', _) => Token::Amp,
            ('|', Some('|')) => {
                self.next();
                Token::Or
            }
            ('|', _) => Token::VBar,
            ('^', _) => Token::Hat,
            ('~', _) => Token::Til,
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
            _ => Error::lexical("invalid token", self.pos)?,
        };

        Ok(token)
    }

    pub fn next_token(&mut self) -> Result<TokenInfo<'a>> {
        while let Some(&ch) = self.peek() {
            if self.skip_whitespace() || self.skip_comment() {
                continue;
            }

            let position = self.pos;
            let token = match ch {
                'a'..='z' | 'A'..='Z' | '_' => self.next_ident(),
                '0'..='9' | '.' => self.next_number(),
                '"' => self.next_string(),
                _ => self.next_symbol(),
            }?;

            return TokenInfo::new(token, position).ok();
        }

        TokenInfo::eof(self.pos).ok()
    }
}

pub struct LexIter<'a> {
    lexer: Lexer<'a>,
    ended: bool,
}

impl<'a> Iterator for LexIter<'a> {
    type Item = Result<TokenInfo<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.lexer.next_token() {
            Ok(token) if token.is_eof() && !self.ended => {
                self.ended = true;
                return Some(Ok(token));
            }
            Ok(token) if token.is_eof() && self.ended => {
                return None;
            }
            any => Some(any),
        }
    }
}

impl<'a> IntoIterator for Lexer<'a> {
    type Item = Result<TokenInfo<'a>>;
    type IntoIter = LexIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        LexIter {
            lexer: self,
            ended: false,
        }
    }
}
