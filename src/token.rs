use crate::ast::{BinOpe, UnaOpe};
use crate::error::Result;
use crate::position::Position;

#[derive(Debug)]
pub enum Token<'a> {
    Var,
    Int,
    Real,
    Char,
    Byte,
    Bool,
    Void,
    Identifier(&'a str),
    If,
    While,
    For,
    To,
    End,
    Then,
    Extern,
    Function,
    Do,
    Return,
    Namespace,
    Use,
    Import,
    Struct,
    SizeOf,
    StrLiteral(&'a str),
    IntLiteral(u32),
    FloatLiteral(f32),
    True,
    False,
    As,
    Equal,
    Not,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    And,
    Or,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Shr,
    Shl,
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    Comma,
    Colon,
    DuoColon,
    SemiColon,
    Amp,
    Til,
    VBar,
    Hat,
    Dot,
    Eof,
}

impl<'a> Token<'a> {
    pub fn from_str(s: &'a str) -> Self {
        match s {
            "var" => Self::Var,
            "int" => Self::Int,
            "real" => Self::Real,
            "char" => Self::Char,
            "byte" => Self::Byte,
            "bool" => Self::Bool,
            "void" => Self::Void,
            "if" => Self::If,
            "while" => Self::While,
            "for" => Self::For,
            "to" => Self::To,
            "then" => Self::Then,
            "extern" => Self::Extern,
            "func" => Self::Function,
            "do" => Self::Do,
            "return" => Self::Return,
            "end" => Self::End,
            "namespace" => Self::Namespace,
            "use" => Self::Use,
            "import" => Self::Import,
            "struct" => Self::Struct,
            "sizeof" => Self::SizeOf,
            "true" => Self::True,
            "false" => Self::False,
            "as" => Self::As,
            "not" => Self::Not,
            "or" => Self::Or,
            "and" => Self::And,
            s => Self::Identifier(s),
        }
    }
}

#[derive(Debug)]
pub struct TokenInfo<'a> {
    token: Token<'a>,
    position: Position,
}

impl<'a> TokenInfo<'a> {
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
    pub fn is_eof(&self) -> bool {
        matches!(self.token, Token::Eof)
    }

    #[inline]
    pub fn ok(self) -> Result<Self> {
        Result::Ok(self)
    }
}

pub struct TokenStream<'a> {
    tokens: std::iter::Peekable<std::vec::IntoIter<TokenInfo<'a>>>,
    position: Position,
}

impl<'a> TokenStream<'a> {
    #[inline]
    pub fn next(&mut self) -> Option<Token<'a>> {
        match self.tokens.next() {
            Some(item) => {
                self.position = item.position;
                Some(item.token)
            }
            None => None,
        }
    }

    #[inline]
    pub fn peek(&mut self) -> Option<&Token<'a>> {
        match self.tokens.peek() {
            Some(TokenInfo { position: _, token }) => Some(token),
            None => None,
        }
    }

    pub fn pos(&self) -> Position {
        self.position.clone()
    }

    #[inline]
    pub fn peek_pos(&mut self) -> Position {
        match self.tokens.peek() {
            Some(TokenInfo { position, token: _ }) => position.clone(),
            None => self.position.clone(),
        }
    }

    pub fn peek_binop(&mut self) -> Option<BinOpe> {
        let token = self.peek()?;

        let op = match token {
            Token::Equal => BinOpe::Equal,
            Token::NotEqual => BinOpe::NotEqual,
            Token::Greater => BinOpe::Greater,
            Token::GreaterEqual => BinOpe::GreaterEqual,
            Token::Less => BinOpe::Less,
            Token::LessEqual => BinOpe::LessEqual,
            Token::And => BinOpe::And,
            Token::Amp => BinOpe::BitwiseAnd,
            Token::Or => BinOpe::Or,
            Token::VBar => BinOpe::BitwiseOr,
            Token::Hat => BinOpe::BitwiseXor,
            Token::Add => BinOpe::Add,
            Token::Sub => BinOpe::Sub,
            Token::Mul => BinOpe::Mul,
            Token::Div => BinOpe::Div,
            Token::Mod => BinOpe::Mod,
            Token::Shr => BinOpe::Shr,
            Token::Shl => BinOpe::Shl,
            Token::Assign => BinOpe::Assign,
            Token::AddAssign => BinOpe::AddAssign,
            Token::SubAssign => BinOpe::SubAssign,
            Token::MulAssign => BinOpe::MulAssign,
            Token::DivAssign => BinOpe::DivAssign,
            _ => return None,
        };

        Some(op)
    }

    pub fn peek_unaop(&mut self) -> Option<UnaOpe> {
        let token = self.peek()?;

        let op = match token {
            Token::Amp => UnaOpe::AddressOf,
            Token::Sub => UnaOpe::Minus,
            Token::Mul => UnaOpe::Dereference,
            Token::Not => UnaOpe::Not,
            Token::Til => UnaOpe::BitwiseNot,
            _ => return None,
        };

        Some(op)
    }
}

impl<'a> TryFrom<crate::lexer::Lexer<'a>> for TokenStream<'a> {
    type Error = crate::error::Error;

    fn try_from(value: crate::lexer::Lexer<'a>) -> std::result::Result<Self, Self::Error> {
        let tokens = value.into_iter().collect::<Result<Vec<_>>>()?;
        Ok(Self {
            tokens: tokens.into_iter().peekable(),
            position: Default::default(),
        })
    }
}
