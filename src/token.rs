use crate::ast::{Operator, UnaryOperator};
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
    Use,
    Import,
    Struct,
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
            "use" => Self::Use,
            "import" => Self::Import,
            "struct" => Self::Struct,
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
    position: Position<'a>,
}

impl<'a> TokenInfo<'a> {
    #[inline]
    pub fn new(token: Token<'a>, position: Position<'a>) -> Self {
        Self { token, position }
    }

    #[inline]
    pub fn eof(position: Position<'a>) -> Self {
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
    pub position: Position<'a>,
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

    #[inline]
    pub fn peek_pos(&mut self) -> Position<'_> {
        match self.tokens.peek() {
            Some(TokenInfo { position, token: _ }) => *position,
            None => self.position,
        }
    }

    pub fn peek_binop(&mut self) -> Option<Operator> {
        let token = self.peek()?;

        let op = match token {
            Token::Equal => Operator::Equal,
            Token::NotEqual => Operator::NotEqual,
            Token::Greater => Operator::Greater,
            Token::GreaterEqual => Operator::GreaterEqual,
            Token::Less => Operator::Less,
            Token::LessEqual => Operator::LessEqual,
            Token::And => Operator::And,
            Token::Amp => Operator::BitwiseAnd,
            Token::Or => Operator::Or,
            Token::VBar => Operator::BitwiseOr,
            Token::Hat => Operator::BitwiseXor,
            Token::Add => Operator::Add,
            Token::Sub => Operator::Sub,
            Token::Mul => Operator::Mul,
            Token::Div => Operator::Div,
            Token::Mod => Operator::Mod,
            Token::Shr => Operator::Shr,
            Token::Shl => Operator::Shl,
            Token::Assign => Operator::Assign,
            Token::AddAssign => Operator::AddAssign,
            Token::SubAssign => Operator::SubAssign,
            Token::MulAssign => Operator::MulAssign,
            Token::DivAssign => Operator::DivAssign,
            _ => return None,
        };

        Some(op)
    }

    pub fn peek_unaop(&mut self) -> Option<UnaryOperator> {
        let token = self.peek()?;

        let op = match token {
            Token::Amp => UnaryOperator::AddressOf,
            Token::Sub => UnaryOperator::Minus,
            Token::Mul => UnaryOperator::Dereference,
            Token::Not => UnaryOperator::Not,
            Token::Til => UnaryOperator::BitwiseNot,
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
            position: Position::default(),
        })
    }
}
