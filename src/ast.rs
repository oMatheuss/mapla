use std::ops::Deref;

use crate::{error::Result, token::Token};

#[derive(Debug)]
pub struct Ast(Vec<AstNode>);

impl Ast {
    pub fn new(nodes: Vec<AstNode>) -> Self {
        Self(nodes)
    }

    #[inline]
    pub fn ok(self) -> Result<Self> {
        Ok(self)
    }
}

impl std::ops::Deref for Ast {
    type Target = [AstNode];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum VarType {
    Int,
    Real,
    Bool,
    Char,
}

#[derive(Debug)]
pub struct Identifier(String);

impl From<&str> for Identifier {
    fn from(value: &str) -> Self {
        Self(String::from(value))
    }
}

impl PartialEq<str> for Identifier {
    fn eq(&self, other: &str) -> bool {
        self.0.eq(other)
    }
}

impl Deref for Identifier {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug)]
pub enum ValueExpr {
    String(String),
    Int(i32),
    Float(f32),
    Bool(bool),
    Identifier(Identifier),
}

#[derive(Debug)]
pub enum Expression {
    Value(ValueExpr),
    BinOp(Operator, Box<[Expression; 2]>),
    Func(Identifier, Vec<Expression>, VarType),
    Cast(VarType, Box<Expression>),
}

impl From<ValueExpr> for Expression {
    fn from(value: ValueExpr) -> Self {
        Self::Value(value)
    }
}

impl Expression {
    pub const TRUE: Self = Self::Value(ValueExpr::Bool(true));
    pub const FALSE: Self = Self::Value(ValueExpr::Bool(false));

    #[inline]
    pub fn float(f: f32) -> Self {
        Expression::Value(ValueExpr::Float(f))
    }

    #[inline]
    pub fn int(i: i32) -> Self {
        Expression::Value(ValueExpr::Int(i))
    }

    #[inline]
    pub fn identifier(id: &str) -> Self {
        Expression::Value(ValueExpr::Identifier(Identifier(String::from(id))))
    }

    #[inline]
    pub fn string(s: &str) -> Self {
        Expression::Value(ValueExpr::String(String::from(s)))
    }
}

#[derive(Debug)]
pub struct Argument {
    pub name: Identifier,
    pub arg_type: VarType,
    pub default: Option<ValueExpr>,
}

impl Argument {
    pub fn new(name: &str, arg_type: VarType) -> Self {
        Self {
            name: name.into(),
            arg_type,
            default: None,
        }
    }

    pub fn with_default(name: &str, arg_type: VarType, default: ValueExpr) -> Self {
        Self {
            name: name.into(),
            arg_type,
            default: Some(default),
        }
    }
}

#[derive(Debug)]
pub enum AstNode {
    Use(Identifier),
    Var(VarType, Identifier, Expression),
    If(Expression, Vec<AstNode>),
    While(Expression, Vec<AstNode>),
    For(Identifier, ValueExpr, Vec<AstNode>),
    Expr(Expression),
    Func(Identifier, Vec<Argument>, Option<VarType>, Vec<AstNode>),
    Ret(Expression),
}

impl AstNode {
    #[inline]
    pub fn ok(self) -> Result<Self> {
        Ok(self)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Operator {
    Equal,
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
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
}

impl<'a> TryFrom<&Token<'a>> for Operator {
    type Error = crate::error::Error;

    fn try_from(value: &Token) -> std::result::Result<Self, Self::Error> {
        let op = match value {
            Token::Equal => Operator::Equal,
            Token::NotEqual => Operator::NotEqual,
            Token::Greater => Operator::Greater,
            Token::GreaterEqual => Operator::GreaterEqual,
            Token::Less => Operator::Less,
            Token::LessEqual => Operator::LessEqual,
            Token::And => Operator::And,
            Token::Or => Operator::Or,
            Token::Add => Operator::Add,
            Token::Sub => Operator::Sub,
            Token::Mul => Operator::Mul,
            Token::Div => Operator::Div,
            Token::Mod => Operator::Mod,
            Token::Assign => Operator::Assign,
            Token::AddAssign => Operator::AddAssign,
            Token::SubAssign => Operator::SubAssign,
            Token::MulAssign => Operator::MulAssign,
            Token::DivAssign => Operator::DivAssign,
            _ => crate::error::Error::syntatic("invalid token")?,
        };

        Ok(op)
    }
}

impl Operator {
    pub fn precedence(&self) -> u8 {
        match self {
            Operator::Mul | Operator::Div | Operator::Mod => 7,
            Operator::Add | Operator::Sub => 6,
            Operator::Greater | Operator::GreaterEqual | Operator::Less | Operator::LessEqual => 5,
            Operator::Equal | Operator::NotEqual => 4,
            Operator::And => 3,
            Operator::Or => 2,
            Operator::Assign
            | Operator::AddAssign
            | Operator::SubAssign
            | Operator::MulAssign
            | Operator::DivAssign => 1,
        }
    }

    pub fn is_right(&self) -> bool {
        matches!(
            self,
            Operator::Assign
                | Operator::AddAssign
                | Operator::SubAssign
                | Operator::MulAssign
                | Operator::DivAssign
        )
    }

    pub fn is_assign(&self) -> bool {
        matches!(
            self,
            Operator::Assign
                | Operator::AddAssign
                | Operator::SubAssign
                | Operator::MulAssign
                | Operator::DivAssign
        )
    }
}
