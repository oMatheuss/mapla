use crate::error::Result;

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

#[derive(Debug, PartialEq)]
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
    Identifier(Identifier),
}

#[derive(Debug)]
pub enum Expression {
    Value(ValueExpr),
    BinOp(Operator, Box<[Expression; 2]>),
    Cast(VarType, Box<Expression>),
}

impl From<ValueExpr> for Expression {
    fn from(value: ValueExpr) -> Self {
        Self::Value(value)
    }
}

#[derive(Debug)]
pub enum AstNode {
    Var(VarType, Identifier, Expression),
    If(Expression, Vec<AstNode>),
    While(Expression, Vec<AstNode>),
    For(Identifier, ValueExpr, Vec<AstNode>),
    Expr(Expression),
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

impl TryFrom<&[char]> for Operator {
    type Error = crate::error::Error;

    fn try_from(value: &[char]) -> std::result::Result<Self, Self::Error> {
        let op = match value {
            ['=', '='] => Operator::Equal,
            ['!', '='] => Operator::NotEqual,
            ['>'] => Operator::Greater,
            ['>', '='] => Operator::GreaterEqual,
            ['<'] => Operator::Less,
            ['<', '='] => Operator::LessEqual,
            ['&', '&'] => Operator::And,
            ['|', '|'] => Operator::Or,
            ['+'] => Operator::Add,
            ['-'] => Operator::Sub,
            ['*'] => Operator::Mul,
            ['/'] => Operator::Div,
            ['%'] => Operator::Mod,
            ['='] => Operator::Assign,
            ['+', '='] => Operator::AddAssign,
            ['-', '='] => Operator::SubAssign,
            ['*', '='] => Operator::MulAssign,
            ['/', '='] => Operator::DivAssign,
            _ => crate::error::Error::syntatic("invalid symbol")?,
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
        *self as u8 >= Operator::Assign as u8
    }

    pub fn is_valid_primary(ch: &char) -> bool {
        ['=', '!', '>', '<', '&', '|', '%', '+', '-', '*', '/'].contains(ch)
    }
}
