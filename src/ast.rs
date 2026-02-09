use std::path::PathBuf;
use std::rc::Rc;

use crate::error::Result;
use crate::position::Position;
use crate::types::{Argument, Type};

#[derive(Debug, Default)]
pub struct Ast {
    pub namespace: Rc<String>,
    pub nodes: Vec<AstRoot>,
    pub imports: Vec<PathBuf>,
    pub uses: Vec<String>,
}

impl Ast {
    pub fn new(namespace: &str) -> Self {
        Self {
            namespace: Rc::new(namespace.into()),
            ..Default::default()
        }
    }
}

#[derive(Debug, Clone)]
pub enum ValueExpr {
    Id(String),
    String(String),
    Byte(u8),
    Int(i32),
    Float(f32),
    Bool(bool),
}

#[derive(Debug)]
pub enum Expression {
    Value {
        value: ValueExpr,
    },
    UnaOp {
        operator: UnaOpe,
        operand: Box<Expression>,
    },
    BinOp {
        operator: BinOpe,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    Call {
        name: String,
        args: Vec<Expression>,
    },
    Index {
        array: Box<Expression>,
        index: Box<Expression>,
    },
    Cast {
        value: Box<Expression>,
        as_type: Type,
    },
}

impl Expression {
    pub const TRUE: Self = Self::Value {
        value: ValueExpr::Bool(true),
    };
    pub const FALSE: Self = Self::Value {
        value: ValueExpr::Bool(false),
    };

    #[inline]
    pub fn id(id: &str) -> Self {
        Self::Value {
            value: ValueExpr::Id(id.into()),
        }
    }

    #[inline]
    pub fn float(f: f32) -> Self {
        Self::Value {
            value: ValueExpr::Float(f),
        }
    }

    #[inline]
    pub fn int(i: i32) -> Self {
        Self::Value {
            value: ValueExpr::Int(i),
        }
    }

    #[inline]
    pub fn bin_op(op: BinOpe, lhs: Expression, rhs: Expression) -> Self {
        Self::BinOp {
            operator: op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    #[inline]
    pub fn una_op(op: UnaOpe, operand: Expression) -> Self {
        Self::UnaOp {
            operator: op,
            operand: Box::new(operand),
        }
    }

    #[inline]
    pub fn call(name: String, args: Vec<Expression>) -> Self {
        Self::Call { name, args }
    }

    #[inline]
    pub fn index(array: Expression, index: Expression) -> Self {
        Self::Index {
            array: Box::new(array),
            index: Box::new(index),
        }
    }

    #[inline]
    pub fn cast(expr: Expression, as_type: Type) -> Self {
        Self::Cast {
            value: Box::new(expr),
            as_type,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub name: String,
    pub position: Position,
}

impl Identifier {
    pub fn new(name: &str, position: Position) -> Self {
        Self {
            name: name.to_owned(),
            position,
        }
    }
}

#[derive(Debug)]
pub enum AstRoot {
    Global(Type, Identifier, Option<ValueExpr>),
    Struct(Identifier, Vec<Argument>),
    Func(Type, Identifier, Vec<Argument>, Vec<AstNode>),
    ExternFunc(Type, Identifier, Vec<Argument>),
}

impl AstRoot {
    pub fn ok(self) -> Result<Self> {
        Result::Ok(self)
    }
}

#[derive(Debug)]
pub enum AstNode {
    TypedVar(Type, String, Option<Expression>),
    Var(String, Expression),
    If(Expression, Vec<AstNode>),
    While(Expression, Vec<AstNode>),
    For(String, Option<ValueExpr>, ValueExpr, Vec<AstNode>),
    Expr(Expression),
    Ret(Expression),
}

impl AstNode {
    pub fn ok(self) -> Result<Self> {
        Result::Ok(self)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BinOpe {
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
    Shr,
    Shl,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
}

impl BinOpe {
    pub fn precedence(&self) -> u8 {
        match self {
            BinOpe::Mul | BinOpe::Div | BinOpe::Mod => 11,
            BinOpe::Add | BinOpe::Sub => 10,
            BinOpe::Shl | BinOpe::Shr => 9,
            BinOpe::Greater | BinOpe::GreaterEqual | BinOpe::Less | BinOpe::LessEqual => 8,
            BinOpe::Equal | BinOpe::NotEqual => 7,
            BinOpe::BitwiseAnd => 6,
            BinOpe::BitwiseXor => 5,
            BinOpe::BitwiseOr => 4,
            BinOpe::And => 3,
            BinOpe::Or => 2,
            BinOpe::Assign
            | BinOpe::AddAssign
            | BinOpe::SubAssign
            | BinOpe::MulAssign
            | BinOpe::DivAssign => 1,
        }
    }

    pub fn is_assign(&self) -> bool {
        matches!(
            self,
            BinOpe::Assign
                | BinOpe::AddAssign
                | BinOpe::SubAssign
                | BinOpe::MulAssign
                | BinOpe::DivAssign
        )
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UnaOpe {
    AddressOf,
    Dereference,
    Minus,
    Not,
    BitwiseNot,
}

impl UnaOpe {
    pub fn precedence(&self) -> u8 {
        12
    }
}
