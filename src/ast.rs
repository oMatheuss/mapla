use std::path::PathBuf;
use std::rc::Rc;

use crate::error::Result;
use crate::position::Position;

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
pub enum AstType {
    Int,
    Real,
    Byte,
    Char,
    Bool,
    Void,
    Pointer(Box<AstType>),
    Array(Box<AstType>, u32),
    Named(Vec<String>),
}

#[derive(Debug, Clone)]
pub struct AstArgument {
    pub name: String,
    pub arg_type: AstType,
}

#[derive(Debug, Clone)]
pub enum Literal {
    String(String),
    Byte(u8),
    Int(i32),
    Float(f32),
    Bool(bool),
}

#[derive(Debug)]
pub enum Expression {
    Identifier {
        id: Identifier,
    },
    Literal {
        lit: Literal,
    },
    UnaOp {
        ope: UnaOpe,
        val: Box<Expression>,
    },
    BinOp {
        ope: BinOpe,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    Call {
        func: Box<Expression>,
        args: Vec<Expression>,
    },
    Index {
        array: Box<Expression>,
        index: Box<Expression>,
    },
    Cast {
        val: Box<Expression>,
        typ: AstType,
    },
    Field {
        expr: Box<Expression>,
        field: Identifier,
    },
    Member {
        ns: Identifier,
        member: Identifier,
    },
    SizeOf {
        val: Box<Expression>,
    },
}

impl Expression {
    pub const TRUE: Self = Self::Literal {
        lit: Literal::Bool(true),
    };
    pub const FALSE: Self = Self::Literal {
        lit: Literal::Bool(false),
    };

    #[inline]
    pub fn id(name: &str, pos: Position) -> Self {
        Self::Identifier {
            id: Identifier::new(name, pos),
        }
    }

    #[inline]
    pub fn float(f: f32) -> Self {
        Self::Literal {
            lit: Literal::Float(f),
        }
    }

    #[inline]
    pub fn int(i: i32) -> Self {
        Self::Literal {
            lit: Literal::Int(i),
        }
    }

    #[inline]
    pub fn bin_op(ope: BinOpe, lhs: Expression, rhs: Expression) -> Self {
        Self::BinOp {
            ope,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    #[inline]
    pub fn una_op(ope: UnaOpe, operand: Expression) -> Self {
        Self::UnaOp {
            ope,
            val: Box::new(operand),
        }
    }

    #[inline]
    pub fn call(func: Expression, args: Vec<Expression>) -> Self {
        Self::Call {
            func: Box::new(func),
            args,
        }
    }

    #[inline]
    pub fn index(array: Expression, index: Expression) -> Self {
        Self::Index {
            array: Box::new(array),
            index: Box::new(index),
        }
    }

    #[inline]
    pub fn cast(expr: Expression, as_type: AstType) -> Self {
        Self::Cast {
            val: Box::new(expr),
            typ: as_type,
        }
    }

    pub fn field(expr: Expression, field: &str, pos: Position) -> Self {
        Self::Field {
            expr: expr.into(),
            field: Identifier::new(field, pos),
        }
    }

    pub fn member(ns: Identifier, member: Identifier) -> Self {
        Self::Member { ns, member }
    }

    pub fn sizeof(expr: Expression) -> Self {
        Self::SizeOf {
            val: Box::new(expr),
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
    Global(AstType, Identifier, Option<Literal>),
    Struct(Identifier, Vec<AstArgument>),
    Func(AstType, Identifier, Vec<AstArgument>, Vec<AstNode>),
    ExternFunc(AstType, Identifier, Vec<AstArgument>),
}

impl AstRoot {
    pub fn ok(self) -> Result<Self> {
        Result::Ok(self)
    }
}

#[derive(Debug)]
pub enum AstNode {
    TypedVar(AstType, String, Option<Expression>),
    Var(String, Expression),
    If(Expression, Vec<AstNode>),
    While(Expression, Vec<AstNode>),
    For(String, Option<Literal>, Expression, Vec<AstNode>),
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
