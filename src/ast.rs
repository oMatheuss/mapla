use std::ops::Deref;

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
    Void,
}

impl std::fmt::Display for VarType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VarType::Int => write!(f, "int"),
            VarType::Real => write!(f, "real"),
            VarType::Bool => write!(f, "bool"),
            VarType::Char => write!(f, "char"),
            VarType::Void => write!(f, "void"),
        }
    }
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
    Identifier(Identifier, VarType),
}

impl ValueExpr {
    fn value_type(&self) -> VarType {
        match self {
            ValueExpr::String(..) => todo!(),
            ValueExpr::Int(..) => VarType::Int,
            ValueExpr::Float(..) => VarType::Real,
            ValueExpr::Bool(..) => VarType::Bool,
            ValueExpr::Identifier(.., var_type) => *var_type,
        }
    }
}

#[derive(Debug)]
pub struct BinaryOp {
    operator: Operator,
    lhs: Box<Expression>,
    rhs: Box<Expression>,
    result: VarType,
}

impl BinaryOp {
    #[inline]
    pub fn lhs(&self) -> &Expression {
        &self.lhs
    }

    #[inline]
    pub fn rhs(&self) -> &Expression {
        &self.rhs
    }

    #[inline]
    pub fn operator(&self) -> Operator {
        self.operator
    }

    #[inline]
    pub fn result_type(&self) -> VarType {
        self.result
    }

    pub fn is_float_expr(&self) -> bool {
        self.lhs.expr_type() == VarType::Real || self.rhs.expr_type() == VarType::Real
    }
}

#[derive(Debug)]
pub struct FunctionCall {
    name: Identifier,
    args: Vec<Expression>,
    ret: VarType,
}

impl FunctionCall {
    #[inline]
    pub fn name(&self) -> &str {
        &self.name
    }

    #[inline]
    pub fn args(&self) -> &[Expression] {
        &self.args
    }

    #[inline]
    pub fn return_type(&self) -> VarType {
        self.ret
    }
}

#[derive(Debug)]
pub enum Expression {
    Value(ValueExpr),
    BinOp(BinaryOp),
    Func(FunctionCall),
    Cast(VarType, Box<Expression>),
}

impl From<ValueExpr> for Expression {
    fn from(value: ValueExpr) -> Self {
        Self::Value(value)
    }
}

impl From<BinaryOp> for Expression {
    fn from(value: BinaryOp) -> Self {
        Self::BinOp(value)
    }
}

impl From<FunctionCall> for Expression {
    fn from(value: FunctionCall) -> Self {
        Self::Func(value)
    }
}

impl Expression {
    pub const TRUE: Self = Self::Value(ValueExpr::Bool(true));
    pub const FALSE: Self = Self::Value(ValueExpr::Bool(false));

    #[inline]
    pub fn float(f: f32) -> Self {
        Self::Value(ValueExpr::Float(f))
    }

    #[inline]
    pub fn int(i: i32) -> Self {
        Self::Value(ValueExpr::Int(i))
    }

    #[inline]
    pub fn identifier(id: &str, var_type: VarType) -> Self {
        Self::Value(ValueExpr::Identifier(id.into(), var_type))
    }

    #[inline]
    pub fn string(s: &str) -> Self {
        Self::Value(ValueExpr::String(String::from(s)))
    }

    #[inline]
    pub fn bin_op(op: Operator, lhs: Expression, rhs: Expression, result: VarType) -> Self {
        Self::BinOp(BinaryOp {
            operator: op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            result,
        })
    }

    #[inline]
    pub fn func(name: &str, args: Vec<Expression>, ret: VarType) -> Self {
        Self::Func(FunctionCall {
            name: name.into(),
            args,
            ret,
        })
    }

    pub fn expr_type(&self) -> VarType {
        match self {
            Expression::Value(value) => value.value_type(),
            Expression::BinOp(bin_op) => bin_op.result_type(),
            Expression::Func(func) => func.return_type(),
            Expression::Cast(cast, ..) => *cast,
        }
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
}

#[derive(Debug)]
pub enum AstNode {
    Use(Identifier),
    Var(VarType, Identifier, Expression),
    If(Expression, Vec<AstNode>),
    While(Expression, Vec<AstNode>),
    For(Identifier, ValueExpr, Vec<AstNode>),
    Expr(Expression),
    Func(Identifier, Vec<Argument>, VarType, Vec<AstNode>),
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
