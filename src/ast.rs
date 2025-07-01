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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Annotation {
    Value,
    Pointer,
    Array(u32),
}

impl std::fmt::Display for Annotation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Annotation::Value => Ok(()),
            Annotation::Pointer => write!(f, "*"),
            Annotation::Array(size) => write!(f, "[{size}]"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TypeAnnot {
    inner: VarType,
    annot: Annotation,
}

impl std::fmt::Display for TypeAnnot {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.inner, self.annot)
    }
}

impl TypeAnnot {
    pub const INT: Self = TypeAnnot::new(VarType::Int);
    pub const REAL: Self = TypeAnnot::new(VarType::Real);
    pub const CHAR: Self = TypeAnnot::new(VarType::Char);
    pub const BOOL: Self = TypeAnnot::new(VarType::Bool);
    pub const VOID: Self = TypeAnnot::new(VarType::Void);

    pub const fn new(inner: VarType) -> Self {
        TypeAnnot {
            inner,
            annot: Annotation::Value,
        }
    }

    pub const fn new_ptr(inner: VarType) -> Self {
        TypeAnnot {
            inner,
            annot: Annotation::Pointer,
        }
    }

    pub const fn new_array(inner: VarType, size: u32) -> Self {
        TypeAnnot {
            inner,
            annot: Annotation::Array(size),
        }
    }

    pub fn is_bool(self) -> bool {
        self == TypeAnnot::BOOL
    }

    pub fn is_char(self) -> bool {
        self == TypeAnnot::CHAR
    }

    pub fn is_number(self) -> bool {
        self == TypeAnnot::INT || self == TypeAnnot::REAL
    }

    pub fn is_int(self) -> bool {
        self == TypeAnnot::INT
    }

    pub fn is_float(self) -> bool {
        self == TypeAnnot::REAL
    }

    pub fn is_void(self) -> bool {
        self == TypeAnnot::VOID
    }

    pub fn is_ref(self) -> bool {
        matches!(self.annot, Annotation::Array(..) | Annotation::Pointer)
    }

    pub fn inner_type(&self) -> VarType {
        self.inner
    }

    pub fn annotation(&self) -> Annotation {
        self.annot
    }
}

pub trait Annotated {
    fn get_annot(&self) -> TypeAnnot;
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
    Identifier(TypeAnnot, Identifier),
}

impl Annotated for ValueExpr {
    fn get_annot(&self) -> TypeAnnot {
        match self {
            ValueExpr::String(s) => TypeAnnot::new_array(VarType::Char, s.len() as u32),
            ValueExpr::Int(..) => TypeAnnot::new(VarType::Int),
            ValueExpr::Float(..) => TypeAnnot::new(VarType::Real),
            ValueExpr::Bool(..) => TypeAnnot::new(VarType::Bool),
            ValueExpr::Identifier(annot, ..) => *annot,
        }
    }
}

#[derive(Debug)]
pub struct BinaryOp {
    operator: Operator,
    lhs: Box<Expression>,
    rhs: Box<Expression>,
    annot: TypeAnnot,
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
}

impl Annotated for BinaryOp {
    fn get_annot(&self) -> TypeAnnot {
        self.annot
    }
}

#[derive(Debug)]
pub struct FunctionCall {
    name: Identifier,
    args: Vec<Expression>,
    annot: TypeAnnot,
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
}

impl Annotated for FunctionCall {
    fn get_annot(&self) -> TypeAnnot {
        self.annot
    }
}

#[derive(Debug)]
pub struct UnaryOp {
    operator: UnaryOperator,
    operand: Box<Expression>,
    annot: TypeAnnot,
}

impl UnaryOp {
    #[inline]
    pub fn operand(&self) -> &Expression {
        &self.operand
    }

    #[inline]
    pub fn operator(&self) -> UnaryOperator {
        self.operator
    }
}

impl Annotated for UnaryOp {
    fn get_annot(&self) -> TypeAnnot {
        self.annot
    }
}

#[derive(Debug)]
pub struct Indexing {
    array: ValueExpr,
    index: Box<Expression>,
    annot: TypeAnnot,
}

impl Indexing {
    #[inline]
    pub fn array(&self) -> &ValueExpr {
        &self.array
    }

    #[inline]
    pub fn index(&self) -> &Expression {
        &self.index
    }
}

impl Annotated for Indexing {
    fn get_annot(&self) -> TypeAnnot {
        self.annot
    }
}

#[derive(Debug)]
pub enum Expression {
    Value(ValueExpr),
    UnaOp(UnaryOp),
    BinOp(BinaryOp),
    Func(FunctionCall),
    Index(Indexing),
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

impl From<UnaryOp> for Expression {
    fn from(value: UnaryOp) -> Self {
        Self::UnaOp(value)
    }
}

impl From<Indexing> for Expression {
    fn from(value: Indexing) -> Self {
        Self::Index(value)
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
    pub fn identifier(annot: TypeAnnot, id: &str) -> Self {
        Self::Value(ValueExpr::Identifier(annot, id.into()))
    }

    #[inline]
    pub fn string(s: &str) -> Self {
        Self::Value(ValueExpr::String(String::from(s)))
    }

    #[inline]
    pub fn bin_op(op: Operator, lhs: Expression, rhs: Expression, annot: TypeAnnot) -> Self {
        Self::BinOp(BinaryOp {
            operator: op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            annot,
        })
    }

    #[inline]
    pub fn func(name: Identifier, args: Vec<Expression>, annot: TypeAnnot) -> Self {
        Self::Func(FunctionCall { name, args, annot })
    }

    #[inline]
    pub fn una_op(op: UnaryOperator, operand: Expression, annot: TypeAnnot) -> Self {
        Self::UnaOp(UnaryOp {
            operator: op,
            operand: Box::new(operand),
            annot,
        })
    }

    #[inline]
    pub fn index(array: ValueExpr, index: Expression, annot: TypeAnnot) -> Self {
        Self::Index(Indexing {
            array,
            index: Box::new(index),
            annot,
        })
    }

    #[inline]
    pub fn is_value(&self) -> bool {
        matches!(self, Self::Value(..))
    }

    #[inline]
    pub fn is_index(&self) -> bool {
        matches!(self, Self::Index(..))
    }
}

impl Annotated for Expression {
    fn get_annot(&self) -> TypeAnnot {
        match self {
            Expression::Value(value) => value.get_annot(),
            Expression::UnaOp(unoop) => unoop.get_annot(),
            Expression::BinOp(binop) => binop.get_annot(),
            Expression::Func(func) => func.get_annot(),
            Expression::Index(index) => index.get_annot(),
        }
    }
}

#[derive(Debug)]
pub struct Argument {
    pub name: Identifier,
    pub annot: TypeAnnot,
}

impl Argument {
    pub fn new(name: &str, annot: TypeAnnot) -> Self {
        Self {
            name: name.into(),
            annot,
        }
    }
}

impl Annotated for Argument {
    fn get_annot(&self) -> TypeAnnot {
        self.annot
    }
}

#[derive(Debug)]
pub enum AstNode {
    Use(Identifier),
    Var(TypeAnnot, Identifier, Option<Expression>),
    If(Expression, Vec<AstNode>),
    While(Expression, Vec<AstNode>),
    For(Identifier, Option<ValueExpr>, ValueExpr, Vec<AstNode>),
    Expr(Expression),
    Func(Identifier, Vec<Argument>, TypeAnnot, Vec<AstNode>),
    ExternFunc(Identifier, Vec<Argument>, TypeAnnot),
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

#[derive(Debug, Clone, Copy)]
pub enum UnaryOperator {
    AddressOf,
    Dereference,
    Minus,
    Not,
    BitwiseNot,
}
