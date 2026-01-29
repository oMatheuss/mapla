#![allow(dead_code)]

use crate::error::Result;
use crate::types::{Annotated, TypeAnnot};

#[derive(Debug)]
pub struct Ast(Vec<AstRoot>);

impl Ast {
    pub fn new(nodes: Vec<AstRoot>) -> Self {
        Self(nodes)
    }

    #[inline]
    pub fn ok(self) -> Result<Self> {
        Ok(self)
    }
}

impl std::ops::Deref for Ast {
    type Target = [AstRoot];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, Clone)]
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

impl std::ops::Deref for Identifier {
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

#[derive(Debug, Clone)]
pub enum ValueExpr {
    String(String),
    Byte(u8),
    Int(i32),
    Float(f32),
    Bool(bool),
    Identifier(TypeAnnot, Identifier),
}

impl Annotated for ValueExpr {
    fn get_annot(&self) -> TypeAnnot {
        match self {
            ValueExpr::String(s) => TypeAnnot::new_string(s.len() as u32),
            ValueExpr::Byte(..) => TypeAnnot::BYTE,
            ValueExpr::Int(..) => TypeAnnot::INT,
            ValueExpr::Float(..) => TypeAnnot::REAL,
            ValueExpr::Bool(..) => TypeAnnot::BOOL,
            ValueExpr::Identifier(annot, ..) => annot.clone(),
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

    pub fn is_float(&self) -> bool {
        return self.lhs.get_annot().is_float() && self.lhs.get_annot().is_float();
    }
}

impl Annotated for BinaryOp {
    fn get_annot(&self) -> TypeAnnot {
        self.annot.clone()
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
        self.annot.clone()
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
        self.annot.clone()
    }
}

#[derive(Debug)]
pub struct Indexing {
    array: Box<Expression>,
    index: Box<Expression>,
    annot: TypeAnnot,
}

impl Indexing {
    #[inline]
    pub fn array(&self) -> &Expression {
        &self.array
    }

    #[inline]
    pub fn index(&self) -> &Expression {
        &self.index
    }
}

impl Annotated for Indexing {
    fn get_annot(&self) -> TypeAnnot {
        self.annot.clone()
    }
}

#[derive(Debug)]
pub struct TypeCast {
    value: Box<Expression>,
    annot: TypeAnnot,
}

impl TypeCast {
    #[inline]
    pub fn value(&self) -> &Expression {
        &self.value
    }
}

impl Annotated for TypeCast {
    fn get_annot(&self) -> TypeAnnot {
        self.annot.clone()
    }
}

#[derive(Debug)]
pub struct AllocType {
    args: Vec<Expression>,
    annot: TypeAnnot,
}

impl AllocType {
    #[inline]
    pub fn args(&self) -> &[Expression] {
        &self.args
    }
}

impl Annotated for AllocType {
    fn get_annot(&self) -> TypeAnnot {
        self.annot.clone()
    }
}

#[derive(Debug)]
pub struct FieldAccess {
    value: Box<Expression>,
    offset: usize,
    annot: TypeAnnot,
}

impl FieldAccess {
    #[inline]
    pub fn value(&self) -> &Expression {
        &self.value
    }

    #[inline]
    pub fn offset(&self) -> usize {
        self.offset
    }
}

impl Annotated for FieldAccess {
    fn get_annot(&self) -> TypeAnnot {
        self.annot.clone()
    }
}

#[derive(Debug)]
pub enum Expression {
    Value(ValueExpr),
    UnaOp(UnaryOp),
    BinOp(BinaryOp),
    Func(FunctionCall),
    Index(Indexing),
    Cast(TypeCast),
    Alloc(AllocType),
    Field(FieldAccess),
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

impl From<AllocType> for Expression {
    fn from(value: AllocType) -> Self {
        Self::Alloc(value)
    }
}

impl From<FieldAccess> for Expression {
    fn from(value: FieldAccess) -> Self {
        Self::Field(value)
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
    pub fn byte(b: u8) -> Self {
        Self::Value(ValueExpr::Byte(b))
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
    pub fn bool(b: bool) -> Self {
        Self::Value(ValueExpr::Bool(b))
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
    pub fn index(array: Expression, index: Expression, annot: TypeAnnot) -> Self {
        Self::Index(Indexing {
            array: Box::new(array),
            index: Box::new(index),
            annot,
        })
    }

    #[inline]
    pub fn cast(expr: Expression, annot: TypeAnnot) -> Self {
        Self::Cast(TypeCast {
            value: Box::new(expr),
            annot,
        })
    }

    #[inline]
    pub fn alloc(args: Vec<Expression>, annot: TypeAnnot) -> Self {
        Self::Alloc(AllocType { args, annot })
    }

    #[inline]
    pub fn field(expr: Expression, offset: usize, annot: TypeAnnot) -> Self {
        Self::Field(FieldAccess {
            value: Box::new(expr),
            offset,
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

    #[inline]
    pub fn is_ident(&self) -> bool {
        matches!(self, Self::Value(ValueExpr::Identifier(..)))
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
            Expression::Cast(cast) => cast.get_annot(),
            Expression::Alloc(alloc) => alloc.get_annot(),
            Expression::Field(field) => field.get_annot(),
        }
    }
}

#[derive(Debug, Clone)]
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
        self.annot.clone()
    }
}

#[derive(Debug)]
pub enum AstRoot {
    Global(TypeAnnot, Identifier, Option<ValueExpr>),
    Func(Identifier, Vec<Argument>, TypeAnnot, Vec<AstNode>),
    ExternFunc(Identifier, Vec<Argument>, TypeAnnot),
}

impl AstRoot {
    #[inline]
    pub fn ok(self) -> Result<Self> {
        Ok(self)
    }
}

#[derive(Debug)]
pub enum AstNode {
    Var(TypeAnnot, Identifier, Option<Expression>),
    If(Expression, Vec<AstNode>),
    While(Expression, Vec<AstNode>),
    For(Identifier, Option<ValueExpr>, ValueExpr, Vec<AstNode>),
    Expr(Expression),
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

impl Operator {
    pub fn precedence(&self) -> u8 {
        match self {
            Operator::Mul | Operator::Div | Operator::Mod => 11,
            Operator::Add | Operator::Sub => 10,
            Operator::Shl | Operator::Shr => 9,
            Operator::Greater | Operator::GreaterEqual | Operator::Less | Operator::LessEqual => 8,
            Operator::Equal | Operator::NotEqual => 7,
            Operator::BitwiseAnd => 6,
            Operator::BitwiseXor => 5,
            Operator::BitwiseOr => 4,
            Operator::And => 3,
            Operator::Or => 2,
            Operator::Assign
            | Operator::AddAssign
            | Operator::SubAssign
            | Operator::MulAssign
            | Operator::DivAssign => 1,
        }
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

    pub fn is_boolean(&self) -> bool {
        matches!(
            self,
            Operator::Greater
                | Operator::GreaterEqual
                | Operator::Less
                | Operator::LessEqual
                | Operator::Equal
                | Operator::NotEqual
                | Operator::And
                | Operator::Or
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

impl UnaryOperator {
    pub fn precedence(&self) -> u8 {
        12
    }
}
