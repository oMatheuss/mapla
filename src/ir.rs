use std::fmt::Display;
use std::rc::Rc;

use crate::ast::{BinOpe, UnaOpe};
use crate::types::{Argument, Type};

#[derive(Debug, Clone)]
pub struct IrFunc {
    pub name: String,
    pub namespace: Rc<String>,
    pub args: Vec<Argument>,
    pub typ: Type,
    pub body: Vec<IrNode>,
}

#[derive(Debug, Clone)]
pub enum IrNode {
    Store { index: usize, typ: Type },
    Alloc { index: usize, typ: Type },
    Load { value: IrArg },
    Pop,
    UnaOp { ope: UnaOpe, typ: Type },
    BinOp { ope: BinOpe, typ: Type },
    Arg,
    Call { args: Vec<Type>, ret: Type },
    Index { typ: Type },
    Cast { from: Type, to: Type },
    Field { offset: Vec<Type>, by_ref: bool },
    Struct { fields: Vec<Type> },
    SizeOf { typ: Type },
    Inc,
    Label { label: usize },
    Jmp { label: usize },
    JmpEq { label: usize },
    JmpFalse { label: usize },
    Return { typ: Type },
}

#[derive(Debug, Clone)]
pub enum IrLiteral {
    String { label: String, size: u32 },
    Byte(u8),
    Int(i32),
    Float(f32),
    Bool(bool),
}

#[derive(Debug, Clone)]
#[rustfmt::skip]
pub enum IrArg {
    Var { index: usize, typ: Type },
    Literal { value: IrLiteral },
    Global { ns: Rc<String>, name: String, typ: Type },
    Extern { name: String, typ: Type },
}

impl IrArg {
    pub fn var(index: usize, typ: Type) -> Self {
        Self::Var { index, typ }
    }

    pub fn get_type(&self) -> Type {
        match self {
            IrArg::Var { index: _, typ } => typ.clone(),
            IrArg::Literal { value } => match value {
                IrLiteral::String { label: _, size } => Type::Array(Box::new(Type::Char), *size),
                IrLiteral::Byte(_) => Type::Byte,
                IrLiteral::Int(_) => Type::Int,
                IrLiteral::Float(_) => Type::Real,
                IrLiteral::Bool(_) => Type::Bool,
            },
            IrArg::Global {
                ns: _,
                name: _,
                typ,
            } => typ.clone(),
            IrArg::Extern { name: _, typ } => typ.clone(),
        }
    }
}

impl From<IrLiteral> for IrArg {
    fn from(value: IrLiteral) -> Self {
        Self::Literal { value }
    }
}

impl Display for IrFunc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}@{}:", self.name, self.namespace)?;
        for node in self.body.iter() {
            writeln!(f, "{node}")?;
        }
        Ok(())
    }
}

impl Display for IrNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IrNode::Store { index, .. } => write!(f, "\tstore\t{index}"),
            IrNode::Alloc { index, typ } => write!(f, "\talloc\t{index} {typ}"),
            IrNode::Load { value } => write!(f, "\tload\t{value}"),
            IrNode::Pop => write!(f, "\tpop"),
            IrNode::UnaOp { ope, .. } => write!(f, "\tunaop\t{ope:?}"),
            IrNode::BinOp { ope, .. } => write!(f, "\tbinop\t{ope:?}"),
            IrNode::Arg => write!(f, "\targ"),
            IrNode::Call { .. } => write!(f, "\tcall"),
            IrNode::Index { .. } => write!(f, "\tindex"),
            IrNode::Cast { from, to } => write!(f, "\tcast\tfrom {from} to {to}"),
            IrNode::Field { offset, .. } => write!(f, "\tmember {}", offset.len()),
            IrNode::Struct { fields } => write!(f, "\tstruct {}", fields.len()),
            IrNode::SizeOf { typ } => write!(f, "\tsizeof {typ}"),
            IrNode::Inc => write!(f, "\tinc\t"),
            IrNode::Label { label } => write!(f, ".L{label}"),
            IrNode::Jmp { label } => write!(f, "\tjmp\t.L{label}"),
            IrNode::JmpEq { label } => write!(f, "\tjmpeq\t.L{label}"),
            IrNode::JmpFalse { label } => write!(f, "\tjmpfalse .L{label}"),
            IrNode::Return { .. } => write!(f, "\treturn"),
        }
    }
}

impl Display for IrArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IrArg::Var { index, typ } => write!(f, "var {index}: {typ}"),
            IrArg::Literal { value } => write!(f, "literal {value}"),
            IrArg::Global { ns, name, typ } => write!(f, "global {}@{name}: {typ}", *ns),
            IrArg::Extern { name, typ } => write!(f, "extern {name}: {typ}"),
        }
    }
}

impl Display for IrLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IrLiteral::String { label, size: _ } => write!(f, "[{label}]"),
            IrLiteral::Byte(b) => write!(f, "{b}"),
            IrLiteral::Int(i) => write!(f, "{i}"),
            IrLiteral::Float(v) => write!(f, "{v}"),
            IrLiteral::Bool(b) => write!(f, "{b}"),
        }
    }
}
