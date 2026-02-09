use crate::ast::{BinOpe, UnaOpe};
use crate::error::{Error, Result};
use crate::position::Position;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Type {
    Int,
    Real,
    Byte,
    Char,
    Bool,
    Void,
    Pointer(Box<Type>),
    Array(Box<Type>, u32),
    Custom(String),
}

impl Type {
    pub fn ptr_to(typ: Self) -> Self {
        Self::Pointer(Box::new(typ))
    }

    pub fn is_void_ptr(&self) -> bool {
        let Type::Pointer(ty) = self else {
            return false;
        };
        matches!(**ty, Type::Void)
    }

    pub fn is_ptr(&self) -> bool {
        matches!(self, Type::Pointer(..) | Type::Array(..))
    }

    pub fn is_array(&self) -> bool {
        matches!(self, Type::Array(..))
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Type::Real)
    }

    pub fn is_int(&self) -> bool {
        matches!(self, Type::Int)
    }

    pub fn is_number(&self) -> bool {
        matches!(self, Type::Int | Type::Real)
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, Type::Bool)
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int => write!(f, "int"),
            Self::Real => write!(f, "real"),
            Self::Byte => write!(f, "byte"),
            Self::Char => write!(f, "char"),
            Self::Bool => write!(f, "bool"),
            Self::Void => write!(f, "void"),
            Self::Pointer(inner) => write!(f, "{inner}*"),
            Self::Array(inner, size) => write!(f, "{inner}[{size}]"),
            Self::Custom(name) => write!(f, "{name}"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Argument {
    pub name: String,
    pub arg_type: Type,
}

impl std::fmt::Display for Argument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self { name, arg_type } = self;
        write!(f, "{name}: {arg_type}")
    }
}

pub struct TypeCheck;

impl TypeCheck {
    pub fn check_binexpr(op: BinOpe, lhs: Type, rhs: Type, pos: Position) -> Result<Type> {
        match op {
            BinOpe::Equal | BinOpe::NotEqual if lhs == rhs => Ok(Type::Bool),

            BinOpe::Greater | BinOpe::GreaterEqual | BinOpe::Less | BinOpe::LessEqual
                if lhs == rhs && lhs.is_number() =>
            {
                Ok(Type::Bool)
            }

            BinOpe::And | BinOpe::Or if lhs == rhs && lhs.is_bool() => Ok(Type::Bool),

            BinOpe::Assign if lhs == rhs => Ok(lhs),
            BinOpe::Assign if lhs.is_ptr() && rhs.is_void_ptr() => Ok(lhs),

            BinOpe::Add
            | BinOpe::Sub
            | BinOpe::Mul
            | BinOpe::Div
            | BinOpe::AddAssign
            | BinOpe::SubAssign
            | BinOpe::MulAssign
            | BinOpe::DivAssign
                if lhs == rhs && lhs.is_number() =>
            {
                Ok(lhs)
            }

            BinOpe::Mod
            | BinOpe::Shr
            | BinOpe::Shl
            | BinOpe::BitwiseAnd
            | BinOpe::BitwiseOr
            | BinOpe::BitwiseXor
                if lhs == rhs && lhs.is_int() =>
            {
                Ok(lhs)
            }

            _ => Error::syntatic("invalid operation between types", pos),
        }
    }

    pub fn check_unaexpr(op: UnaOpe, ope: Type, pos: Position) -> Result<Type> {
        match op {
            UnaOpe::AddressOf => Ok(Type::ptr_to(ope)),

            UnaOpe::Minus if ope.is_number() => Ok(ope),
            UnaOpe::Minus => Error::syntatic("cannot apply unary minus here", pos),

            UnaOpe::Dereference => match ope {
                Type::Int => Ok(Type::Void),
                Type::Pointer(inner) => Ok(*inner),
                _ => Error::syntatic("can only dereference addresses", pos),
            },

            UnaOpe::Not if ope.is_bool() => Ok(ope),
            UnaOpe::Not => Error::syntatic("cannot apply unary not here", pos),

            UnaOpe::BitwiseNot if ope.is_int() => Ok(ope),
            UnaOpe::BitwiseNot => Error::syntatic("cannot apply bitwise not here", pos),
        }
    }

    pub fn check_cast(from: &Type, to: &Type, pos: Position) -> Result<()> {
        if from.is_number() && to.is_number() {
            Ok(())
        } else if from.is_void_ptr() && to.is_ptr() {
            Ok(())
        } else {
            let msg = format!("cannot cast from {from} to {to}");
            Error::syntatic(msg, pos)
        }
    }

    pub fn check_index(array: Type, index: Type, pos: Position) -> Result<Type> {
        let result = match array {
            Type::Int => Type::Void,
            Type::Pointer(inner) => *inner,
            Type::Array(inner, _) => *inner,
            _ => return Error::syntatic(format!("cannot index into {array}"), pos),
        };

        if !index.is_int() {
            return Error::syntatic("can only index array using int", pos);
        };

        Ok(result)
    }
}
