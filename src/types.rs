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
    Func(Vec<Argument>, Box<Type>),
    Pointer(Box<Type>),
    Array(Box<Type>, u32),
    Struct(Vec<Argument>),
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

    pub fn is_void(&self) -> bool {
        matches!(self, Type::Void)
    }

    pub fn is_func(&self) -> bool {
        matches!(self, Type::Func(..))
    }

    pub fn is_struct(&self) -> bool {
        matches!(self, Type::Struct(..))
    }

    pub fn is_compatible(&self, other: &Self) -> bool {
        match (self, other) {
            (_, _) if self == other => true,
            (Self::Array(first, ..), Self::Pointer(second)) if *first == *second => true,
            (Self::Pointer(..), second) if second.is_void_ptr() => true,
            (Self::Func(..), second) if second.is_void_ptr() => true,
            _ => false,
        }
    }

    pub fn inner(&self) -> Option<Self> {
        match self {
            Type::Pointer(inner) => Some(*inner.clone()),
            Type::Array(inner, _) => Some(*inner.clone()),
            _ => None,
        }
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
            Self::Func(args, ret) => write!(f, "func({args:?}): {ret}"),
            Self::Pointer(inner) => write!(f, "{inner}*"),
            Self::Array(inner, size) => write!(f, "{inner}[{size}]"),
            Self::Struct(fields) => write!(f, "struct({fields:?})"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

            _ => Error::type_err("invalid operation between types", pos),
        }
    }

    pub fn check_unaexpr(op: UnaOpe, ope: Type, pos: Position) -> Result<Type> {
        match op {
            UnaOpe::AddressOf => Ok(Type::ptr_to(ope)),

            UnaOpe::Minus if ope.is_number() => Ok(ope),
            UnaOpe::Minus => Error::type_err("cannot apply unary minus here", pos),

            UnaOpe::Dereference => match ope {
                Type::Int => Ok(Type::Void),
                Type::Pointer(inner) => Ok(*inner),
                _ => Error::type_err("can only dereference addresses", pos),
            },

            UnaOpe::Not if ope.is_bool() => Ok(ope),
            UnaOpe::Not => Error::type_err("cannot apply unary not here", pos),

            UnaOpe::BitwiseNot if ope.is_int() => Ok(ope),
            UnaOpe::BitwiseNot => Error::type_err("cannot apply bitwise not here", pos),
        }
    }

    pub fn check_cast(from: &Type, to: &Type, pos: Position) -> Result<()> {
        let allowed = from.is_number() && to.is_number()
            || from.is_void_ptr() && to.is_ptr()
            || from.is_ptr() && to.is_void_ptr()
            || from.is_func() && to.is_void_ptr();

        if !allowed {
            let msg = format!("cannot cast from {from} to {to}");
            return Error::type_err(msg, pos);
        }

        Ok(())
    }

    pub fn check_index(array: Type, index: Type, pos: Position) -> Result<Type> {
        let result = match array {
            Type::Int => Type::Void,
            Type::Pointer(inner) => *inner,
            Type::Array(inner, _) => *inner,
            _ => return Error::type_err(format!("cannot index into {array}"), pos),
        };

        if !index.is_int() {
            return Error::type_err("can only index array using int", pos);
        };

        Ok(result)
    }

    pub fn check_callargs(func_args: &Vec<Argument>, args: &Vec<Type>) -> Result<()> {
        let pos = Position::default();
        if func_args.len() != args.len() {
            return Error::type_err("wrong number of arguments provided to the function", pos);
        }

        for (func_arg, arg) in func_args.iter().zip(args) {
            if !arg.is_compatible(&func_arg.arg_type) {
                return Error::type_err("wrong type provided to the function", pos);
            }
        }

        Ok(())
    }
}
