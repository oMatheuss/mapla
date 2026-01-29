#![allow(dead_code)]

use std::borrow::Cow;

#[derive(Debug, Clone, PartialEq)]
pub enum Primitive {
    Int,
    Real,
    Byte,
    Char,
    Bool,
    Void,
}

impl std::fmt::Display for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Primitive::Int => write!(f, "int"),
            Primitive::Real => write!(f, "real"),
            Primitive::Byte => write!(f, "byte"),
            Primitive::Char => write!(f, "char"),
            Primitive::Bool => write!(f, "bool"),
            Primitive::Void => write!(f, "void"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Primitive(Primitive),
    Custom {
        name: Cow<'static, str>,
        size: usize,
    },
}

impl Type {
    pub fn new_custom(name: impl Into<std::borrow::Cow<'static, str>>, size: usize) -> Self {
        Self::Custom {
            name: name.into(),
            size,
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeMeta {
    Value,
    Array(u32),
    Pointer(u8),
}

impl TypeMeta {
    pub fn is_value(&self) -> bool {
        matches!(self, TypeMeta::Value)
    }

    pub fn is_ptr(&self) -> bool {
        matches!(self, TypeMeta::Array(..) | TypeMeta::Pointer(..))
    }

    pub fn is_array(&self) -> bool {
        matches!(self, TypeMeta::Array(..))
    }

    pub fn indirection(&self) -> u8 {
        match self {
            TypeMeta::Value => 0,
            TypeMeta::Array(_) => 1,
            TypeMeta::Pointer(p) => *p,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeAnnot {
    pub base: Type,
    pub meta: TypeMeta,
}

impl PartialEq for TypeAnnot {
    fn eq(&self, other: &Self) -> bool {
        self.base == other.base && self.meta.indirection() == other.meta.indirection()
        // && self.array == other.array
    }
}

impl std::fmt::Display for TypeAnnot {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match &self.base {
            Type::Primitive(p) => &p.to_string(),
            Type::Custom { name, .. } => name.as_ref(),
        };
        let indir = self.meta.indirection() as usize;
        write!(f, "{}{e:*>w$}", name, e = "", w = indir)
    }
}

impl TypeAnnot {
    pub const INT: Self = TypeAnnot::new_const(Primitive::Int);
    pub const REAL: Self = TypeAnnot::new_const(Primitive::Real);
    pub const BYTE: Self = TypeAnnot::new_const(Primitive::Byte);
    pub const CHAR: Self = TypeAnnot::new_const(Primitive::Char);
    pub const BOOL: Self = TypeAnnot::new_const(Primitive::Bool);
    pub const VOID: Self = TypeAnnot::new_const(Primitive::Void);

    const fn new_const(base: Primitive) -> Self {
        TypeAnnot {
            base: Type::Primitive(base),
            meta: TypeMeta::Value,
        }
    }

    pub const fn new_value(base: Type) -> Self {
        TypeAnnot {
            base,
            meta: TypeMeta::Value,
        }
    }

    pub const fn new_array(base: Type, size: u32) -> Self {
        TypeAnnot {
            base,
            meta: TypeMeta::Array(size),
        }
    }

    pub const fn new_ptr(base: Type, indir: u8) -> Self {
        TypeAnnot {
            base,
            meta: TypeMeta::Pointer(indir),
        }
    }

    pub const fn new_string(size: u32) -> Self {
        TypeAnnot {
            base: Type::Primitive(Primitive::Char),
            meta: TypeMeta::Array(size),
        }
    }

    pub fn is_bool(&self) -> bool {
        *self == TypeAnnot::BOOL
    }

    pub fn is_byte(&self) -> bool {
        *self == TypeAnnot::BYTE
    }

    pub fn is_number(&self) -> bool {
        *self == TypeAnnot::INT || *self == TypeAnnot::REAL
    }

    pub fn is_int(&self) -> bool {
        *self == TypeAnnot::INT
    }

    pub fn is_float(&self) -> bool {
        *self == TypeAnnot::REAL
    }

    pub fn is_void(&self) -> bool {
        *self == TypeAnnot::VOID
    }

    pub fn is_void_ptr(&self) -> bool {
        matches!(self.base, Type::Primitive(Primitive::Void)) && self.is_ref()
    }

    pub fn is_byte_ptr(&self) -> bool {
        matches!(self.base, Type::Primitive(Primitive::Byte)) && self.is_ref()
    }

    pub fn is_ptr(&self) -> bool {
        self.meta.is_ptr()
    }

    pub fn is_ref(&self) -> bool {
        self.meta.indirection() == 1
    }

    pub fn is_array(&self) -> bool {
        self.meta.is_array()
    }

    pub fn is_max_indirection(&self) -> bool {
        self.meta.indirection() == u8::MAX
    }

    pub fn deref(self) -> Self {
        match self.meta {
            TypeMeta::Value => self,
            TypeMeta::Array(..) => Self::new_value(self.base),
            TypeMeta::Pointer(i) if i > 1 => Self::new_ptr(self.base, i - 1),
            TypeMeta::Pointer(..) => Self::new_value(self.base),
        }
    }

    pub fn create_ref(self) -> Self {
        match self.meta {
            TypeMeta::Value => Self::new_ptr(self.base, 1),
            TypeMeta::Array(..) => Self::new_ptr(self.base, 2),
            TypeMeta::Pointer(i) => Self::new_ptr(self.base, i + 1),
        }
    }
}

pub trait Annotated {
    fn get_annot(&self) -> TypeAnnot;

    fn type_name(&self) -> String {
        match &self.get_annot().base {
            Type::Primitive(p) => p.to_string(),
            Type::Custom { name, .. } => name.to_string(),
        }
    }

    fn is_primitive(&self) -> bool {
        matches!(&self.get_annot().base, Type::Primitive(..))
    }

    fn type_size(&self) -> usize {
        let annot = self.get_annot();

        let size = match &annot.base {
            Type::Primitive(p) => match p {
                Primitive::Int => 4,
                Primitive::Real => 4,
                Primitive::Byte => 1,
                Primitive::Char => 1,
                Primitive::Bool => 1,
                Primitive::Void => 0,
            },
            Type::Custom { name: _, size } => *size,
        };

        match annot.meta {
            TypeMeta::Value => size,
            TypeMeta::Array(array_size) => size * array_size as usize,
            TypeMeta::Pointer(_) => 8,
        }
    }
}

impl Annotated for TypeAnnot {
    fn get_annot(&self) -> TypeAnnot {
        self.clone()
    }
}
