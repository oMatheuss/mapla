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

#[derive(Debug, Clone)]
pub struct TypeAnnot {
    pub base: Type,
    pub indir: u8,
    pub array: u32,
}

impl PartialEq for TypeAnnot {
    fn eq(&self, other: &Self) -> bool {
        self.base == other.base && self.indir == other.indir
        // && self.array == other.array
    }
}

impl std::fmt::Display for TypeAnnot {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match &self.base {
            Type::Primitive(p) => &p.to_string(),
            Type::Custom { name, .. } => name.as_ref(),
        };
        write!(f, "{}{e:*>w$}", name, e = "", w = self.indir as usize)
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
            indir: 0,
            array: 0,
        }
    }

    pub fn new(name: impl Into<std::borrow::Cow<'static, str>>, size: usize) -> Self {
        TypeAnnot {
            base: Type::Custom {
                name: name.into(),
                size,
            },
            indir: 0,
            array: 0,
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
        self.indir > 0
    }

    pub fn is_ref(&self) -> bool {
        self.indir == 1
    }

    pub fn is_max_indirection(&self) -> bool {
        self.indir == u8::MAX
    }

    pub fn deref(mut self) -> Self {
        if self.indir > 0 {
            self.indir -= 1;
        }
        self
    }

    pub fn create_ref(mut self) -> Self {
        self.indir += 1;
        self
    }
}

pub trait Annotated {
    fn get_annot(&self) -> TypeAnnot;

    fn name(&self) -> String {
        match &self.get_annot().base {
            Type::Primitive(p) => p.to_string(),
            Type::Custom { name, .. } => name.to_string(),
        }
    }

    fn size(&self) -> usize {
        let annot = self.get_annot();

        if annot.is_ptr() {
            return 8;
        }

        match &annot.base {
            Type::Primitive(p) => match p {
                Primitive::Int => 4,
                Primitive::Real => 4,
                Primitive::Byte => 1,
                Primitive::Char => 1,
                Primitive::Bool => 1,
                Primitive::Void => 0,
            },
            Type::Custom { name: _, size } => *size,
        }
    }
}

impl Annotated for TypeAnnot {
    fn get_annot(&self) -> TypeAnnot {
        self.clone()
    }
}
