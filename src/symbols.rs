use std::collections::HashMap;
use std::rc::Rc;

use crate::{
    error::{Error, Result},
    position::Position,
    types::{Argument, Type},
};

#[derive(Debug, Clone)]
pub struct FuncDef {
    pub pos: Position,
    pub args: Vec<Argument>,
    pub ret: Type,
    pub extrn: bool,
    pub variadic: bool,
}

impl FuncDef {
    pub fn as_type(self) -> Type {
        Type::Func(self.args.into(), Box::new(self.ret), self.variadic)
    }
}

#[derive(Debug, Clone)]
pub struct TypeDef {
    pub pos: Position,
    pub fields: Vec<Argument>,
}

impl TypeDef {
    pub fn as_type(self) -> Type {
        Type::Struct(self.fields.into())
    }
}

#[derive(Debug, Clone)]
pub struct GlobalVar {
    pub pos: Position,
    pub typ: Type,
    pub extrn: bool,
}

#[derive(Debug, Clone)]
pub enum SymbolValue {
    FuncDef(FuncDef),
    TypeDef(TypeDef),
    GlobalVar(GlobalVar),
}

impl SymbolValue {
    pub fn as_type(self) -> Type {
        match self {
            SymbolValue::FuncDef(func_def) => func_def.as_type(),
            SymbolValue::TypeDef(type_def) => type_def.as_type(),
            SymbolValue::GlobalVar(global_var) => global_var.typ,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct SymbolKey {
    pub name: String,
    pub namespace: Rc<String>,
}

#[derive(Debug, Default)]
pub struct SymbolTable {
    values: HashMap<SymbolKey, GlobalVar>,
    types: HashMap<SymbolKey, TypeDef>,
    functions: HashMap<SymbolKey, FuncDef>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self::default()
    }

    fn err_if_exists(&self, name: &str, ns: Rc<String>) -> Result<SymbolKey> {
        let key = SymbolKey {
            name: name.into(),
            namespace: ns,
        };

        match self.values.get(&key) {
            Some(sym) => Error::syntatic("symbol already defined here", sym.pos.clone())?,
            None => {}
        }

        match self.types.get(&key) {
            Some(sym) => Error::syntatic("symbol already defined here", sym.pos.clone())?,
            None => {}
        }

        match self.functions.get(&key) {
            Some(sym) => Error::syntatic("symbol already defined here", sym.pos.clone())?,
            None => {}
        }

        Ok(key)
    }

    pub fn set_var(&mut self, name: &str, ns: Rc<String>, symbol: GlobalVar) -> Result<()> {
        let key = self.err_if_exists(name, ns)?;
        let _ = self.values.insert(key, symbol);
        Ok(())
    }

    pub fn set_type(&mut self, name: &str, ns: Rc<String>, symbol: TypeDef) -> Result<()> {
        let key = self.err_if_exists(name, ns)?;
        let _ = self.types.insert(key, symbol);
        Ok(())
    }

    pub fn set_func(&mut self, name: &str, ns: Rc<String>, symbol: FuncDef) -> Result<()> {
        let key = self.err_if_exists(name, ns)?;
        let _ = self.functions.insert(key, symbol);
        Ok(())
    }

    pub fn get(&self, name: &str, ns: &str) -> Option<SymbolValue> {
        let key = SymbolKey {
            name: name.into(),
            namespace: String::from(ns).into(),
        };
        if let Some(value) = self.values.get(&key) {
            Some(SymbolValue::GlobalVar(value.clone()))
        } else if let Some(value) = self.functions.get(&key) {
            Some(SymbolValue::FuncDef(value.clone()))
        } else if let Some(value) = self.types.get(&key) {
            Some(SymbolValue::TypeDef(value.clone()))
        } else {
            None
        }
    }

    pub fn iter_extrns(&self) -> impl Iterator<Item = &SymbolKey> {
        let vars = self.values.iter().filter(|f| f.1.extrn).map(|f| f.0);
        let funcs = self.functions.iter().filter(|f| f.1.extrn).map(|f| f.0);
        vars.chain(funcs)
    }
}

impl std::fmt::Display for SymbolKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", self.namespace, self.name)
    }
}
