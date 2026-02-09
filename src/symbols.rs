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
}

#[derive(Debug, Clone)]
pub struct TypeDef {
    pub pos: Position,
    pub fields: Vec<Argument>,
}

#[derive(Debug, Clone)]
pub struct GlobalVar {
    pub pos: Position,
    pub typ: Type,
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

    pub fn get_var(&self, name: &str, ns: &str) -> Option<&GlobalVar> {
        self.values.get(&SymbolKey {
            name: name.into(),
            namespace: String::from(ns).into(),
        })
    }

    pub fn get_func(&self, name: &str, ns: &str) -> Option<&FuncDef> {
        self.functions.get(&SymbolKey {
            name: name.into(),
            namespace: String::from(ns).into(),
        })
    }

    pub fn get_type(&self, name: &str, ns: &str) -> Option<&TypeDef> {
        self.types.get(&SymbolKey {
            name: name.into(),
            namespace: String::from(ns).into(),
        })
    }

    pub fn iter_extrns(&self) -> impl Iterator<Item = (&SymbolKey, &FuncDef)> {
        self.functions.iter().filter(|f| f.1.extrn)
    }
}

impl std::fmt::Display for SymbolKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", self.namespace, self.name)
    }
}
