use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::asm::{Mem, MemSize, Operand, Reg};

#[derive(Debug, Clone, Copy, Default)]
pub struct ScopeContext {
    // actual stack offset of local variables
    pub local_off: isize,

    // actual stack offset of temp variables
    pub temp_off: isize,

    // max stack size needed for a function call
    pub max_func: usize,

    // max stack offset needed temp variables
    pub max_temp_off: isize,

    // label count used for jumps
    pub lbl_count: usize,
}

impl ScopeContext {
    pub fn get_max(&self) -> u64 {
        self.local_off.abs() as u64 + self.max_temp_off.abs() as u64 + self.max_func as u64
    }
}

#[derive(Debug, Default)]
pub struct Scope {
    pub var: HashMap<String, Mem>,
    pub ctx: Rc<RefCell<ScopeContext>>,
    pub sup: Option<Box<Scope>>,
}

impl Scope {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn new_inner(&mut self) {
        let cur = std::mem::take(self);
        let mut src = Self::new();
        src.sup = Some(Box::new(cur));
        let _ = std::mem::replace(self, src);
    }

    pub fn continue_on(&mut self) {
        let cur = std::mem::take(self);
        let src = Self {
            var: HashMap::new(),
            ctx: cur.ctx.clone(),
            sup: Some(Box::new(cur)),
        };
        let _ = std::mem::replace(self, src);
    }

    pub fn exit(&mut self) {
        let cur = std::mem::take(self);
        let outer = cur.sup.expect("at least global scope should exist");
        let _ = std::mem::replace(self, *outer);
    }

    pub fn get(&self, ident: &str) -> Mem {
        if let Some(local) = self.var.get(ident) {
            *local
        } else if let Some(sup) = &self.sup {
            sup.get(ident)
        } else {
            panic!("'{ident}' was not found: variable does not exist")
        }
    }

    #[inline]
    pub fn set(&mut self, ident: &str, operand: Mem) -> Option<Mem> {
        self.var.insert(String::from(ident), operand)
    }

    pub fn new_local(&mut self, ident: &str, mem_size: MemSize) -> Operand {
        let mut mem = self.ctx.borrow_mut();
        mem.local_off -= mem_size as isize;
        let mem = Mem::offset(Reg::Rbp, mem.local_off, mem_size);
        self.var.insert(String::from(ident), mem);
        Operand::Mem(mem)
    }

    pub fn new_array(&mut self, ident: &str, size: u32, mem_size: MemSize) -> Operand {
        let mut mem = self.ctx.borrow_mut();
        mem.local_off -= mem_size as isize * size as isize;
        let mem = Mem::offset(Reg::Rbp, mem.local_off, MemSize::QWord);
        self.var.insert(String::from(ident), mem);
        Operand::Mem(mem)
    }

    pub fn alloc(&mut self, size: usize) -> isize {
        let mut mem = self.ctx.borrow_mut();
        mem.temp_off -= size as isize;
        if mem.temp_off < mem.max_temp_off {
            mem.max_temp_off = mem.temp_off;
        }
        mem.local_off + mem.temp_off
    }

    pub fn new_temp(&mut self, mem_size: MemSize) -> Mem {
        let offset = self.alloc(mem_size as usize);
        Mem::offset(Reg::Rbp, offset, mem_size)
    }

    pub fn new_call(&mut self, call_size: usize) {
        let mut mem = self.ctx.borrow_mut();
        if call_size > mem.max_func {
            mem.max_func = call_size;
        }
    }

    #[inline]
    pub fn reset_temps(&mut self) {
        self.ctx.borrow_mut().temp_off = 0;
    }

    #[inline]
    pub fn new_label(&mut self) -> String {
        let mut ctx = self.ctx.borrow_mut();
        ctx.lbl_count += 1;
        format!(".L{cnt}", cnt = ctx.lbl_count)
    }
}
