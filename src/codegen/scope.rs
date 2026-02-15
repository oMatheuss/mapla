use super::asm::{Mem, MemSize, Operand, Reg};

#[derive(Debug, Default)]
pub struct Scope {
    // actual stack offset of local variables
    local_off: isize,

    // actual stack offset of temp variables
    temp_off: isize,

    // max stack size needed for a function call
    max_func: usize,

    // max stack offset needed temp variables
    max_temp_off: isize,

    // array with local variables (usize -> Operand)
    locals: Vec<Option<Mem>>,

    // expression stack
    stack: Vec<Operand>,
}

impl Scope {
    pub fn set_fixed_var(&mut self, index: usize, mem: Mem) {
        push_set(&mut self.locals, index, mem.into());
    }

    pub fn set_sized_var(&mut self, index: usize, size: usize, mem_size: MemSize) -> Operand {
        self.local_off -= size as isize;
        let mem = Mem::offset(Reg::Rbp, self.local_off, mem_size);
        push_set(&mut self.locals, index, mem.into());
        Operand::Mem(mem)
    }

    pub fn set_var(&mut self, index: usize, mem_size: MemSize) -> Operand {
        self.set_sized_var(index, mem_size as usize, mem_size)
    }

    pub fn get_var(&self, index: usize) -> Option<Operand> {
        let mem = self.locals.get(index)?.clone()?;
        Some(mem.into())
    }

    pub fn new_sized_temp(&mut self, size: usize, mem_size: MemSize) -> Operand {
        self.temp_off -= size as isize;
        if self.temp_off < self.max_temp_off {
            self.max_temp_off = self.temp_off;
        }
        let mem = Mem::offset(Reg::Rbp, self.local_off + self.temp_off, mem_size);
        mem.into()
    }

    pub fn new_temp(&mut self, mem_size: MemSize) -> Operand {
        self.new_sized_temp(mem_size as usize, mem_size)
    }

    pub fn push(&mut self, ope: Operand) {
        self.stack.push(ope);
    }

    pub fn push_var(&mut self, index: usize) -> Option<()> {
        self.push(self.get_var(index)?);
        Some(())
    }

    pub fn pop(&mut self) -> Option<Operand> {
        self.stack.pop()
    }

    pub fn new_call(&mut self, call_size: usize) {
        if call_size > self.max_func {
            self.max_func = call_size;
        }
    }

    pub fn reset(&mut self) {
        *self = Self::default();
    }

    pub fn reset_temps(&mut self) {
        assert!(self.stack.is_empty());
        self.temp_off = 0;
    }

    pub fn get_max(&self) -> u64 {
        self.local_off.abs() as u64 + self.max_temp_off.abs() as u64 + self.max_func as u64
    }
}

fn push_set<T: Default>(vec: &mut Vec<T>, index: usize, item: T) {
    if index >= vec.len() {
        for _ in vec.len()..=index {
            vec.push(T::default());
        }
    }
    vec[index] = item;
}
