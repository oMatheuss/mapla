use std::fmt::Display;

pub enum OpCode {
    Db, // Declare byte (1 byte)
    Dw, // Declare word (2 bytes)
    Dd, // Declare double word (4 bytes)
    Dq, // Declare quad word (8 bytes)
    Dt, // Declare ten word (10 bytes)

    Jmp, // Jump incondicional
    Je,  // Jump if equal
    Jne, // Jump if not equal
    Jg,  // Jump if greater than
    Jl,  // Jump if less than

    Sete,
    Setne,
    Setg,
    Setl,

    Inc, // Increment one
    Dec, // Decrement one
    Mov, // Move x into y
    Cmp, // Compare two values
    Lea, // Load effective address

    Push, // Push from reg/mem into stack
    Pop,  // Pop value from the stack into reg/mem
    Call, // Push next address then jumps to label,
    Ret,  // Pop value and jumps to it

    Add,
    Sub,
    Mul,
    Imul,
    Div,
    Idiv,
    Rem,
    And,
    Or,
    Not,
    Xor, // Exclusive or
    Shl, // Shift left
    Shr, // Shift right
    Neg, // Negate

    Syscall,
    Nop,
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpCode::Db => write!(f, "db"),
            OpCode::Dw => write!(f, "dw"),
            OpCode::Dd => write!(f, "dd"),
            OpCode::Dq => write!(f, "dq"),
            OpCode::Dt => write!(f, "dt"),
            OpCode::Jmp => write!(f, "jmp"),
            OpCode::Je => write!(f, "je"),
            OpCode::Jne => write!(f, "jne"),
            OpCode::Jg => write!(f, "jg"),
            OpCode::Jl => write!(f, "jl"),
            OpCode::Sete => write!(f, "sete"),
            OpCode::Setne => write!(f, "sete"),
            OpCode::Setg => write!(f, "setg"),
            OpCode::Setl => write!(f, "setl"),
            OpCode::Inc => write!(f, "inc"),
            OpCode::Dec => write!(f, "dec"),
            OpCode::Mov => write!(f, "mov"),
            OpCode::Cmp => write!(f, "cmp"),
            OpCode::Lea => write!(f, "lea"),
            OpCode::Push => write!(f, "push"),
            OpCode::Pop => write!(f, "pop"),
            OpCode::Call => write!(f, "call"),
            OpCode::Ret => write!(f, "ret"),
            OpCode::Add => write!(f, "add"),
            OpCode::Sub => write!(f, "sub"),
            OpCode::Mul => write!(f, "mul"),
            OpCode::Imul => write!(f, "imul"),
            OpCode::Div => write!(f, "div"),
            OpCode::Idiv => write!(f, "idiv"),
            OpCode::Rem => write!(f, "rem"),
            OpCode::And => write!(f, "and"),
            OpCode::Or => write!(f, "or"),
            OpCode::Not => write!(f, "not"),
            OpCode::Xor => write!(f, "xor"),
            OpCode::Shl => write!(f, "shl"),
            OpCode::Shr => write!(f, "shr"),
            OpCode::Neg => write!(f, "neg"),
            OpCode::Syscall => write!(f, "syscall"),
            OpCode::Nop => write!(f, "nop"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Reg {
    // accumulator
    Rax,
    Eax,
    Ax,
    Ah,
    Al,

    // counter
    Rcx,
    Ecx,
    Cx,
    Ch,
    Cl,

    // data
    Rdx,
    Edx,
    Dx,
    Dh,
    Dl,

    // base
    Rbx,
    Ebx,
    Bx,
    Bh,
    Bl,

    // stack pointer
    Rsp,
    Esp,
    Sp,
    Spl,

    // base pointer
    Rbp,
    Ebp,
    Bp,
    Bpl,

    // source index
    Rsi,
    Esi,
    Si,
    Sil,

    // destination index
    Rdi,
    Edi,
    Di,
    Dil,

    R8,
    R8D,
    R8W,

    R9,
    R9D,
    R9W,

    R10,
    R10D,
    R10W,

    R11,
    R11D,
    R11W,

    R12,
    R12D,
    R12W,

    R13,
    R13D,
    R13W,

    R14,
    R14D,
    R14W,

    R15,
    R15D,
    R15W,
}

impl Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Reg::Rax => write!(f, "rax"),
            Reg::Eax => write!(f, "eax"),
            Reg::Ax => write!(f, "ax"),
            Reg::Ah => write!(f, "ah"),
            Reg::Al => write!(f, "al"),

            Reg::Ecx => write!(f, "ecx"),
            Reg::Rcx => write!(f, "rcx"),
            Reg::Cx => write!(f, "cx"),
            Reg::Ch => write!(f, "ch"),
            Reg::Cl => write!(f, "cl"),

            Reg::Rdx => write!(f, "rdx"),
            Reg::Edx => write!(f, "edx"),
            Reg::Dx => write!(f, "dx"),
            Reg::Dh => write!(f, "dh"),
            Reg::Dl => write!(f, "dl"),

            Reg::Rbx => write!(f, "rbx"),
            Reg::Ebx => write!(f, "ebx"),
            Reg::Bx => write!(f, "bx"),
            Reg::Bh => write!(f, "bh"),
            Reg::Bl => write!(f, "bl"),

            Reg::Rsp => write!(f, "rsp"),
            Reg::Esp => write!(f, "esp"),
            Reg::Sp => write!(f, "sp"),
            Reg::Spl => write!(f, "spl"),

            Reg::Rbp => write!(f, "rbp"),
            Reg::Ebp => write!(f, "ebp"),
            Reg::Bp => write!(f, "bp"),
            Reg::Bpl => write!(f, "bpl"),

            Reg::Rsi => write!(f, "rsi"),
            Reg::Esi => write!(f, "esi"),
            Reg::Si => write!(f, "si"),
            Reg::Sil => write!(f, "sil"),

            Reg::Rdi => write!(f, "rdi"),
            Reg::Edi => write!(f, "edi"),
            Reg::Di => write!(f, "sdi"),
            Reg::Dil => write!(f, "dil"),

            Reg::R8 => write!(f, "r8"),
            Reg::R8D => write!(f, "r8d"),
            Reg::R8W => write!(f, "r8w"),

            Reg::R9 => write!(f, "r9"),
            Reg::R9D => write!(f, "r9d"),
            Reg::R9W => write!(f, "r9w"),

            Reg::R10 => write!(f, "r10"),
            Reg::R10D => write!(f, "r10d"),
            Reg::R10W => write!(f, "r10w"),

            Reg::R11 => write!(f, "r11"),
            Reg::R11D => write!(f, "r11d"),
            Reg::R11W => write!(f, "r11w"),

            Reg::R12 => write!(f, "r12"),
            Reg::R12D => write!(f, "r12d"),
            Reg::R12W => write!(f, "r12w"),

            Reg::R13 => write!(f, "r13"),
            Reg::R13D => write!(f, "r13d"),
            Reg::R13W => write!(f, "r13w"),

            Reg::R14 => write!(f, "r14"),
            Reg::R14D => write!(f, "r14d"),
            Reg::R14W => write!(f, "r14w"),

            Reg::R15 => write!(f, "r15"),
            Reg::R15D => write!(f, "r15d"),
            Reg::R15W => write!(f, "r15w"),
        }
    }
}

impl Reg {
    pub fn mem_size(&self) -> MemSize {
        match self {
            Reg::Rax
            | Reg::Rcx
            | Reg::Rdx
            | Reg::Rbx
            | Reg::Rsp
            | Reg::Rbp
            | Reg::Rsi
            | Reg::Rdi
            | Reg::R8
            | Reg::R9
            | Reg::R10
            | Reg::R11
            | Reg::R12
            | Reg::R13
            | Reg::R14
            | Reg::R15 => MemSize::QWord,

            Reg::Eax
            | Reg::Ecx
            | Reg::Edx
            | Reg::Ebx
            | Reg::Esp
            | Reg::Ebp
            | Reg::Esi
            | Reg::Edi
            | Reg::R8D
            | Reg::R9D
            | Reg::R10D
            | Reg::R11D
            | Reg::R12D
            | Reg::R13D
            | Reg::R14D
            | Reg::R15D => MemSize::DWord,

            Reg::Ax
            | Reg::Cx
            | Reg::Dx
            | Reg::Bx
            | Reg::Sp
            | Reg::Bp
            | Reg::Si
            | Reg::Di
            | Reg::R8W
            | Reg::R9W
            | Reg::R10W
            | Reg::R11W
            | Reg::R12W
            | Reg::R13W
            | Reg::R14W
            | Reg::R15W => MemSize::Word,

            Reg::Ah
            | Reg::Al
            | Reg::Ch
            | Reg::Cl
            | Reg::Dh
            | Reg::Dl
            | Reg::Bh
            | Reg::Bl
            | Reg::Spl
            | Reg::Bpl
            | Reg::Sil
            | Reg::Dil => MemSize::Byte,
        }
    }

    pub fn get_a(mem_size: MemSize) -> Reg {
        match mem_size {
            MemSize::Byte => Reg::Al,
            MemSize::Word => Reg::Ax,
            MemSize::DWord => Reg::Eax,
            MemSize::QWord => Reg::Rax,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MemSize {
    Byte = 1,
    Word = 2,
    DWord = 4,
    QWord = 8,
}

impl Display for MemSize {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Byte => write!(f, "byte"),
            Self::Word => write!(f, "word"),
            Self::DWord => write!(f, "dword"),
            Self::QWord => write!(f, "qword"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum MemIndex {
    None,
    Offset(isize),
    ByReg(Reg),
}

impl Display for MemIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MemIndex::None => Ok(()),
            MemIndex::Offset(offset) => {
                if *offset > 0 {
                    write!(f, "+ {offset}")
                } else {
                    write!(f, "- {offset}", offset = offset.abs())
                }
            }
            MemIndex::ByReg(reg) => write!(f, "+ {reg}"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum MemScale {
    Uni,
    Duo,
    Quadri,
    Octo,
}

impl Display for MemScale {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MemScale::Uni => write!(f, "* 1"),
            MemScale::Duo => write!(f, "* 2"),
            MemScale::Quadri => write!(f, "* 4"),
            MemScale::Octo => write!(f, "* 8"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Mem {
    Abs {
        size: MemSize,
        addr: usize,
    },
    Rel {
        size: MemSize,
        base: Reg,
        index: MemIndex,
        scale: MemScale,
    },
}

impl Mem {
    pub fn abs(addr: usize, size: MemSize) -> Self {
        Self::Abs { size, addr }
    }

    pub fn reg(reg: Reg, size: MemSize) -> Self {
        Self::Rel {
            size,
            base: reg,
            index: MemIndex::None,
            scale: MemScale::Uni,
        }
    }

    pub fn offset(reg: Reg, offset: isize, size: MemSize) -> Self {
        Self::Rel {
            size,
            base: reg,
            index: MemIndex::Offset(offset),
            scale: MemScale::Uni,
        }
    }

    pub fn mem_size(&self) -> MemSize {
        match self {
            Mem::Abs { size, addr: _ } => *size,
            Mem::Rel {
                size,
                base: _,
                index: _,
                scale: _,
            } => *size,
        }
    }
}

impl Display for Mem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Mem::Abs { size, addr } => write!(f, "{size} [{addr:#X}]"),
            Mem::Rel {
                size,
                base,
                index,
                scale,
            } => match index {
                MemIndex::None => write!(f, "{size} [{base}]"),
                _ => match scale {
                    MemScale::Uni => write!(f, "{size} [{base} {index}]"),
                    _ => write!(f, "{size} [{base} {index} {scale}]"),
                },
            },
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Imm {
    Byte(u8),
    Char(char),
    Int32(i32),
    Int64(i64),
    Float32(f32),
    Float64(f64),
}

impl Display for Imm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Imm::Byte(value) => write!(f, "byte {value}"),
            Imm::Char(value) => write!(f, "byte '{value}'"),
            Imm::Int32(value) => write!(f, "dword {value}"),
            Imm::Int64(value) => write!(f, "qword {value}"),
            Imm::Float32(value) => write!(f, "dword {value}"),
            Imm::Float64(value) => write!(f, "qword {value}"),
        }
    }
}

impl Imm {
    pub const TRUE: Self = Self::Byte(1);
    pub const FALSE: Self = Self::Byte(0);

    pub fn mem_size(&self) -> MemSize {
        match self {
            Imm::Byte(..) | Imm::Char(..) => MemSize::Byte,
            Imm::Int32(..) | Imm::Float32(..) => MemSize::DWord,
            Imm::Int64(..) | Imm::Float64(..) => MemSize::QWord,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Operand {
    Reg(Reg),
    Mem(Mem),
    Imm(Imm),
}

impl Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Reg(reg) => reg.fmt(f),
            Self::Mem(mem) => mem.fmt(f),
            Self::Imm(imm) => imm.fmt(f),
        }
    }
}

impl From<Reg> for Operand {
    fn from(value: Reg) -> Self {
        Self::Reg(value)
    }
}

impl From<Mem> for Operand {
    fn from(value: Mem) -> Self {
        Self::Mem(value)
    }
}

impl From<Imm> for Operand {
    fn from(value: Imm) -> Self {
        Self::Imm(value)
    }
}

impl Operand {
    pub fn mem_size(&self) -> MemSize {
        match self {
            Operand::Reg(reg) => reg.mem_size(),
            Operand::Mem(mem) => mem.mem_size(),
            Operand::Imm(imm) => imm.mem_size(),
        }
    }

    pub fn is_reg(&self) -> bool {
        match self {
            Operand::Reg(..) => true,
            _ => false,
        }
    }

    pub fn is_mem(&self) -> bool {
        match self {
            Operand::Mem(..) => true,
            _ => false,
        }
    }

    pub fn is_imm(&self) -> bool {
        match self {
            Operand::Imm(..) => true,
            _ => false,
        }
    }
}

pub struct AsmBuilder {
    builder: String,
}

impl AsmBuilder {
    pub fn new() -> Self {
        Self {
            builder: String::new(),
        }
    }

    pub fn global(&mut self, symbols: &[&str]) {
        let line = format!("global {symbols}\n", symbols = symbols.join(", "));
        self.builder.push_str(&line);
    }

    pub fn section(&mut self, label: &str) {
        let line = format!("section .{label}\n");
        self.builder.push_str(&line);
    }

    pub fn label(&mut self, label: &str) {
        let line = format!("{label}:\n");
        self.builder.push_str(&line);
    }

    pub fn bits(&mut self, bits: u8) {
        let line = format!("bits {bits}\n");
        self.builder.push_str(&line);
    }

    pub fn db(&mut self, label: &str, value: u8) {
        let line = format!("  {label}: {opcode} {value}\n", opcode = OpCode::Db);
        self.builder.push_str(&line);
    }

    pub fn dw(&mut self, label: &str, value: u16) {
        let line = format!("  {label}: {opcode} {value}\n", opcode = OpCode::Dw);
        self.builder.push_str(&line);
    }

    pub fn dd(&mut self, label: &str, value: u32) {
        let line = format!("  {label}: {opcode} {value}\n", opcode = OpCode::Dd);
        self.builder.push_str(&line);
    }

    pub fn dq(&mut self, label: &str, value: u64) {
        let line = format!("  {label}: {opcode} {value}\n", opcode = OpCode::Dq);
        self.builder.push_str(&line);
    }

    pub fn dt(&mut self, label: &str, value: [u8; 10]) {
        todo!()
    }

    pub fn jmp(&mut self, label: &str) {
        let line = format!("  {opcode} {label}\n", opcode = OpCode::Jmp);
        self.builder.push_str(&line);
    }

    pub fn je(&mut self, label: &str) {
        let line = format!("  {opcode} {label}\n", opcode = OpCode::Je);
        self.builder.push_str(&line);
    }

    pub fn jne(&mut self, label: &str) {
        let line = format!("  {opcode} {label}\n", opcode = OpCode::Jne);
        self.builder.push_str(&line);
    }

    pub fn jg(&mut self, label: &str) {
        let line = format!("  {opcode} {label}\n", opcode = OpCode::Jg);
        self.builder.push_str(&line);
    }

    pub fn jl(&mut self, label: &str) {
        let line = format!("  {opcode} {label}\n", opcode = OpCode::Jl);
        self.builder.push_str(&line);
    }

    pub fn sete<T: Into<Operand> + Display>(&mut self, value: T) {
        let line = format!("  {opcode} {value}\n", opcode = OpCode::Sete);
        self.builder.push_str(&line);
    }

    pub fn setne<T: Into<Operand> + Display>(&mut self, value: T) {
        let line = format!("  {opcode} {value}\n", opcode = OpCode::Setne);
        self.builder.push_str(&line);
    }

    pub fn setg<T: Into<Operand> + Display>(&mut self, value: T) {
        let line = format!("  {opcode} {value}\n", opcode = OpCode::Setg);
        self.builder.push_str(&line);
    }

    pub fn setl<T: Into<Operand> + Display>(&mut self, value: T) {
        let line = format!("  {opcode} {value}\n", opcode = OpCode::Setl);
        self.builder.push_str(&line);
    }

    pub fn inc<T: Into<Operand> + Display>(&mut self, value: T) {
        let line = format!("  {opcode} {value}\n", opcode = OpCode::Inc);
        self.builder.push_str(&line);
    }

    pub fn dec(&mut self, value: Operand) {
        let line = format!("  {opcode} {value}\n", opcode = OpCode::Dec);
        self.builder.push_str(&line);
    }

    pub fn mov<T1, T2>(&mut self, value1: T1, value2: T2)
    where
        T1: Into<Operand> + Display,
        T2: Into<Operand> + Display,
    {
        let line = format!("  {opcode} {value1}, {value2}\n", opcode = OpCode::Mov);
        self.builder.push_str(&line);
    }

    pub fn cmp<T1, T2>(&mut self, value1: T1, value2: T2)
    where
        T1: Into<Operand> + Display,
        T2: Into<Operand> + Display,
    {
        let line = format!("  {opcode} {value1}, {value2}\n", opcode = OpCode::Cmp);
        self.builder.push_str(&line);
    }

    pub fn lea(&mut self, reg: Reg, mem: Mem) {
        let line = format!("  {opcode} {reg} {mem}\n", opcode = OpCode::Lea);
        self.builder.push_str(&line);
    }

    pub fn push<T: Into<Operand> + Display>(&mut self, value: T) {
        let line = format!("  {opcode} {value}\n", opcode = OpCode::Push);
        self.builder.push_str(&line);
    }

    pub fn pop<T: Into<Operand> + Display>(&mut self, value: T) {
        let line = format!("  {opcode} {value}\n", opcode = OpCode::Pop);
        self.builder.push_str(&line);
    }

    pub fn call(&mut self, label: &str) {
        let line = format!("  {opcode} {label}\n", opcode = OpCode::Call);
        self.builder.push_str(&line);
    }

    pub fn ret(&mut self, nbytes: usize) {
        let line = format!("  {opcode} {nbytes}\n", opcode = OpCode::Ret);
        self.builder.push_str(&line);
    }

    pub fn add<T1, T2>(&mut self, value1: T1, value2: T2)
    where
        T1: Into<Operand> + Display,
        T2: Into<Operand> + Display,
    {
        let line = format!("  {opcode} {value1}, {value2}\n", opcode = OpCode::Add);
        self.builder.push_str(&line);
    }

    pub fn sub<T1, T2>(&mut self, value1: T1, value2: T2)
    where
        T1: Into<Operand> + Display,
        T2: Into<Operand> + Display,
    {
        let line = format!("  {opcode} {value1}, {value2}\n", opcode = OpCode::Sub);
        self.builder.push_str(&line);
    }

    // Mul,

    pub fn imul<T1, T2>(&mut self, value1: T1, value2: T2)
    where
        T1: Into<Operand> + Display,
        T2: Into<Operand> + Display,
    {
        let line = format!("  {opcode} {value1}, {value2}\n", opcode = OpCode::Imul);
        self.builder.push_str(&line);
    }

    pub fn div<T: Into<Operand> + Display>(&mut self, value: T) {
        let line = format!("  {opcode} {value}\n", opcode = OpCode::Div);
        self.builder.push_str(&line);
    }

    pub fn idiv<T: Into<Operand> + Display>(&mut self, value: T) {
        let line = format!("  {opcode} {value}\n", opcode = OpCode::Idiv);
        self.builder.push_str(&line);
    }

    // Rem,

    pub fn and<T1, T2>(&mut self, value1: T1, value2: T2)
    where
        T1: Into<Operand> + Display,
        T2: Into<Operand> + Display,
    {
        let line = format!("  {opcode} {value1}, {value2}\n", opcode = OpCode::And);
        self.builder.push_str(&line);
    }

    pub fn or<T1, T2>(&mut self, value1: T1, value2: T2)
    where
        T1: Into<Operand> + Display,
        T2: Into<Operand> + Display,
    {
        let line = format!("  {opcode} {value1}, {value2}\n", opcode = OpCode::Or);
        self.builder.push_str(&line);
    }

    // Not,

    pub fn xor<T1, T2>(&mut self, value1: T1, value2: T2)
    where
        T1: Into<Operand> + Display,
        T2: Into<Operand> + Display,
    {
        let line = format!("  {opcode} {value1}, {value2}\n", opcode = OpCode::Xor);
        self.builder.push_str(&line);
    }

    // Shl, // Shift left
    // Shr, // Shift right
    // Neg, // Negate

    pub fn syscall(&mut self) {
        let line = format!("  {opcode}\n", opcode = OpCode::Syscall);
        self.builder.push_str(&line);
    }

    pub fn nop(&mut self) {
        let line = format!("  {opcode}\n", opcode = OpCode::Nop);
        self.builder.push_str(&line);
    }

    pub fn push_sf(&mut self) {
        self.push(Reg::Rbp);
        self.mov(Reg::Rbp, Reg::Rsp);
    }

    pub fn pop_sf(&mut self) {
        self.mov(Reg::Rsp, Reg::Rbp);
        self.pop(Reg::Rbp);
    }

    pub fn sys_exit(&mut self, code: i64) {
        self.mov(Reg::Rax, Imm::Int64(60));
        self.mov(Reg::Rdi, Imm::Int64(code));
        self.syscall();
    }

    pub fn sys_write<T1, T2>(&mut self, buffer: T1, nbytes: T2)
    where
        T1: Into<Operand> + Display,
        T2: Into<Operand> + Display,
    {
        self.mov(Reg::Rax, Imm::Int64(1));
        self.mov(Reg::Rdi, Imm::Int64(1));
        self.mov(Reg::Rsi, buffer);
        self.mov(Reg::Rdx, nbytes);
        self.syscall();
    }
}

impl Display for AsmBuilder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.builder.fmt(f)
    }
}
