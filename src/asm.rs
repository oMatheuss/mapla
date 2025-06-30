#![allow(dead_code)]

use std::fmt::{Display, Write};
use std::hash::Hasher;

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
    Jge,
    Jl, // Jump if less than
    Jle,

    Sete,
    Setne,
    Setg,
    Setge,
    Setl,
    Setle,

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

    Movss,  // mov f32 to/from xmm to xmm/memory
    Comiss, // compares f32 and set eflags (branchfull)
    Cmpss,  // xmm1, xmm2/mem, imm (branchless)

    Addss,
    Subss,
    Mulss,
    Divss,

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
            OpCode::Jge => write!(f, "jge"),
            OpCode::Jl => write!(f, "jl"),
            OpCode::Jle => write!(f, "jle"),
            OpCode::Sete => write!(f, "sete"),
            OpCode::Setne => write!(f, "setne"),
            OpCode::Setg => write!(f, "setg"),
            OpCode::Setge => write!(f, "setge"),
            OpCode::Setl => write!(f, "setl"),
            OpCode::Setle => write!(f, "setle"),
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
            OpCode::Movss => write!(f, "movss"),
            OpCode::Comiss => write!(f, "comiss"),
            OpCode::Cmpss => write!(f, "cmpss"),
            OpCode::Addss => write!(f, "addss"),
            OpCode::Subss => write!(f, "subss"),
            OpCode::Mulss => write!(f, "mulss"),
            OpCode::Divss => write!(f, "divss"),
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
    #[inline]
    pub fn acc(mem_size: MemSize) -> Self {
        match mem_size {
            MemSize::Byte => Reg::Al,
            MemSize::Word => Reg::Ax,
            MemSize::DWord => Reg::Eax,
            MemSize::QWord => Reg::Rax,
        }
    }

    #[inline]
    pub fn cnt(mem_size: MemSize) -> Self {
        match mem_size {
            MemSize::Byte => Reg::Cl,
            MemSize::Word => Reg::Cx,
            MemSize::DWord => Reg::Ecx,
            MemSize::QWord => Reg::Rcx,
        }
    }

    #[inline]
    pub fn dta(mem_size: MemSize) -> Self {
        match mem_size {
            MemSize::Byte => Reg::Dl,
            MemSize::Word => Reg::Dx,
            MemSize::DWord => Reg::Edx,
            MemSize::QWord => Reg::Rdx,
        }
    }

    #[inline]
    pub fn bse(mem_size: MemSize) -> Self {
        match mem_size {
            MemSize::Byte => Reg::Bl,
            MemSize::Word => Reg::Bx,
            MemSize::DWord => Reg::Ebx,
            MemSize::QWord => Reg::Rbx,
        }
    }

    #[inline]
    pub fn src(mem_size: MemSize) -> Self {
        match mem_size {
            MemSize::Byte => Reg::Sil,
            MemSize::Word => Reg::Si,
            MemSize::DWord => Reg::Esi,
            MemSize::QWord => Reg::Rsi,
        }
    }

    #[inline]
    pub fn dst(mem_size: MemSize) -> Self {
        match mem_size {
            MemSize::Byte => Reg::Dil,
            MemSize::Word => Reg::Di,
            MemSize::DWord => Reg::Edi,
            MemSize::QWord => Reg::Rdi,
        }
    }

    #[inline]
    pub fn r8(mem_size: MemSize) -> Self {
        match mem_size {
            MemSize::Byte => todo!(),
            MemSize::Word => Reg::R8W,
            MemSize::DWord => Reg::R8D,
            MemSize::QWord => Reg::R8,
        }
    }

    #[inline]
    pub fn r9(mem_size: MemSize) -> Self {
        match mem_size {
            MemSize::Byte => todo!(),
            MemSize::Word => Reg::R9W,
            MemSize::DWord => Reg::R9D,
            MemSize::QWord => Reg::R9,
        }
    }

    #[inline]
    pub fn r10(mem_size: MemSize) -> Self {
        match mem_size {
            MemSize::Byte => todo!(),
            MemSize::Word => Reg::R10W,
            MemSize::DWord => Reg::R10D,
            MemSize::QWord => Reg::R10,
        }
    }

    pub fn is_acc(&self) -> bool {
        matches!(self, Self::Rax | Self::Eax | Self::Ax | Self::Al)
    }
}

impl MemSized for Reg {
    fn mem_size(&self) -> MemSize {
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
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MemSize {
    Byte = 1,
    Word = 2,
    DWord = 4,
    QWord = 8,
}

pub trait MemSized {
    fn mem_size(&self) -> MemSize;
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
pub enum MemBase {
    Reg(Reg),
    Lbl(Lbl),
}

impl Display for MemBase {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MemBase::Reg(reg) => write!(f, "{reg}"),
            MemBase::Lbl(lbl) => write!(f, "rel {lbl}"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Mem {
    size: MemSize,
    base: MemBase,
    index: MemIndex,
    scale: MemScale,
}

impl Mem {
    pub fn reg(reg: Reg, size: MemSize) -> Self {
        Self {
            size,
            base: MemBase::Reg(reg),
            index: MemIndex::None,
            scale: MemScale::Uni,
        }
    }

    pub fn lbl(label: Lbl, size: MemSize) -> Self {
        Self {
            size,
            base: MemBase::Lbl(label),
            index: MemIndex::None,
            scale: MemScale::Uni,
        }
    }

    pub fn offset(reg: Reg, offset: isize, size: MemSize) -> Self {
        Self {
            size,
            base: MemBase::Reg(reg),
            index: MemIndex::Offset(offset),
            scale: MemScale::Uni,
        }
    }
}

impl MemSized for Mem {
    fn mem_size(&self) -> MemSize {
        self.size
    }
}

impl Display for Mem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Mem { size, base, .. } = *self;
        match self.index {
            MemIndex::None => write!(f, "{size} [{base}]"),
            index => match self.scale {
                MemScale::Uni => write!(f, "{size} [{base} {index}]"),
                scale => write!(f, "{size} [{base} {index} {scale}]"),
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
            Imm::Float32(value) => write!(f, "dword 0x{:x}", value.to_bits()),
            Imm::Float64(value) => write!(f, "qword 0x{:x}", value.to_bits()),
        }
    }
}

impl Imm {
    pub const TRUE: Self = Self::Byte(1);
    pub const FALSE: Self = Self::Byte(0);
}

impl MemSized for Imm {
    fn mem_size(&self) -> MemSize {
        match self {
            Imm::Byte(..) | Imm::Char(..) => MemSize::Byte,
            Imm::Int32(..) | Imm::Float32(..) => MemSize::DWord,
            Imm::Int64(..) | Imm::Float64(..) => MemSize::QWord,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum XmmReg {
    Xmm0,
    Xmm1,
    Xmm2,
    Xmm3,
    Xmm4,
    Xmm5,
    Xmm6,
    Xmm7,
}

#[derive(Debug, Clone, Copy)]
pub struct Xmm {
    reg: XmmReg,
    size: MemSize,
    packed: bool,
}

impl Xmm {
    pub fn new(reg: XmmReg, mem_size: MemSize) -> Self {
        Self {
            reg,
            size: mem_size,
            packed: false,
        }
    }

    pub fn xmm0(mem_size: MemSize) -> Self {
        Self::new(XmmReg::Xmm0, mem_size)
    }

    pub fn xmm1(mem_size: MemSize) -> Self {
        Self::new(XmmReg::Xmm1, mem_size)
    }

    pub fn is_xmm0(&self) -> bool {
        matches!(
            self,
            Self {
                reg: XmmReg::Xmm0,
                ..
            }
        )
    }
}

impl MemSized for Xmm {
    fn mem_size(&self) -> MemSize {
        self.size
    }
}

impl Display for Xmm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.reg {
            XmmReg::Xmm0 => write!(f, "xmm0"),
            XmmReg::Xmm1 => write!(f, "xmm1"),
            XmmReg::Xmm2 => write!(f, "xmm2"),
            XmmReg::Xmm3 => write!(f, "xmm3"),
            XmmReg::Xmm4 => write!(f, "xmm4"),
            XmmReg::Xmm5 => write!(f, "xmm5"),
            XmmReg::Xmm6 => write!(f, "xmm6"),
            XmmReg::Xmm7 => write!(f, "xmm7"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Lbl(u64);

impl Lbl {
    pub fn from_str(slice: &str) -> Self {
        let mut state = std::hash::DefaultHasher::new();
        std::hash::Hash::hash_slice(slice.as_bytes(), &mut state);
        Self(state.finish())
    }
}

impl MemSized for Lbl {
    fn mem_size(&self) -> MemSize {
        MemSize::QWord
    }
}

impl Display for Lbl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "D{:x}", self.0)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Operand {
    Reg(Reg),
    Mem(Mem),
    Imm(Imm),
    Xmm(Xmm),
    Lbl(Lbl),
}

impl Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Reg(reg) => reg.fmt(f),
            Self::Mem(mem) => mem.fmt(f),
            Self::Imm(imm) => imm.fmt(f),
            Self::Xmm(xmm) => xmm.fmt(f),
            Self::Lbl(lbl) => lbl.fmt(f),
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

impl From<Xmm> for Operand {
    fn from(value: Xmm) -> Self {
        Self::Xmm(value)
    }
}

impl Operand {
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

    pub fn is_xmm(&self) -> bool {
        match self {
            Operand::Xmm(..) => true,
            _ => false,
        }
    }
}

impl MemSized for Operand {
    fn mem_size(&self) -> MemSize {
        match self {
            Operand::Reg(reg) => reg.mem_size(),
            Operand::Mem(mem) => mem.mem_size(),
            Operand::Imm(imm) => imm.mem_size(),
            Operand::Xmm(xmm) => xmm.mem_size(),
            Operand::Lbl(lbl) => lbl.mem_size(),
        }
    }
}

#[derive(Debug, Default)]
pub struct AsmBuilder(String);

impl AsmBuilder {
    pub fn new() -> Self {
        Self(String::new())
    }

    fn write_fmt(&mut self, args: std::fmt::Arguments<'_>) {
        let _ = self.0.write_fmt(args);
    }

    pub fn append(&mut self, another: Self) {
        self.0.push_str(&another.0);
    }

    pub fn global(&mut self, symbols: &[&str]) {
        writeln!(self, "global {symbols}", symbols = symbols.join(", "));
    }

    pub fn extrn(&mut self, symbols: &[&str]) {
        writeln!(self, "extern {symbols}", symbols = symbols.join(", "));
    }

    pub fn section(&mut self, label: &str) {
        writeln!(self, "section .{label}");
    }

    pub fn label(&mut self, label: &str) {
        writeln!(self, "{label}:");
    }

    pub fn bits(&mut self, bits: u8) {
        writeln!(self, "bits {bits}");
    }

    pub fn db(&mut self, label: &str, bytes: &[u8]) {
        write!(self, "{label}: {} ", OpCode::Db);
        for byte in bytes {
            write!(self, "0x{byte:x},");
        }
        write!(self, "0x00\n");
    }

    pub fn jmp(&mut self, label: &str) {
        writeln!(self, "  {opcode} {label}", opcode = OpCode::Jmp);
    }

    pub fn je(&mut self, label: &str) {
        writeln!(self, "  {opcode} {label}", opcode = OpCode::Je);
    }

    pub fn jne(&mut self, label: &str) {
        writeln!(self, "  {opcode} {label}", opcode = OpCode::Jne);
    }

    pub fn jg(&mut self, label: &str) {
        writeln!(self, "  {opcode} {label}", opcode = OpCode::Jg);
    }

    pub fn jge(&mut self, label: &str) {
        writeln!(self, "  {opcode} {label}", opcode = OpCode::Jge);
    }

    pub fn jl(&mut self, label: &str) {
        writeln!(self, "  {opcode} {label}", opcode = OpCode::Jl);
    }

    pub fn jle(&mut self, label: &str) {
        writeln!(self, "  {opcode} {label}", opcode = OpCode::Jle)
    }

    pub fn sete<T: Into<Operand> + Display>(&mut self, value: T) {
        writeln!(self, "  {opcode} {value}", opcode = OpCode::Sete);
    }

    pub fn setne<T: Into<Operand> + Display>(&mut self, value: T) {
        writeln!(self, "  {opcode} {value}", opcode = OpCode::Setne);
    }

    pub fn setg<T: Into<Operand> + Display>(&mut self, value: T) {
        writeln!(self, "  {opcode} {value}", opcode = OpCode::Setg);
    }

    pub fn setge<T: Into<Operand> + Display>(&mut self, value: T) {
        writeln!(self, "  {opcode} {value}", opcode = OpCode::Setge);
    }

    pub fn setl<T: Into<Operand> + Display>(&mut self, value: T) {
        writeln!(self, "  {opcode} {value}", opcode = OpCode::Setl);
    }

    pub fn setle<T: Into<Operand> + Display>(&mut self, value: T) {
        writeln!(self, "  {opcode} {value}", opcode = OpCode::Setle);
    }

    pub fn inc<T: Into<Operand> + Display>(&mut self, value: T) {
        writeln!(self, "  {opcode} {value}", opcode = OpCode::Inc);
    }

    pub fn dec(&mut self, value: Operand) {
        writeln!(self, "  {opcode} {value}", opcode = OpCode::Dec);
    }

    pub fn mov<T1, T2>(&mut self, value1: T1, value2: T2)
    where
        T1: Into<Operand> + Display,
        T2: Into<Operand> + Display,
    {
        writeln!(self, "  {opcode} {value1}, {value2}", opcode = OpCode::Mov);
    }

    pub fn cmp<T1, T2>(&mut self, value1: T1, value2: T2)
    where
        T1: Into<Operand> + Display,
        T2: Into<Operand> + Display,
    {
        writeln!(self, "  {opcode} {value1}, {value2}", opcode = OpCode::Cmp);
    }

    pub fn lea(&mut self, reg: Reg, mem: Mem) {
        writeln!(self, "  {opcode} {reg}, {mem}", opcode = OpCode::Lea);
    }

    pub fn push<T: Into<Operand> + Display>(&mut self, value: T) {
        writeln!(self, "  {opcode} {value}", opcode = OpCode::Push);
    }

    pub fn pop<T: Into<Operand> + Display>(&mut self, value: T) {
        writeln!(self, "  {opcode} {value}", opcode = OpCode::Pop);
    }

    pub fn call(&mut self, label: &str) {
        writeln!(self, "  {opcode} {label}", opcode = OpCode::Call);
    }

    pub fn ret(&mut self, nbytes: usize) {
        if nbytes == 0 {
            writeln!(self, "  {opcode}", opcode = OpCode::Ret);
        } else {
            writeln!(self, "  {opcode} {nbytes}", opcode = OpCode::Ret);
        }
    }

    pub fn add<T1, T2>(&mut self, value1: T1, value2: T2)
    where
        T1: Into<Operand> + Display,
        T2: Into<Operand> + Display,
    {
        writeln!(self, "  {opcode} {value1}, {value2}", opcode = OpCode::Add);
    }

    pub fn sub<T1, T2>(&mut self, value1: T1, value2: T2)
    where
        T1: Into<Operand> + Display,
        T2: Into<Operand> + Display,
    {
        writeln!(self, "  {opcode} {value1}, {value2}", opcode = OpCode::Sub);
    }

    // Mul,

    pub fn imul<T1, T2>(&mut self, value1: T1, value2: T2)
    where
        T1: Into<Operand> + Display,
        T2: Into<Operand> + Display,
    {
        writeln!(self, "  {opcode} {value1}, {value2}", opcode = OpCode::Imul);
    }

    pub fn div<T: Into<Operand> + Display>(&mut self, value: T) {
        writeln!(self, "  {opcode} {value}", opcode = OpCode::Div);
    }

    pub fn idiv<T: Into<Operand> + Display>(&mut self, value: T) {
        writeln!(self, "  {opcode} {value}", opcode = OpCode::Idiv);
    }

    // Rem,

    pub fn and<T1, T2>(&mut self, value1: T1, value2: T2)
    where
        T1: Into<Operand> + Display,
        T2: Into<Operand> + Display,
    {
        writeln!(self, "  {opcode} {value1}, {value2}", opcode = OpCode::And);
    }

    pub fn or<T1, T2>(&mut self, value1: T1, value2: T2)
    where
        T1: Into<Operand> + Display,
        T2: Into<Operand> + Display,
    {
        writeln!(self, "  {opcode} {value1}, {value2}", opcode = OpCode::Or);
    }

    pub fn not<T: Into<Operand> + Display>(&mut self, value: T) {
        writeln!(self, "  {opcode} {value}", opcode = OpCode::Not);
    }

    pub fn xor<T1, T2>(&mut self, value1: T1, value2: T2)
    where
        T1: Into<Operand> + Display,
        T2: Into<Operand> + Display,
    {
        writeln!(self, "  {opcode} {value1}, {value2}", opcode = OpCode::Xor);
    }

    pub fn shl<T: Into<Operand> + Display>(&mut self, value1: T, value2: Imm) {
        writeln!(self, "  {opcode} {value1}, {value2}", opcode = OpCode::Shl);
    }

    pub fn shl_cl<T: Into<Operand> + Display>(&mut self, value: T) {
        let reg = Reg::Cl;
        writeln!(self, "  {opcode} {value}, {reg}", opcode = OpCode::Shl);
    }

    pub fn shr<T: Into<Operand> + Display>(&mut self, value1: T, value2: Imm) {
        writeln!(self, "  {opcode} {value1}, {value2}", opcode = OpCode::Shr);
    }

    pub fn shr_cl<T: Into<Operand> + Display>(&mut self, value: T) {
        let reg = Reg::Cl;
        writeln!(self, "  {opcode} {value}, {reg}", opcode = OpCode::Shr);
    }

    pub fn neg<T: Into<Operand> + Display>(&mut self, value: T) {
        writeln!(self, "  {opcode} {value}", opcode = OpCode::Neg);
    }

    pub fn movss<T1, T2>(&mut self, value1: T1, value2: T2)
    where
        T1: Into<Operand> + Display,
        T2: Into<Operand> + Display,
    {
        writeln!(
            self,
            "  {opcode} {value1}, {value2}",
            opcode = OpCode::Movss
        );
    }

    pub fn comiss<T1, T2>(&mut self, value1: T1, value2: T2)
    where
        T1: Into<Operand> + Display,
        T2: Into<Operand> + Display,
    {
        writeln!(
            self,
            "  {opcode} {value1}, {value2}",
            opcode = OpCode::Comiss
        );
    }

    pub fn addss<T1, T2>(&mut self, value1: T1, value2: T2)
    where
        T1: Into<Operand> + Display,
        T2: Into<Operand> + Display,
    {
        writeln!(
            self,
            "  {opcode} {value1}, {value2}",
            opcode = OpCode::Addss
        );
    }

    pub fn subss<T1, T2>(&mut self, value1: T1, value2: T2)
    where
        T1: Into<Operand> + Display,
        T2: Into<Operand> + Display,
    {
        writeln!(
            self,
            "  {opcode} {value1}, {value2}",
            opcode = OpCode::Subss
        );
    }

    pub fn mulss<T1, T2>(&mut self, value1: T1, value2: T2)
    where
        T1: Into<Operand> + Display,
        T2: Into<Operand> + Display,
    {
        writeln!(
            self,
            "  {opcode} {value1}, {value2}",
            opcode = OpCode::Mulss
        );
    }

    pub fn divss<T1, T2>(&mut self, value1: T1, value2: T2)
    where
        T1: Into<Operand> + Display,
        T2: Into<Operand> + Display,
    {
        writeln!(
            self,
            "  {opcode} {value1}, {value2}",
            opcode = OpCode::Divss
        );
    }

    pub fn syscall(&mut self) {
        writeln!(self, "  {opcode}", opcode = OpCode::Syscall);
    }

    pub fn nop(&mut self) {
        writeln!(self, "  {opcode}", opcode = OpCode::Nop);
    }

    pub fn push_sf(&mut self) {
        self.push(Reg::Rbp);
        self.mov(Reg::Rbp, Reg::Rsp);
    }

    pub fn pop_sf(&mut self) {
        self.mov(Reg::Rsp, Reg::Rbp);
        self.pop(Reg::Rbp);
    }
}

impl Display for AsmBuilder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}
