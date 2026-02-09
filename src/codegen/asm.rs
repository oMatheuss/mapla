#![allow(dead_code)]

use std::fmt::{Display, Write};

pub enum OpCode {
    Db,
    Dw,
    Dd,
    Dq,
    Dt,

    Jmp,
    Je,
    Jne,
    Jg,
    Jge,
    Jl,
    Jle,

    Sete,
    Setne,
    Setg,
    Setge,
    Setl,
    Setle,

    Seta,
    Setae,
    Setb,
    Setbe,

    Inc,
    Dec,
    Mov,
    Movd,
    Movq,
    Cmp,
    Lea,

    Push,
    Pop,
    Call,
    Ret,

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
    Xor,
    Shl,
    Shr,
    Neg,

    Movss,
    Comiss,
    Cmpss,

    Addss,
    Subss,
    Mulss,
    Divss,

    Cvtss2si,
    Cvtsi2ss,

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
            OpCode::Seta => write!(f, "seta"),
            OpCode::Setae => write!(f, "setae"),
            OpCode::Setb => write!(f, "setb"),
            OpCode::Setbe => write!(f, "setbe"),
            OpCode::Inc => write!(f, "inc"),
            OpCode::Dec => write!(f, "dec"),
            OpCode::Mov => write!(f, "mov"),
            OpCode::Movd => write!(f, "movd"),
            OpCode::Movq => write!(f, "movq"),
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
            OpCode::Cvtss2si => write!(f, "cvtss2si"),
            OpCode::Cvtsi2ss => write!(f, "cvtsi2ss"),
            OpCode::Syscall => write!(f, "syscall"),
            OpCode::Nop => write!(f, "nop"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
#[rustfmt::skip]
pub enum Reg {
    Rax, Eax, Ax, Ah, Al,
    Rcx, Ecx, Cx, Ch, Cl,
    Rdx, Edx, Dx, Dh, Dl,
    Rbx, Ebx, Bx, Bh, Bl,
    Rsp, Esp, Sp, Spl,
    Rbp, Ebp, Bp, Bpl,
    Rsi, Esi, Si, Sil,
    Rdi, Edi, Di, Dil,
    R8,  R8D, R8W, R8B,
    R9,  R9D, R9W, R9B,
    R10, R10D, R10W, R10B,
    R11, R11D, R11W, R11B,
    R12, R12D, R12W, R12B,
    R13, R13D, R13W, R13B,
    R14, R14D, R14W, R14B,
    R15, R15D, R15W, R15B,
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
            Reg::R8B => write!(f, "r8b"),

            Reg::R9 => write!(f, "r9"),
            Reg::R9D => write!(f, "r9d"),
            Reg::R9W => write!(f, "r9w"),
            Reg::R9B => write!(f, "r9b"),

            Reg::R10 => write!(f, "r10"),
            Reg::R10D => write!(f, "r10d"),
            Reg::R10W => write!(f, "r10w"),
            Reg::R10B => write!(f, "r10b"),

            Reg::R11 => write!(f, "r11"),
            Reg::R11D => write!(f, "r11d"),
            Reg::R11W => write!(f, "r11w"),
            Reg::R11B => write!(f, "r11b"),

            Reg::R12 => write!(f, "r12"),
            Reg::R12D => write!(f, "r12d"),
            Reg::R12W => write!(f, "r12w"),
            Reg::R12B => write!(f, "r12b"),

            Reg::R13 => write!(f, "r13"),
            Reg::R13D => write!(f, "r13d"),
            Reg::R13W => write!(f, "r13w"),
            Reg::R13B => write!(f, "r13b"),

            Reg::R14 => write!(f, "r14"),
            Reg::R14D => write!(f, "r14d"),
            Reg::R14W => write!(f, "r14w"),
            Reg::R14B => write!(f, "r14b"),

            Reg::R15 => write!(f, "r15"),
            Reg::R15D => write!(f, "r15d"),
            Reg::R15W => write!(f, "r15w"),
            Reg::R15B => write!(f, "r15b"),
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
            MemSize::Byte => Reg::R8B,
            MemSize::Word => Reg::R8W,
            MemSize::DWord => Reg::R8D,
            MemSize::QWord => Reg::R8,
        }
    }

    #[inline]
    pub fn r9(mem_size: MemSize) -> Self {
        match mem_size {
            MemSize::Byte => Reg::R9B,
            MemSize::Word => Reg::R9W,
            MemSize::DWord => Reg::R9D,
            MemSize::QWord => Reg::R9,
        }
    }

    #[inline]
    pub fn r10(mem_size: MemSize) -> Self {
        match mem_size {
            MemSize::Byte => Reg::R9B,
            MemSize::Word => Reg::R10W,
            MemSize::DWord => Reg::R10D,
            MemSize::QWord => Reg::R10,
        }
    }

    pub fn is_reserved(&self) -> bool {
        matches!(self, Self::Rsp | Self::Rbp)
    }

    pub fn is_acc(&self) -> bool {
        matches!(self, Self::Rax | Self::Eax | Self::Ax | Self::Al)
    }

    pub fn is_cnt(&self) -> bool {
        matches!(self, Self::Rcx | Self::Ecx | Self::Cx | Self::Cl)
    }

    pub fn is_dta(&self) -> bool {
        matches!(self, Self::Rdx | Self::Edx | Self::Dx | Self::Dl)
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
            | Reg::Dil
            | Reg::R8B
            | Reg::R9B
            | Reg::R10B
            | Reg::R11B
            | Reg::R12B
            | Reg::R13B
            | Reg::R14B
            | Reg::R15B => MemSize::Byte,
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

impl TryFrom<usize> for MemSize {
    type Error = usize;

    fn try_from(value: usize) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(Self::Byte),
            2 => Ok(Self::DWord),
            4 => Ok(Self::DWord),
            8 => Ok(Self::QWord),
            _ => Err(value),
        }
    }
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

    pub fn base(&self) -> MemBase {
        self.base
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
    Dword(u32),
    Qword(u64),
}

impl Display for Imm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Imm::Byte(value) => write!(f, "byte 0x{value:x}"),
            Imm::Dword(value) => write!(f, "dword 0x{value:x}"),
            Imm::Qword(value) => write!(f, "qword 0x{value:x}"),
        }
    }
}

impl Imm {
    pub const TRUE: Self = Self::Byte(1);
    pub const FALSE: Self = Self::Byte(0);

    pub fn from_i32(v: i32) -> Self {
        Self::Dword(v as u32)
    }

    pub fn from_f32(v: f32) -> Self {
        Self::Dword(v.to_bits())
    }
}

impl MemSized for Imm {
    fn mem_size(&self) -> MemSize {
        match self {
            Imm::Byte(..) => MemSize::Byte,
            Imm::Dword(..) => MemSize::DWord,
            Imm::Qword(..) => MemSize::QWord,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
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
    pub(crate) reg: XmmReg,
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

    pub fn xmm(n: i32, mem_size: MemSize) -> Self {
        let xmm = match n {
            0 => XmmReg::Xmm0,
            1 => XmmReg::Xmm1,
            2 => XmmReg::Xmm2,
            3 => XmmReg::Xmm3,
            4 => XmmReg::Xmm4,
            5 => XmmReg::Xmm5,
            6 => XmmReg::Xmm6,
            7 => XmmReg::Xmm7,
            _ => panic!("xmm register does not exist"),
        };
        Self::new(xmm, mem_size)
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

impl PartialEq for Xmm {
    fn eq(&self, other: &Self) -> bool {
        self.reg == other.reg
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
pub struct Lbl(usize);

thread_local! {
    static LABELS: std::cell::RefCell<Vec<String>> = const { std::cell::RefCell::new(Vec::new()) };
}

impl Lbl {
    pub fn from_label(label: &str) -> Self {
        LABELS.with_borrow_mut(|labels| {
            if let Some((id, _)) = labels.iter().enumerate().find(|(_, l)| *l == label) {
                Self(id)
            } else {
                let id = labels.len();
                labels.push(String::from(label));
                Self(id)
            }
        })
    }
}

impl MemSized for Lbl {
    fn mem_size(&self) -> MemSize {
        MemSize::QWord
    }
}

impl Display for Lbl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        LABELS.with_borrow(|labels| write!(f, "{}", labels[self.0]))
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Operand {
    Reg(Reg),
    Mem(Mem),
    Imm(Imm),
    Xmm(Xmm),
}

impl Default for Operand {
    fn default() -> Self {
        Self::Imm(Imm::TRUE)
    }
}

impl Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Reg(reg) => reg.fmt(f),
            Self::Mem(mem) => mem.fmt(f),
            Self::Imm(imm) => imm.fmt(f),
            Self::Xmm(xmm) => xmm.fmt(f),
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

    pub fn expect_reg(self) -> Reg {
        match self {
            Operand::Reg(reg) => reg,
            _ => panic!("expected that operand was a register, but it was: {self}"),
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
        }
    }
}

macro_rules! code {
    ($builder:expr, $opcode:ident) => {{
        use $crate::codegen::asm::OpCode::$opcode;
        ::core::writeln!($builder, "  {}", $opcode);
    }};

    ($builder:expr, $opcode:ident, $dst:expr) => {{
        use $crate::codegen::asm::OpCode::$opcode;
        ::core::writeln!($builder, "  {} {}", $opcode, $dst);
    }};

    ($builder:expr, $opcode:ident, $dst:expr, $src:expr) => {{
        use $crate::codegen::asm::OpCode::$opcode;
        ::core::writeln!($builder, "  {} {}, {}", $opcode, $dst, $src);
    }};

    ($builder:expr, $($arg:tt)*) => {
        ::core::writeln!($builder, $($arg)*)
    };
}

pub(crate) use code;

#[derive(Debug, Default)]
pub struct AsmBuilder(String);

impl AsmBuilder {
    pub fn new() -> Self {
        Self(String::new())
    }

    pub fn write_fmt(&mut self, args: std::fmt::Arguments<'_>) {
        let _ = self.0.write_fmt(args);
    }

    pub fn append(&mut self, another: Self) {
        self.0.push_str(&another.0);
    }
}

impl Display for AsmBuilder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}
