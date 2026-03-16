use crate::target::CompilerTarget;

use super::asm::{MemBase, MemSize, Operand, Reg, Xmm, XmmReg};
use std::fmt::Display;

#[derive(Debug)]
pub struct RegManager<T> {
    registers: std::collections::HashMap<&'static str, bool>,

    // could be PhantomInvariant, but is unstable
    panthon: std::marker::PhantomData<T>,
}

pub trait OperandManager<T>
where
    T: Copy + Display,
{
    fn volatiles(target: CompilerTarget) -> &'static [&'static str];
    fn name(ope: T) -> &'static str;
    fn get_reg(name: &str, mem_size: MemSize) -> T;
    fn try_push(&mut self, operand: Operand);
}

impl<T> RegManager<T>
where
    RegManager<T>: OperandManager<T>,
    T: Copy + Display,
{
    pub fn new(target: CompilerTarget) -> Self {
        let volatiles = Self::volatiles(target);
        let mut registers = std::collections::HashMap::with_capacity(volatiles.len());
        for reg in volatiles {
            registers.insert(*reg, false);
        }
        Self {
            registers,
            panthon: std::marker::PhantomData,
        }
    }

    pub fn push(&mut self, ope: T) {
        match self.registers.get_mut(Self::name(ope)) {
            Some(false) => {}
            Some(in_use) => *in_use = false,
            None => panic!("register does not exists"),
        };
    }

    pub fn take(&mut self, ope: T) -> bool {
        match self.registers.get_mut(Self::name(ope)) {
            Some(in_use @ false) => *in_use = true,
            Some(true) | None => return false,
        };
        return true;
    }

    pub fn take_any(&mut self, mem_size: MemSize) -> T {
        let reg = self
            .registers
            .iter_mut()
            .find(|r| !*r.1)
            .expect("register available");

        *reg.1 = true;
        Self::get_reg(reg.0, mem_size)
    }

    pub fn ensure(&self, ope: T) -> bool {
        match self.registers.get(Self::name(ope)) {
            Some(false) => true,
            Some(true) | None => false,
        }
    }

    pub fn switch_size(&self, ope: T, mem_size: MemSize) -> T {
        Self::get_reg(Self::name(ope), mem_size)
    }
}

impl OperandManager<Reg> for RegManager<Reg> {
    fn volatiles(target: CompilerTarget) -> &'static [&'static str] {
        match target {
            CompilerTarget::Linux => &["acc", "cnt", "dta", "src", "dst", "r8", "r9", "r10", "r11"],
            CompilerTarget::Windows => &["acc", "cnt", "dta", "r8", "r9", "r10", "r11"],
        }
    }

    fn name(reg: Reg) -> &'static str {
        match reg {
            Reg::Rax | Reg::Eax | Reg::Ax | Reg::Ah | Reg::Al => "acc",
            Reg::Rcx | Reg::Ecx | Reg::Cx | Reg::Ch | Reg::Cl => "cnt",
            Reg::Rdx | Reg::Edx | Reg::Dx | Reg::Dh | Reg::Dl => "dta",
            Reg::Rbx | Reg::Ebx | Reg::Bx | Reg::Bh | Reg::Bl => "bse",
            Reg::Rsp | Reg::Esp | Reg::Sp | Reg::Spl => todo!(),
            Reg::Rbp | Reg::Ebp | Reg::Bp | Reg::Bpl => todo!(),
            Reg::Rsi | Reg::Esi | Reg::Si | Reg::Sil => "src",
            Reg::Rdi | Reg::Edi | Reg::Di | Reg::Dil => "dst",
            Reg::R8 | Reg::R8D | Reg::R8W | Reg::R8B => "r8",
            Reg::R9 | Reg::R9D | Reg::R9W | Reg::R9B => "r9",
            Reg::R10 | Reg::R10D | Reg::R10W | Reg::R10B => "r10",
            Reg::R11 | Reg::R11D | Reg::R11W | Reg::R11B => "r11",
            Reg::R12 | Reg::R12D | Reg::R12W | Reg::R12B => "r12",
            Reg::R13 | Reg::R13D | Reg::R13W | Reg::R13B => "r13",
            Reg::R14 | Reg::R14D | Reg::R14W | Reg::R14B => "r14",
            Reg::R15 | Reg::R15D | Reg::R15W | Reg::R15B => "r15",
        }
    }

    fn get_reg(name: &str, mem_size: MemSize) -> Reg {
        match name {
            "acc" => Reg::acc(mem_size),
            "cnt" => Reg::cnt(mem_size),
            "dta" => Reg::dta(mem_size),
            "bse" => Reg::bse(mem_size),
            "src" => Reg::src(mem_size),
            "dst" => Reg::dst(mem_size),
            "r8" => Reg::r8(mem_size),
            "r9" => Reg::r9(mem_size),
            "r10" => Reg::r10(mem_size),
            "r11" => Reg::r11(mem_size),
            _ => panic!("register does not exists"),
        }
    }

    fn try_push(&mut self, operand: Operand) {
        match operand {
            Operand::Reg(reg) => self.push(reg),
            Operand::Mem(mem) => match mem.base() {
                MemBase::Reg(reg) => match reg {
                    Reg::Rbp | Reg::Rsp => {}
                    _ => self.push(reg),
                },
                MemBase::Lbl(..) => {}
            },
            _ => {}
        }
    }
}

impl OperandManager<Xmm> for RegManager<Xmm> {
    fn volatiles(target: CompilerTarget) -> &'static [&'static str] {
        match target {
            CompilerTarget::Linux => &[
                "xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7",
            ],
            CompilerTarget::Windows => &["xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5"],
        }
    }

    fn name(ope: Xmm) -> &'static str {
        match ope.reg {
            XmmReg::Xmm0 => "xmm0",
            XmmReg::Xmm1 => "xmm1",
            XmmReg::Xmm2 => "xmm2",
            XmmReg::Xmm3 => "xmm3",
            XmmReg::Xmm4 => "xmm4",
            XmmReg::Xmm5 => "xmm5",
            XmmReg::Xmm6 => "xmm6",
            XmmReg::Xmm7 => "xmm7",
        }
    }

    fn get_reg(name: &str, mem_size: MemSize) -> Xmm {
        match name {
            "xmm0" => Xmm::xmm(0, mem_size),
            "xmm1" => Xmm::xmm(1, mem_size),
            "xmm2" => Xmm::xmm(2, mem_size),
            "xmm3" => Xmm::xmm(3, mem_size),
            "xmm4" => Xmm::xmm(4, mem_size),
            "xmm5" => Xmm::xmm(5, mem_size),
            "xmm6" => Xmm::xmm(6, mem_size),
            "xmm7" => Xmm::xmm(7, mem_size),
            _ => panic!("unknown register"),
        }
    }

    fn try_push(&mut self, operand: Operand) {
        match operand {
            Operand::Xmm(xmm) => self.push(xmm),
            _ => {}
        }
    }
}
