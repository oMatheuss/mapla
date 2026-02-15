use crate::codegen::asm;
use crate::codegen::asm::{Imm, Mem, MemSize, MemSized, Operand, Reg, Xmm};
use crate::codegen::regs::OperandManager;
use crate::types::{Argument, Type};

use super::CodeGen;

fn call_regs(reg_num: usize, mem_size: MemSize) -> Option<Reg> {
    let reg = match reg_num {
        0 => Reg::cnt(mem_size),
        1 => Reg::dta(mem_size),
        2 => Reg::r8(mem_size),
        3 => Reg::r9(mem_size),
        _ => return None,
    };
    Some(reg)
}

fn call_xmms(xmm_num: usize, mem_size: MemSize) -> Option<Xmm> {
    let xmm = match xmm_num {
        0 => Xmm::xmm(0, mem_size),
        1 => Xmm::xmm(1, mem_size),
        2 => Xmm::xmm(2, mem_size),
        3 => Xmm::xmm(3, mem_size),
        _ => return None,
    };
    Some(xmm)
}

fn release_call_regs(c: &mut CodeGen) {
    let mut i = 0;
    while let Some(reg) = call_regs(i, MemSize::QWord) {
        c.regs.push(reg);
        i += 1;
    }
    i = 0;
    while let Some(xmm) = call_xmms(i, MemSize::QWord) {
        c.xmms.push(xmm);
        i += 1;
    }
}

pub fn compile_call(
    c: &mut CodeGen,
    name: Operand,
    args: Vec<(Operand, Type)>,
    typ: &Type,
) -> Operand {
    let stack_space = 32 + 0.max(args.len() as isize - 4) as usize * MemSize::QWord as usize;
    c.scope.new_call(stack_space);

    let mut i = 0;
    let mut offset = 32;
    let mut arg_iter = args.iter();

    if c.type_size(typ) > 8 {
        let reg = call_regs(i, MemSize::QWord).unwrap();
        let mem = c.scope.new_sized_temp(c.type_size(typ), MemSize::QWord);
        asm::code!(c.code, Lea, reg, mem);
        i += 1;
    }

    while let Some((arg, typ)) = arg_iter.next() {
        let mem_size = arg.mem_size();
        let is_float = matches!(typ, Type::Real);

        if is_float && let Some(xmm) = call_xmms(i, mem_size) {
            assert!(c.xmms.take(xmm), "arg register (xmm) should be free");
            match arg {
                Operand::Xmm(arg) if *arg == xmm => {}
                Operand::Imm(imm) => {
                    let reg = c.regs.take_any(imm.mem_size());
                    asm::code!(c.code, Mov, reg, imm);
                    asm::code!(c.code, Movd, xmm, reg);
                    c.regs.push(reg);
                }
                Operand::Xmm(..) | Operand::Mem(..) => {
                    asm::code!(c.code, Movss, xmm, arg);
                }
                Operand::Reg(reg) => asm::code!(c.code, Movd, xmm, reg),
                Operand::Lbl(lbl) => {
                    let reg = c.regs.take_any(lbl.mem_size());
                    asm::code!(c.code, Lea, reg, lbl);
                    asm::code!(c.code, Movq, xmm, reg);
                    c.regs.push(reg);
                }
            }
        } else if !is_float && let Some(reg) = call_regs(i, mem_size) {
            assert!(c.regs.take(reg), "arg register should be free");
            match arg {
                Operand::Reg(arg) if reg == *arg => {}
                Operand::Xmm(xmm) => asm::code!(c.code, Movd, reg, xmm),
                Operand::Lbl(lbl) => asm::code!(c.code, Lea, reg, lbl),
                _ => asm::code!(c.code, Mov, reg, arg),
            }
        } else {
            let mem = Mem::offset(Reg::Rsp, offset, mem_size);
            offset += MemSize::QWord as isize;
            match arg {
                Operand::Xmm(xmm) => asm::code!(c.code, Movss, mem, xmm),
                Operand::Reg(reg) => asm::code!(c.code, Mov, mem, reg),
                Operand::Mem(..) => {
                    let reg = c.regs.take_any(mem_size);
                    asm::code!(c.code, Mov, reg, arg);
                    asm::code!(c.code, Mov, mem, reg);
                    c.regs.push(reg);
                }
                Operand::Lbl(..) => {
                    let reg = c.regs.take_any(mem_size);
                    asm::code!(c.code, Lea, reg, arg);
                    asm::code!(c.code, Mov, mem, reg);
                    c.regs.push(reg);
                }
                _ => asm::code!(c.code, Mov, mem, arg),
            }
        }

        c.regs.try_push(*arg);
        c.xmms.try_push(*arg);

        i += 1;
    }

    asm::code!(c.code, Call, name);
    release_call_regs(c);

    match typ {
        _ if c.type_size(typ) > 8 => {
            let reg = Reg::acc(MemSize::QWord);
            assert!(c.regs.take(reg), "return register should be available");
            Operand::Reg(reg)
        }
        Type::Real => {
            let xmm = Xmm::xmm0(MemSize::DWord);
            assert!(c.xmms.take(xmm), "return register should be available");
            Operand::Xmm(xmm)
        }
        Type::Void => Operand::Imm(Imm::Dword(0)),
        _ => {
            let mem_size = c.type_size(typ).try_into().unwrap();
            let reg = Reg::acc(mem_size);
            assert!(c.regs.take(reg), "return register should be available");
            Operand::Reg(reg)
        }
    }
}

pub fn compile_args(c: &mut CodeGen, args: &[Argument], fn_type: &Type) {
    let mut i = 0;
    let mut mem_offset = 48; // rip + rbp + shadow space
    if c.type_size(fn_type) > 8 {
        let reg = call_regs(i, MemSize::QWord).unwrap();
        let mem = c.scope.set_sized_var(i, 8, MemSize::QWord);
        asm::code!(c.code, Mov, mem, reg);
        i += 1; // first register will be allocated to the return value
    }
    for Argument { name: _, arg_type } in args.iter() {
        let size = c.type_size(arg_type);
        let mem_size = size.try_into().unwrap();
        let is_float = matches!(arg_type, Type::Real);
        if is_float && let Some(xmm) = call_xmms(i, mem_size) {
            let mem = c.scope.set_sized_var(i, size, mem_size);
            asm::code!(c.code, Movss, mem, xmm);
        } else if !is_float && let Some(reg) = call_regs(i, mem_size) {
            let mem = c.scope.set_sized_var(i, size, mem_size);
            asm::code!(c.code, Mov, mem, reg);
        } else {
            let mem = Mem::offset(Reg::Rbp, mem_offset, mem_size);
            mem_offset += MemSize::QWord as isize;
            c.scope.set_fixed_var(i, mem);
        }
        i += 1;
    }
}

pub fn compile_ret_struct(c: &mut CodeGen, ret: Operand, size: usize) {
    assert!(ret.is_mem());
    let dst = Reg::acc(MemSize::QWord);
    // assume the return address is the first variable in this stack frame
    let mem = Mem::offset(Reg::Rbp, -8, MemSize::QWord);
    asm::code!(c.code, Mov, dst, mem);
    let src = Reg::src(MemSize::QWord);
    asm::code!(c.code, Lea, src, ret);
    c.copy_bytes_inline(dst, src, Reg::Rcx, size);
}
