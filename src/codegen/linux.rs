use crate::codegen::asm;
use crate::codegen::asm::{Imm, Mem, MemSize, MemSized, Operand, Reg, Xmm};
use crate::codegen::regs::OperandManager;
use crate::types::{Argument, Type};

use super::CodeGen;

fn cdecl(arg_num: usize, mem_size: MemSize) -> Option<Reg> {
    let reg = match arg_num {
        0 => Reg::dst(mem_size),
        1 => Reg::src(mem_size),
        2 => Reg::dta(mem_size),
        3 => Reg::cnt(mem_size),
        4 => Reg::r8(mem_size),
        5 => Reg::r9(mem_size),
        _ => return None,
    };
    Some(reg)
}

fn cdecl_f(arg_num: usize, mem_size: MemSize) -> Option<Xmm> {
    let xmm = match arg_num {
        0 => Xmm::xmm(0, mem_size),
        1 => Xmm::xmm(1, mem_size),
        2 => Xmm::xmm(2, mem_size),
        3 => Xmm::xmm(3, mem_size),
        4 => Xmm::xmm(4, mem_size),
        5 => Xmm::xmm(5, mem_size),
        6 => Xmm::xmm(6, mem_size),
        7 => Xmm::xmm(7, mem_size),
        _ => return None,
    };
    Some(xmm)
}

pub fn compile_call(
    c: &mut CodeGen,
    name: Operand,
    args: Vec<(Operand, Type)>,
    typ: &Type,
) -> Operand {
    let ptr_size = MemSize::QWord as isize;
    let mut stack_offset = 0.max(args.len() as isize - 6) * ptr_size;

    c.scope.new_call(stack_offset as usize);

    if let Type::Struct(..) = typ {
        todo!("implement return value greater than 8 bytes");
    }

    for (arg_num, (arg, typ)) in args.iter().enumerate().rev() {
        let mem_size = arg.mem_size();
        let is_float = matches!(typ, Type::Real);

        if let Some(reg) = cdecl(arg_num, mem_size) {
            assert!(c.regs.take(reg), "arg register should be free");
            match arg {
                Operand::Reg(arg) if reg == *arg => {}
                Operand::Xmm(xmm) => asm::code!(c.code, Movd, reg, xmm),
                Operand::Lbl(lbl) => asm::code!(c.code, Lea, reg, Mem::lbl(*lbl, MemSize::QWord)),
                _ => asm::code!(c.code, Mov, reg, arg),
            }
        } else {
            stack_offset -= ptr_size;
            let mem = Mem::offset(Reg::Rsp, stack_offset, mem_size);

            match arg {
                Operand::Xmm(xmm) => asm::code!(c.code, Movss, mem, xmm),
                Operand::Reg(reg) => asm::code!(c.code, Mov, mem, reg),
                Operand::Mem(..) => {
                    let reg = c.regs.take_any(mem_size);
                    asm::code!(c.code, Mov, reg, arg);
                    asm::code!(c.code, Mov, mem, reg);
                    c.regs.push(reg);
                }
                Operand::Lbl(lbl) => {
                    let reg = c.regs.take_any(mem_size);
                    asm::code!(c.code, Lea, reg, Mem::lbl(*lbl, MemSize::QWord));
                    asm::code!(c.code, Mov, mem, reg);
                    c.regs.push(reg);
                }
                _ => asm::code!(c.code, Mov, mem, arg),
            }
        }

        if is_float && let Some(xmm) = cdecl_f(arg_num, mem_size) {
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
                    let reg = c.regs.take_any(mem_size);
                    asm::code!(c.code, Lea, reg, Mem::lbl(*lbl, MemSize::QWord));
                    asm::code!(c.code, Movq, xmm, reg);
                    c.regs.push(reg);
                }
            }
        }

        c.regs.try_push(*arg);
        c.xmms.try_push(*arg);
    }

    asm::code!(c.code, Call, name);

    for (arg_num, (arg, _)) in args.iter().enumerate() {
        if let Some(reg) = cdecl(arg_num, arg.mem_size()) {
            c.regs.push(reg);
        }
        if let Some(xmm) = cdecl_f(arg_num, arg.mem_size()) {
            c.xmms.push(xmm);
        }
    }

    let mem_size = c.type_size(typ).try_into().unwrap();

    match typ {
        Type::Void => Operand::Imm(Imm::Dword(0)),
        Type::Real => {
            let xmm = Xmm::xmm0(mem_size);
            assert!(c.xmms.take(xmm), "return register should be available");
            Operand::Xmm(xmm)
        }
        Type::Struct(..) => todo!(),
        _ => {
            let reg = Reg::acc(mem_size);
            assert!(c.regs.take(reg), "return register should be available");
            Operand::Reg(reg)
        }
    }
}

pub fn compile_args(c: &mut CodeGen, args: &[Argument], fn_type: &Type) {
    let mut mem_offset = 16; // rip + rbp
    if c.type_size(fn_type) > 8 {
        todo!("implement return value greater than 8 bytes");
    }
    for (arg_num, arg) in args.iter().enumerate() {
        let size = c.type_size(&arg.arg_type);
        let mem_size = size.try_into().unwrap();
        if let Some(reg) = cdecl(arg_num, mem_size) {
            let mem = c.scope.set_sized_var(arg_num, size, mem_size);
            asm::code!(c.code, Mov, mem, reg);
        } else {
            let mem = Mem::offset(Reg::Rbp, mem_offset, mem_size);
            mem_offset += MemSize::QWord as isize;
            c.scope.set_fixed_var(arg_num, mem);
        }
    }
}

pub fn compile_ret_struct(_c: &mut CodeGen, ret: Operand, _size: usize) {
    assert!(ret.is_mem());
    todo!("return value greater than 8 bytes")
}
