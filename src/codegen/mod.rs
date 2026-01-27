mod asm;
mod linux;
mod regs;
mod scope;
mod windows;

use std::collections::HashMap;

use crate::ast::{
    Argument, Ast, AstNode, AstRoot, Expression, Identifier, Operator, UnaryOperator, ValueExpr,
};
use crate::target::CompilerTarget;
use crate::types::{Annotated, Type, TypeAnnot};
use crate::utils::HexSlice;
use asm::{AsmBuilder, Imm, Lbl, Mem, MemSize, MemSized, Operand, Reg, Xmm};
use regs::{OperandManager, RegManager};
use scope::Scope;

type AsmData = HashMap<Lbl, Vec<u8>>;

impl MemSized for TypeAnnot {
    fn mem_size(&self) -> MemSize {
        match self.size() {
            1 => MemSize::Byte,
            2 => MemSize::Word,
            4 => MemSize::DWord,
            8 => MemSize::QWord,
            _ => MemSize::QWord,
        }
    }
}

#[derive(Default)]
struct CodeGen {
    target: CompilerTarget,
    code: AsmBuilder,
    scope: Scope,
    data: AsmData,
    regs: RegManager<Reg>,
    xmms: RegManager<Xmm>,
}

impl CodeGen {
    pub fn new(target: CompilerTarget) -> Self {
        Self {
            target,
            ..Default::default()
        }
    }

    pub fn copy_bytes_inline(&mut self, base_dst: Reg, base_src: Reg, aux_reg: Reg, size: usize) {
        let mut offset = 0;
        loop {
            let remaining = size - offset;
            let copy_size = match remaining {
                8.. => MemSize::QWord,
                4.. => MemSize::DWord,
                2.. => MemSize::DWord,
                1.. => MemSize::Byte,
                _ => break,
            };

            let mem_src = Mem::offset(base_src, offset as isize, copy_size);
            let mem_dst = Mem::offset(base_dst, offset as isize, copy_size);

            let aux_reg = self.regs.switch_size(aux_reg, copy_size);
            asm::code!(self.code, Mov, aux_reg, mem_src);
            asm::code!(self.code, Mov, mem_dst, aux_reg);
            offset += copy_size as usize;
        }
    }
}

fn move_operand_to_reg(c: &mut CodeGen, annot: &TypeAnnot, operand: Operand) -> Operand {
    match operand {
        Operand::Mem(mem) if annot.is_float() => {
            c.regs.try_push(operand);
            let xmm = c.xmms.take_any(mem.mem_size());
            asm::code!(c.code, Movss, xmm, mem);
            Operand::Xmm(xmm)
        }
        Operand::Reg(reg) if annot.is_float() => {
            c.regs.push(reg);
            let xmm = c.xmms.take_any(reg.mem_size());
            asm::code!(c.code, Movd, xmm, reg);
            Operand::Xmm(xmm)
        }
        Operand::Imm(imm) if annot.is_float() => {
            let xmm = c.xmms.take_any(imm.mem_size());
            let tmp = c.scope.new_temp(imm.mem_size());
            asm::code!(c.code, Mov, tmp, imm);
            asm::code!(c.code, Movss, xmm, tmp);
            Operand::Xmm(xmm)
        }
        Operand::Mem(mem) => {
            let reg = c.regs.take_any(mem.mem_size());
            c.regs.try_push(operand);
            asm::code!(c.code, Mov, reg, mem);
            Operand::Reg(reg)
        }
        Operand::Imm(imm) => {
            let reg = c.regs.take_any(imm.mem_size());
            asm::code!(c.code, Mov, reg, imm);
            Operand::Reg(reg)
        }
        _ => operand,
    }
}

fn move_operand_to_mem(c: &mut CodeGen, operand: Operand) -> Operand {
    match operand {
        Operand::Reg(reg) => {
            c.regs.push(reg);
            let mem = c.scope.new_temp(operand.mem_size());
            asm::code!(c.code, Mov, mem, reg);
            Operand::Mem(mem)
        }
        Operand::Mem(mem) => match mem.base() {
            asm::MemBase::Reg(bse) if !bse.is_reserved() => {
                let tmp = c.scope.new_temp(operand.mem_size());
                let reg = c.regs.take_any(operand.mem_size());
                asm::code!(c.code, Mov, reg, mem);
                asm::code!(c.code, Mov, tmp, reg);
                c.regs.push(reg);
                c.regs.push(bse);
                Operand::Mem(tmp)
            }
            _ => operand,
        },
        Operand::Xmm(xmm) => {
            c.xmms.push(xmm);
            let mem = c.scope.new_temp(operand.mem_size());
            asm::code!(c.code, Movss, mem, xmm);
            Operand::Mem(mem)
        }
        _ => operand,
    }
}

fn backup_reg(c: &mut CodeGen, reg: Reg) -> Option<Reg> {
    if !c.regs.ensure(reg) {
        let bkp = c.regs.take_any(MemSize::QWord);
        let reg = c.regs.switch_size(reg, MemSize::QWord);
        asm::code!(c.code, Mov, bkp, reg);
        Some(bkp)
    } else {
        None
    }
}

fn restore_reg(c: &mut CodeGen, reg: Reg, bkp: Option<Reg>) {
    if let Some(bkp) = bkp {
        asm::code!(c.code, Mov, reg, bkp);
        c.regs.push(bkp);
    }
}

fn compile_value(c: &mut CodeGen, value: &ValueExpr) -> Operand {
    match value {
        ValueExpr::String(string) => {
            let label = Lbl::new();
            c.data.insert(label, string.as_bytes().to_vec());
            let reg = c.regs.take_any(MemSize::QWord);
            asm::code!(c.code, Lea, reg, Mem::lbl(label, MemSize::QWord));
            Operand::Reg(reg)
        }
        ValueExpr::Byte(value) => Operand::Imm(Imm::Byte(*value)),
        ValueExpr::Int(value) => Operand::Imm(Imm::from_i32(*value)),
        ValueExpr::Float(value) => Operand::Imm(Imm::from_f32(*value)),
        ValueExpr::Bool(value) => match value {
            true => Operand::Imm(Imm::TRUE),
            false => Operand::Imm(Imm::FALSE),
        },
        ValueExpr::Identifier(annot, id) => match annot.array {
            0 => Operand::Mem(c.scope.get(id)),
            1.. => {
                let mem = c.scope.get(id);
                let reg = c.regs.take_any(MemSize::QWord);
                asm::code!(c.code, Lea, reg, mem);
                Operand::Reg(reg)
            }
        },
    }
}

fn compile_binop(
    c: &mut CodeGen,
    ope: Operator,
    lhs: Operand,
    rhs: Operand,
    annot: TypeAnnot,
) -> Operand {
    let (lhs, rhs) = if ope.is_assign() {
        let rhs = move_operand_to_reg(c, &annot, rhs);
        assert!(lhs.is_mem());

        (lhs, rhs)
    } else {
        let lhs = move_operand_to_reg(c, &annot, lhs);
        (lhs, rhs)
    };

    match annot.is_float() {
        true => compile_fop(c, ope, lhs, rhs),
        false => compile_iop(c, ope, lhs, rhs),
    }
}

fn compile_iop(c: &mut CodeGen, ope: Operator, lhs: Operand, rhs: Operand) -> Operand {
    assert!(ope.is_assign() == lhs.is_mem());
    assert!(lhs.mem_size() == rhs.mem_size());

    let result = match ope {
        Operator::Equal
        | Operator::NotEqual
        | Operator::Greater
        | Operator::GreaterEqual
        | Operator::Less
        | Operator::LessEqual => {
            asm::code!(c.code, Cmp, lhs, rhs);
            let lhs = c.regs.switch_size(lhs.expect_reg(), MemSize::Byte);
            match ope {
                Operator::Equal => asm::code!(c.code, Sete, lhs),
                Operator::NotEqual => asm::code!(c.code, Setne, lhs),
                Operator::Greater => asm::code!(c.code, Setg, lhs),
                Operator::GreaterEqual => asm::code!(c.code, Setge, lhs),
                Operator::Less => asm::code!(c.code, Setl, lhs),
                Operator::LessEqual => asm::code!(c.code, Setle, lhs),
                _ => unreachable!(),
            }
            lhs.into()
        }
        Operator::And | Operator::BitwiseAnd => {
            asm::code!(c.code, And, lhs, rhs);
            lhs
        }
        Operator::Or | Operator::BitwiseOr => {
            asm::code!(c.code, Or, lhs, rhs);
            lhs
        }
        Operator::BitwiseXor => {
            asm::code!(c.code, Xor, lhs, rhs);
            lhs
        }
        Operator::Add => {
            asm::code!(c.code, Add, lhs, rhs);
            lhs
        }
        Operator::Sub => {
            asm::code!(c.code, Sub, lhs, rhs);
            lhs
        }
        Operator::Mul => {
            asm::code!(c.code, Imul, lhs, rhs);
            lhs
        }
        Operator::Div => {
            let lhs = lhs.expect_reg();
            let rhs = match rhs {
                Operand::Imm(imm) => {
                    let tmp = c.scope.new_temp(imm.mem_size()).into();
                    asm::code!(c.code, Mov, tmp, imm);
                    tmp
                }
                _ => move_operand_to_mem(c, rhs),
            };

            if lhs.is_acc() {
                let bkp_dta = backup_reg(c, Reg::Rdx);
                asm::code!(c.code, Xor, Reg::Rdx, Reg::Rdx); // remainder
                asm::code!(c.code, Idiv, rhs); // divisor (rhs)
                restore_reg(c, Reg::Rdx, bkp_dta);
                Operand::Reg(lhs)
            } else if lhs.is_dta() {
                let bkp_acc = backup_reg(c, Reg::Rax);
                asm::code!(c.code, Mov, Reg::acc(lhs.mem_size()), lhs);
                asm::code!(c.code, Xor, Reg::Rdx, Reg::Rdx); // remainder
                asm::code!(c.code, Idiv, rhs); // divisor (rhs)
                asm::code!(c.code, Mov, lhs, Reg::acc(lhs.mem_size()));
                restore_reg(c, Reg::Rax, bkp_acc);
                Operand::Reg(lhs)
            } else {
                let bkp_dta = backup_reg(c, Reg::Rdx);
                let bkp_acc = backup_reg(c, Reg::Rax);
                asm::code!(c.code, Mov, Reg::acc(lhs.mem_size()), lhs);
                asm::code!(c.code, Xor, Reg::Rdx, Reg::Rdx); // remainder
                asm::code!(c.code, Idiv, rhs); // divisor (rhs)
                asm::code!(c.code, Mov, lhs, Reg::acc(lhs.mem_size()));
                restore_reg(c, Reg::Rdx, bkp_dta);
                restore_reg(c, Reg::Rax, bkp_acc);
                Operand::Reg(lhs)
            }
        }
        Operator::Mod => {
            let lhs = lhs.expect_reg();
            let rhs = match rhs {
                Operand::Imm(imm) => {
                    let tmp = c.scope.new_temp(imm.mem_size()).into();
                    asm::code!(c.code, Mov, tmp, imm);
                    tmp
                }
                _ => move_operand_to_mem(c, rhs),
            };

            if lhs.is_acc() {
                let bkp_dta = backup_reg(c, Reg::Rdx);
                asm::code!(c.code, Xor, Reg::Rdx, Reg::Rdx); // remainder
                asm::code!(c.code, Idiv, rhs); // divisor (rhs)
                asm::code!(c.code, Mov, lhs, Reg::dta(lhs.mem_size()));
                restore_reg(c, Reg::Rdx, bkp_dta);
                Operand::Reg(lhs)
            } else if lhs.is_dta() {
                let bkp_acc = backup_reg(c, Reg::Rax);
                asm::code!(c.code, Mov, Reg::acc(lhs.mem_size()), lhs);
                asm::code!(c.code, Xor, Reg::Rdx, Reg::Rdx); // remainder
                asm::code!(c.code, Idiv, rhs); // divisor (rhs)
                restore_reg(c, Reg::Rax, bkp_acc);
                Operand::Reg(lhs)
            } else {
                let bkp_dta = backup_reg(c, Reg::Rdx);
                let bkp_acc = backup_reg(c, Reg::Rax);
                asm::code!(c.code, Mov, Reg::acc(lhs.mem_size()), lhs);
                asm::code!(c.code, Xor, Reg::Rdx, Reg::Rdx); // remainder
                asm::code!(c.code, Idiv, rhs); // divisor (rhs)
                asm::code!(c.code, Mov, lhs, Reg::dta(lhs.mem_size()));
                restore_reg(c, Reg::Rdx, bkp_dta);
                restore_reg(c, Reg::Rax, bkp_acc);
                Operand::Reg(lhs)
            }
        }
        Operator::Shl | Operator::Shr => {
            let lhs = match lhs {
                Operand::Reg(cnt) if cnt.is_cnt() => {
                    let reg = c.regs.take_any(lhs.mem_size());
                    c.regs.push(cnt);
                    asm::code!(c.code, Mov, reg, cnt);
                    Operand::Reg(reg)
                }
                _ => lhs,
            };
            match rhs {
                Operand::Reg(reg) if reg.is_cnt() => {
                    match ope {
                        Operator::Shl => asm::code!(c.code, Shl, lhs, Reg::Cl),
                        Operator::Shr => asm::code!(c.code, Shr, lhs, Reg::Cl),
                        _ => unreachable!(),
                    };
                    lhs
                }
                _ => {
                    let bkp = backup_reg(c, Reg::Rcx);
                    asm::code!(c.code, Mov, Reg::cnt(rhs.mem_size()), rhs);
                    match ope {
                        Operator::Shl => asm::code!(c.code, Shl, lhs, Reg::Cl),
                        Operator::Shr => asm::code!(c.code, Shr, lhs, Reg::Cl),
                        _ => unreachable!(),
                    };
                    restore_reg(c, Reg::Rcx, bkp);
                    lhs
                }
            }
        }
        Operator::Assign => {
            asm::code!(c.code, Mov, lhs, rhs);
            lhs
        }
        Operator::AddAssign => {
            asm::code!(c.code, Add, lhs, rhs);
            lhs
        }
        Operator::SubAssign => {
            asm::code!(c.code, Sub, lhs, rhs);
            lhs
        }
        Operator::MulAssign => {
            let reg = c.regs.take_any(lhs.mem_size());
            asm::code!(c.code, Mov, reg, lhs);
            asm::code!(c.code, Imul, reg, rhs);
            asm::code!(c.code, Mov, lhs, reg);
            c.regs.push(reg);
            lhs
        }
        Operator::DivAssign => {
            let reg = c.regs.take_any(lhs.mem_size());
            asm::code!(c.code, Mov, reg, lhs); // quotient (lhs)
            asm::code!(c.code, Mov, Reg::Edx, Imm::Dword(0)); // remainder
            asm::code!(c.code, Idiv, rhs); // divisor (rhs)
            asm::code!(c.code, Mov, lhs, reg);
            c.regs.push(reg);
            lhs
        }
    };

    c.regs.try_push(rhs);

    result
}

fn compile_fop(c: &mut CodeGen, ope: Operator, lhs: Operand, mut rhs: Operand) -> Operand {
    if let Operand::Imm(imm) = rhs {
        rhs = c.scope.new_temp(imm.mem_size()).into();
        asm::code!(c.code, Mov, rhs, imm);
    }

    assert!(lhs.mem_size() == rhs.mem_size());

    let result = match ope {
        Operator::Equal
        | Operator::NotEqual
        | Operator::Greater
        | Operator::GreaterEqual
        | Operator::Less
        | Operator::LessEqual => {
            let reg = c.regs.take_any(MemSize::Byte);
            asm::code!(c.code, Comiss, lhs, rhs);

            match ope {
                Operator::Equal => asm::code!(c.code, Sete, reg),
                Operator::NotEqual => asm::code!(c.code, Setne, reg),
                Operator::Greater => asm::code!(c.code, Seta, reg),
                Operator::GreaterEqual => asm::code!(c.code, Setae, reg),
                Operator::Less => asm::code!(c.code, Setb, reg),
                Operator::LessEqual => asm::code!(c.code, Setbe, reg),
                _ => unreachable!(),
            }

            c.xmms.try_push(lhs);
            Operand::Reg(reg)
        }
        Operator::Add => {
            asm::code!(c.code, Addss, lhs, rhs);
            lhs
        }
        Operator::Sub => {
            asm::code!(c.code, Subss, lhs, rhs);
            lhs
        }
        Operator::Mul => {
            asm::code!(c.code, Mulss, lhs, rhs);
            lhs
        }
        Operator::Div => {
            asm::code!(c.code, Divss, lhs, rhs);
            lhs
        }
        Operator::Assign => {
            assert!(lhs.is_mem() && rhs.is_xmm());
            asm::code!(c.code, Movss, lhs, rhs);
            lhs
        }
        Operator::AddAssign => {
            assert!(lhs.is_mem() && rhs.is_xmm());
            // commutative property allow us to change the order of operands
            asm::code!(c.code, Addss, rhs, lhs);
            asm::code!(c.code, Movss, lhs, rhs);
            lhs
        }
        Operator::SubAssign => {
            assert!(lhs.is_mem() && rhs.is_xmm());
            let xmm = c.xmms.take_any(lhs.mem_size());
            asm::code!(c.code, Movss, xmm, lhs);
            asm::code!(c.code, Subss, xmm, rhs);
            asm::code!(c.code, Movss, lhs, xmm);
            c.xmms.push(xmm);
            lhs
        }
        Operator::MulAssign => {
            assert!(lhs.is_mem() && rhs.is_xmm());
            // commutative property allow us to change the order of operands
            asm::code!(c.code, Mulss, rhs, lhs);
            asm::code!(c.code, Movss, lhs, rhs);
            lhs
        }
        Operator::DivAssign => {
            assert!(lhs.is_mem() && rhs.is_xmm());
            let xmm = c.xmms.take_any(lhs.mem_size());
            asm::code!(c.code, Movss, xmm, lhs);
            asm::code!(c.code, Divss, xmm, rhs);
            asm::code!(c.code, Movss, lhs, xmm);
            c.xmms.push(xmm);
            lhs
        }
        Operator::Shr
        | Operator::Shl
        | Operator::And
        | Operator::Or
        | Operator::Mod
        | Operator::BitwiseAnd
        | Operator::BitwiseOr
        | Operator::BitwiseXor => {
            unimplemented!()
        }
    };

    c.xmms.try_push(rhs);

    result
}

fn compile_unaop(
    c: &mut CodeGen,
    una_op: UnaryOperator,
    operand: Operand,
    expr_ty: &TypeAnnot,
) -> Operand {
    match una_op {
        UnaryOperator::AddressOf => {
            let Operand::Mem(mem) = operand else {
                panic!("operator AddressOf can only be used on memory");
            };
            let reg = c.regs.take_any(MemSize::QWord);
            asm::code!(c.code, Lea, reg, mem);
            Operand::Reg(reg)
        }
        UnaryOperator::Dereference => {
            let size = expr_ty.mem_size();
            match operand {
                Operand::Reg(reg) => Operand::Mem(Mem::reg(reg, size)),
                Operand::Mem(mem) => {
                    let reg = c.regs.take_any(MemSize::QWord);
                    asm::code!(c.code, Mov, reg, mem);
                    Operand::Mem(Mem::reg(reg, size))
                }
                _ => panic!("invalid operand type for dereference"),
            }
        }
        UnaryOperator::Minus => {
            if expr_ty.is_int() {
                match operand {
                    Operand::Reg(..) => {
                        asm::code!(c.code, Neg, operand);
                        operand
                    }
                    Operand::Imm(..) | Operand::Mem(..) => {
                        let reg = c.regs.take_any(operand.mem_size());
                        asm::code!(c.code, Mov, reg, operand);
                        asm::code!(c.code, Neg, reg);
                        Operand::Reg(reg)
                    }
                    _ => panic!("operator minus could not be applied"),
                }
            } else {
                const MINUS_BIT: u32 = 1 << 31;
                match operand {
                    Operand::Reg(..) | Operand::Mem(..) => {
                        asm::code!(c.code, Xor, operand, Imm::Dword(MINUS_BIT));
                        operand
                    }
                    Operand::Imm(imm) => {
                        let tmp = c.scope.new_temp(imm.mem_size());
                        asm::code!(c.code, Mov, tmp, imm);
                        asm::code!(c.code, Xor, tmp, Imm::Dword(MINUS_BIT));
                        Operand::Mem(tmp)
                    }
                    Operand::Xmm(xmm) => {
                        let tmp = c.scope.new_temp(xmm.mem_size());
                        c.xmms.push(xmm);
                        asm::code!(c.code, Movss, tmp, xmm);
                        asm::code!(c.code, Xor, tmp, Imm::Dword(MINUS_BIT));
                        Operand::Mem(tmp)
                    }
                }
            }
        }
        UnaryOperator::Not => match operand {
            Operand::Reg(..) | Operand::Mem(..) => {
                asm::code!(c.code, Xor, operand, Imm::Byte(0xFF));
                asm::code!(c.code, And, operand, Imm::Byte(0x01));
                operand
            }
            Operand::Imm(imm) => {
                let reg = c.regs.take_any(imm.mem_size());
                asm::code!(c.code, Mov, reg, imm);
                asm::code!(c.code, Xor, reg, Imm::Byte(0xFF));
                asm::code!(c.code, And, reg, Imm::Byte(0x01));
                Operand::Reg(reg)
            }
            Operand::Xmm(..) => unimplemented!(),
        },
        UnaryOperator::BitwiseNot => match operand {
            Operand::Reg(..) | Operand::Mem(..) => {
                asm::code!(c.code, Not, operand);
                operand
            }
            Operand::Imm(imm) => {
                let reg = c.regs.take_any(imm.mem_size());
                asm::code!(c.code, Mov, reg, imm);
                asm::code!(c.code, Not, reg);
                Operand::Reg(reg)
            }
            Operand::Xmm(xmm) => {
                let tmp = c.scope.new_temp(xmm.mem_size());
                asm::code!(c.code, Movss, tmp, xmm);
                asm::code!(c.code, Not, tmp);
                Operand::Mem(tmp)
            }
        },
    }
}

fn compile_index(c: &mut CodeGen, array: Operand, index: Operand, annot: &TypeAnnot) -> Operand {
    let size = annot.mem_size();

    let reg = match index {
        Operand::Reg(reg) => reg,
        offset => {
            let reg = c.regs.take_any(offset.mem_size());
            c.regs.try_push(offset);
            asm::code!(c.code, Mov, reg, offset);
            reg
        }
    };

    let reg = c.regs.switch_size(reg, MemSize::QWord);
    asm::code!(c.code, Imul, reg, Imm::Qword(size as u64));
    asm::code!(c.code, Add, reg, array);
    c.regs.try_push(array);

    Operand::Mem(Mem::reg(reg, size))
}

fn compile_cast(
    c: &mut CodeGen,
    operand: Operand,
    cast_from: &TypeAnnot,
    cast_to: &TypeAnnot,
) -> Operand {
    if cast_from.is_float() && cast_to.is_int() {
        match operand {
            Operand::Mem(..) | Operand::Xmm(..) => {
                let reg = c.regs.take_any(operand.mem_size());
                asm::code!(c.code, Cvtss2si, reg, operand);
                Operand::Reg(reg)
            }
            Operand::Imm(imm) => {
                let tmp = c.scope.new_temp(operand.mem_size());
                let reg = c.regs.take_any(operand.mem_size());
                asm::code!(c.code, Mov, tmp, imm);
                asm::code!(c.code, Cvtss2si, reg, tmp);
                Operand::Reg(reg)
            }
            Operand::Reg(reg) => {
                let tmp = c.scope.new_temp(operand.mem_size());
                asm::code!(c.code, Mov, tmp, reg);
                asm::code!(c.code, Cvtss2si, reg, tmp);
                Operand::Reg(reg)
            }
        }
    } else if cast_from.is_int() && cast_to.is_float() {
        match operand {
            Operand::Reg(..) | Operand::Mem(..) => {
                let xmm = c.xmms.take_any(operand.mem_size());
                asm::code!(c.code, Cvtsi2ss, xmm, operand);
                Operand::Xmm(xmm)
            }
            Operand::Imm(imm) => {
                let tmp = c.scope.new_temp(operand.mem_size());
                let xmm = c.xmms.take_any(operand.mem_size());
                asm::code!(c.code, Mov, tmp, imm);
                asm::code!(c.code, Cvtsi2ss, xmm, tmp);
                Operand::Xmm(xmm)
            }
            Operand::Xmm(..) => unimplemented!(),
        }
    } else if cast_from.is_void_ptr() && cast_to.is_ptr() {
        operand
    } else {
        todo!("conversion between {cast_from} to {cast_to} not implemented yet");
    }
}

fn compile_alloc(c: &mut CodeGen, args: Vec<(Operand, &TypeAnnot)>, annot: &TypeAnnot) -> Operand {
    let base_mem = c.scope.new_temp(annot.mem_size());
    let base_reg = c.regs.take_any(MemSize::QWord);
    asm::code!(c.code, Lea, base_reg, base_mem);
    let mut offset = 0;
    for (operand, annot) in args {
        let operand_reg = move_operand_to_reg(c, annot, operand);
        let offset_mem = Mem::offset(base_reg, offset, annot.mem_size());
        asm::code!(c.code, Mov, offset_mem, operand_reg);
        c.regs.try_push(operand_reg);
        offset += annot.size() as isize;
    }
    c.regs.push(base_reg);
    Operand::Mem(base_mem)
}

fn compile_expr(c: &mut CodeGen, expr: &Expression) -> Operand {
    let flat_expr = crate::ir::IrExpr::from_expr(expr);
    let mut mems = vec![Operand::Imm(Imm::Byte(0)); flat_expr.len()];
    let mut last = 0;

    use crate::ir::IrExprOpe::*;
    for ir in &flat_expr {
        last = ir.id;
        mems[ir.id] = match ir.ope.clone() {
            Value { value } => compile_value(c, &value),
            UnaOp { operator, operand } => compile_unaop(c, operator, mems[operand], &ir.annot),
            BinOp {
                operator,
                lhs,
                rhs,
                annot,
            } => compile_binop(c, operator, mems[lhs], mems[rhs], annot),
            Func { name, args } => {
                let args = args.iter().map(|(id, annot)| (mems[*id], annot)).collect();
                match c.target {
                    CompilerTarget::Linux => linux::compile_call(c, &name, args, &ir.annot),
                    CompilerTarget::Windows => windows::compile_call(c, &name, args, &ir.annot),
                }
            }
            Index { array, index } => {
                let array = compile_value(c, &array);
                compile_index(c, array, mems[index], &ir.annot)
            }
            Cast { value, cast_from } => compile_cast(c, mems[value], &cast_from, &ir.annot),
            Alloc { args } => {
                let args = args.iter().map(|(id, annot)| (mems[*id], annot)).collect();
                compile_alloc(c, args, &ir.annot)
            }
        };

        if !ir.assign {
            // TODO: register management is a mess, so we allways move to stack
            mems[ir.id] = move_operand_to_mem(c, mems[ir.id]);
        }
    }

    c.scope.reset_temps();
    return mems[last];
}

fn compile_node(c: &mut CodeGen, node: &AstNode) {
    match node {
        AstNode::Var(annot, ident, expr) => {
            let local = if annot.array > 0 {
                let mem_size = annot.clone().deref().mem_size();
                c.scope.new_array(ident, annot.array, mem_size)
            } else {
                let mem_size = annot.mem_size();
                c.scope.new_local(ident, mem_size)
            };
            if let Some(expr) = expr {
                let result = compile_expr(c, expr);
                match result {
                    Operand::Reg(reg) => {
                        asm::code!(c.code, Mov, local, reg);
                        c.regs.push(reg);
                    }
                    Operand::Xmm(xmm) => {
                        c.xmms.push(xmm);
                        asm::code!(c.code, Movss, local, xmm);
                    }
                    Operand::Mem(mem) => {
                        let reg = c.regs.take_any(mem.mem_size());
                        asm::code!(c.code, Mov, reg, mem);
                        asm::code!(c.code, Mov, local, reg);
                        c.regs.push(reg);
                        c.regs.try_push(mem.into());
                    }
                    Operand::Imm(..) => {
                        asm::code!(c.code, Mov, local, result);
                    }
                }
            }
        }
        AstNode::If(expr, nodes) => {
            let endif_lbl = c.scope.new_label();

            let mut result = compile_expr(c, expr);
            if let Operand::Imm(imm) = result {
                let reg = c.regs.take_any(imm.mem_size());
                result = Operand::Reg(reg);
                asm::code!(c.code, Mov, result, imm);
            }

            asm::code!(c.code, Cmp, result, Imm::FALSE);
            asm::code!(c.code, Je, endif_lbl);

            c.regs.try_push(result);
            c.scope.continue_on();

            for inner in nodes {
                compile_node(c, inner);
            }

            c.scope.exit();

            asm::code!(c.code, "{endif_lbl}:");
        }
        AstNode::While(expr, nodes) => {
            let startwhile_lbl = c.scope.new_label();
            let endwhile_lbl = c.scope.new_label();

            asm::code!(c.code, "{startwhile_lbl}:");

            let mut result = compile_expr(c, expr);
            if let Operand::Imm(imm) = result {
                let reg = c.regs.take_any(imm.mem_size());
                result = Operand::Reg(reg);
                asm::code!(c.code, Mov, result, imm);
            }

            asm::code!(c.code, Cmp, result, Imm::FALSE);
            asm::code!(c.code, Je, endwhile_lbl);

            c.regs.try_push(result);
            c.scope.continue_on();

            for inner in nodes {
                compile_node(c, inner);
            }

            c.scope.exit();

            asm::code!(c.code, Jmp, startwhile_lbl);
            asm::code!(c.code, "{endwhile_lbl}:");
        }
        AstNode::For(ident, init, limit, nodes) => {
            let startfor_lbl = c.scope.new_label();
            let endfor_lbl = c.scope.new_label();

            c.scope.continue_on();

            let counter = c.scope.new_local(ident, MemSize::DWord);
            let init = match init {
                Some(expr) => {
                    let value = compile_value(c, expr);
                    move_operand_to_reg(c, &expr.get_annot(), value)
                }
                None => Imm::Dword(0).into(),
            };

            asm::code!(c.code, Mov, counter, init);
            asm::code!(c.code, "{startfor_lbl}:");

            c.regs.try_push(init);

            match limit {
                ValueExpr::Int(value) => {
                    asm::code!(c.code, Cmp, counter, Imm::from_i32(*value));
                }
                ValueExpr::Identifier(.., id) => {
                    asm::code!(c.code, Mov, Reg::Eax, c.scope.get(id));
                    asm::code!(c.code, Cmp, counter, Reg::Eax);
                }
                _ => unreachable!(),
            };

            asm::code!(c.code, Jge, endfor_lbl);

            for inner in nodes {
                compile_node(c, inner);
            }

            c.scope.exit();

            asm::code!(c.code, Inc, counter);
            asm::code!(c.code, Jmp, startfor_lbl);
            asm::code!(c.code, "{endfor_lbl}:");
        }
        AstNode::Expr(expr) => {
            let result = compile_expr(c, expr);
            c.regs.try_push(result);
            c.xmms.try_push(result);
        }
        AstNode::Ret(expr) => {
            let result = compile_expr(c, expr);
            let annot = expr.get_annot();
            if !annot.is_ptr()
                && let Type::Custom { name: _, size } = annot.base
                && size > MemSize::QWord as usize
            {
                match c.target {
                    CompilerTarget::Linux => linux::compile_ret_struct(c, result, size),
                    CompilerTarget::Windows => windows::compile_ret_struct(c, result, size),
                }
            } else if annot.is_float() {
                let xmm0 = Xmm::xmm0(result.mem_size());
                if !matches!(result, Operand::Xmm(xmm) if xmm.is_xmm0()) {
                    match result {
                        Operand::Reg(reg) => {
                            c.regs.push(reg);
                            asm::code!(c.code, Movd, xmm0, reg);
                        }
                        Operand::Xmm(xmm) => {
                            c.xmms.push(xmm);
                            asm::code!(c.code, Movss, xmm0, xmm);
                        }
                        Operand::Mem(mem) => {
                            c.regs.try_push(result);
                            asm::code!(c.code, Movss, xmm0, mem);
                        }
                        Operand::Imm(imm) => {
                            let tmp = c.scope.new_temp(imm.mem_size());
                            asm::code!(c.code, Mov, tmp, imm);
                            asm::code!(c.code, Movss, xmm0, tmp);
                        }
                    }
                } else {
                    c.xmms.push(xmm0);
                }
            } else {
                let acc = Reg::acc(result.mem_size());
                if !matches!(result, Operand::Reg(reg) if reg.is_acc()) {
                    match result {
                        Operand::Reg(reg) => {
                            c.regs.push(reg);
                            asm::code!(c.code, Mov, acc, reg);
                        }
                        Operand::Mem(..) | Operand::Imm(..) => {
                            c.regs.try_push(result);
                            asm::code!(c.code, Mov, acc, result);
                        }
                        Operand::Xmm(xmm) => {
                            c.xmms.push(xmm);
                            let tmp = c.scope.new_temp(xmm.mem_size());
                            asm::code!(c.code, Movss, tmp, xmm);
                            asm::code!(c.code, Mov, acc, tmp);
                        }
                    }
                } else {
                    c.regs.push(acc);
                }
            }
            asm::code!(c.code, Jmp, ".R");
        }
    }
}

fn compile_func(
    c: &mut CodeGen,
    ident: &Identifier,
    args: &Vec<Argument>,
    annot: &TypeAnnot,
    nodes: &Vec<AstNode>,
) {
    asm::code!(c.code, "{ident}:");
    asm::code!(c.code, Push, Reg::Rbp);
    asm::code!(c.code, Mov, Reg::Rbp, Reg::Rsp);

    let mut code = std::mem::take(&mut c.code);

    c.scope.new_inner();

    match c.target {
        CompilerTarget::Linux => linux::compile_args(c, args, annot),
        CompilerTarget::Windows => windows::compile_args(c, args, annot),
    };

    for inner in nodes {
        compile_node(c, inner);
    }

    let total_mem = c.scope.ctx.borrow().get_max();
    if total_mem > 0 {
        // align by 16
        let total_mem = total_mem + (16 - total_mem % 16);
        asm::code!(code, Sub, Reg::Rsp, Imm::Qword(total_mem));
    }

    c.scope.exit();

    let inner_code = std::mem::replace(&mut c.code, code);
    c.code.append(inner_code);

    asm::code!(c.code, ".R:");
    asm::code!(c.code, Mov, Reg::Rsp, Reg::Rbp);
    asm::code!(c.code, Pop, Reg::Rbp);
    asm::code!(c.code, Ret);
}

fn compile_global(
    c: &mut CodeGen,
    annot: &TypeAnnot,
    ident: &Identifier,
    value: &Option<ValueExpr>,
) {
    let label = Lbl::from_label(ident);
    c.scope.set(ident, Mem::lbl(label, annot.mem_size()));
    match value {
        Some(value) => match value {
            ValueExpr::String(s) => {
                c.data.insert(label, s.as_bytes().to_vec());
            }
            ValueExpr::Byte(b) => {
                c.data.insert(label, [*b as u8].to_vec());
            }
            ValueExpr::Int(i) => {
                c.data.insert(label, i.to_le_bytes().to_vec());
            }
            ValueExpr::Float(f) => {
                c.data.insert(label, f.to_le_bytes().to_vec());
            }
            ValueExpr::Bool(b) => {
                c.data.insert(label, [*b as u8].to_vec());
            }
            ValueExpr::Identifier(..) => {}
        },
        None => {
            todo!("create a entry on .bss section")
        }
    }
}

fn compile_root(c: &mut CodeGen, node: &AstRoot) {
    match node {
        AstRoot::Func(ident, args, annot, ast_nodes) => {
            compile_func(c, ident, args, annot, ast_nodes);
        }
        AstRoot::Global(annot, ident, value) => {
            compile_global(c, annot, ident, value);
        }
        AstRoot::ExternFunc(name, ..) => asm::code!(c.code, "extern {name}"),
    }
}

pub fn compile(target: CompilerTarget, ast: Ast) -> String {
    let mut c = CodeGen::new(target);
    asm::code!(c.code, "bits 64");
    asm::code!(c.code, "section .text");
    asm::code!(c.code, "global main");

    for node in ast.iter() {
        compile_root(&mut c, node);
    }

    if !c.data.is_empty() {
        asm::code!(c.code, "section .data");

        for (label, bytes) in c.data.iter() {
            asm::code!(c.code, "  {label}: db {:x}", HexSlice::new(bytes));
        }
    }

    c.code.to_string()
}
