mod asm;
mod linux;
mod regs;
mod scope;
mod windows;

use crate::ast::{BinOpe, UnaOpe};
use crate::ir::{IrArg, IrFunc, IrLiteral, IrNode};
use crate::symbols::SymbolTable;
use crate::target::CompilerTarget;
use crate::types::Type;
use crate::utils::HexSlice;
use asm::{AsmBuilder, Imm, Lbl, Mem, MemSize, MemSized, Operand, Reg, Xmm};
use regs::{OperandManager, RegManager};
use scope::Scope;

#[derive(Default)]
pub struct CodeGen {
    target: CompilerTarget,
    code: AsmBuilder,
    scope: Scope,
    regs: RegManager<Reg>,
    xmms: RegManager<Xmm>,
    symbols: SymbolTable,
}

impl CodeGen {
    pub fn new(target: CompilerTarget, symbols: SymbolTable) -> Self {
        Self {
            target,
            symbols,
            ..Default::default()
        }
    }

    pub(self) fn copy_bytes_inline(&mut self, dst: Reg, src: Reg, aux: Reg, size: usize) {
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

            let mem_src = Mem::offset(src, offset as isize, copy_size);
            let mem_dst = Mem::offset(dst, offset as isize, copy_size);

            let aux_reg = self.regs.switch_size(aux, copy_size);
            asm::code!(self.code, Mov, aux_reg, mem_src);
            asm::code!(self.code, Mov, mem_dst, aux_reg);
            offset += copy_size as usize;
        }
    }

    pub(self) fn move_operand_to_reg(&mut self, typ: &Type, operand: Operand) -> Operand {
        match operand {
            Operand::Mem(mem) if matches!(typ, Type::Real) => {
                self.regs.try_push(operand);
                let xmm = self.xmms.take_any(mem.mem_size());
                asm::code!(self.code, Movss, xmm, mem);
                Operand::Xmm(xmm)
            }
            Operand::Reg(reg) if matches!(typ, Type::Real) => {
                self.regs.push(reg);
                let xmm = self.xmms.take_any(reg.mem_size());
                asm::code!(self.code, Movd, xmm, reg);
                Operand::Xmm(xmm)
            }
            Operand::Imm(imm) if matches!(typ, Type::Real) => {
                let xmm = self.xmms.take_any(imm.mem_size());
                let tmp = self.scope.new_temp(imm.mem_size());
                asm::code!(self.code, Mov, tmp, imm);
                asm::code!(self.code, Movss, xmm, tmp);
                Operand::Xmm(xmm)
            }
            Operand::Mem(mem) => {
                let reg = self.regs.take_any(mem.mem_size());
                self.regs.try_push(operand);
                asm::code!(self.code, Mov, reg, mem);
                Operand::Reg(reg)
            }
            Operand::Imm(imm) => {
                let reg = self.regs.take_any(imm.mem_size());
                asm::code!(self.code, Mov, reg, imm);
                Operand::Reg(reg)
            }
            _ => operand,
        }
    }

    pub(self) fn move_operand_to_mem(&mut self, operand: Operand) -> Operand {
        match operand {
            Operand::Reg(reg) => {
                self.regs.push(reg);
                let mem = self.scope.new_temp(operand.mem_size());
                asm::code!(self.code, Mov, mem, reg);
                mem
            }
            Operand::Mem(mem) => match mem.base() {
                asm::MemBase::Reg(bse) if !bse.is_reserved() => {
                    let tmp = self.scope.new_temp(operand.mem_size());
                    let reg = self.regs.take_any(operand.mem_size());
                    asm::code!(self.code, Mov, reg, mem);
                    asm::code!(self.code, Mov, tmp, reg);
                    self.regs.push(reg);
                    self.regs.push(bse);
                    tmp
                }
                _ => operand,
            },
            Operand::Xmm(xmm) => {
                self.xmms.push(xmm);
                let mem = self.scope.new_temp(operand.mem_size());
                asm::code!(self.code, Movss, mem, xmm);
                mem
            }
            _ => operand,
        }
    }

    pub(self) fn clear_all_regs(&mut self) {
        let mut vec = Vec::new();
        while let Some(val) = self.scope.pop() {
            let mem = self.move_operand_to_mem(val);
            vec.push(mem);
        }
        while let Some(val) = vec.pop() {
            self.scope.push(val);
        }
    }

    pub(self) fn type_size(&self, typ: &Type) -> usize {
        match typ {
            Type::Void => 0,
            Type::Byte | Type::Char | Type::Bool => 1,
            Type::Int | Type::Real => 4,
            Type::Func(..) | Type::Pointer(..) | Type::Array(..) => 8,
            Type::Struct(fields) => fields.iter().map(|f| self.type_size(&f.arg_type)).sum(),
        }
    }

    pub(self) fn backup_reg(&mut self, reg: Reg) -> Option<Reg> {
        if !self.regs.ensure(reg) {
            let bkp = self.regs.take_any(MemSize::QWord);
            let reg = self.regs.switch_size(reg, MemSize::QWord);
            asm::code!(self.code, Mov, bkp, reg);
            Some(bkp)
        } else {
            None
        }
    }

    pub(self) fn restore_reg(&mut self, reg: Reg, bkp: Option<Reg>) {
        if let Some(bkp) = bkp {
            asm::code!(self.code, Mov, reg, bkp);
            self.regs.push(bkp);
        }
    }

    pub fn gen_intro(&mut self) {
        asm::code!(self.code, "bits 64");
        asm::code!(self.code, "section .text");

        for (sym, ..) in self.symbols.iter_extrns() {
            asm::code!(self.code, "extern {}", sym.name);
        }
    }

    pub fn gen_func(&mut self, func: IrFunc, is_entry: bool) {
        if is_entry {
            asm::code!(self.code, "global main");
            asm::code!(self.code, "main:");
        }
        asm::code!(self.code, "{}@{}:", func.namespace, func.name);
        asm::code!(self.code, Push, Reg::Rbp);
        asm::code!(self.code, Mov, Reg::Rbp, Reg::Rsp);

        let mut code = std::mem::take(&mut self.code);

        match self.target {
            CompilerTarget::Linux => linux::compile_args(self, &func.args, &func.typ),
            CompilerTarget::Windows => windows::compile_args(self, &func.args, &func.typ),
        };
        for inner in func.body {
            self.gen_node(inner);
        }

        let total_mem = self.scope.get_max();
        if total_mem > 0 {
            let total_mem = total_mem + (16 - total_mem % 16);
            asm::code!(code, Sub, Reg::Rsp, Imm::Qword(total_mem));
        }

        self.scope.reset();

        let inner_code = std::mem::replace(&mut self.code, code);
        self.code.append(inner_code);

        asm::code!(self.code, ".R:");
        asm::code!(self.code, Mov, Reg::Rsp, Reg::Rbp);
        asm::code!(self.code, Pop, Reg::Rbp);
        asm::code!(self.code, Ret);
    }

    pub fn gen_data(&mut self, data: std::collections::HashMap<String, Vec<u8>>) {
        asm::code!(self.code, "section .data");
        for (label, bytes) in data.iter() {
            asm::code!(self.code, "  {label}: db {:x}", HexSlice::new(bytes));
        }
    }

    fn gen_node(&mut self, node: IrNode) {
        match node {
            IrNode::Store { index, typ } => {
                let size = self.type_size(&typ);
                let val = self.scope.pop().unwrap();
                match size {
                    1 | 2 | 4 | 8 => {
                        let mem_size = size.try_into().unwrap();
                        let local = self.scope.set_var(index, mem_size);
                        match val {
                            Operand::Reg(reg) => {
                                asm::code!(self.code, Mov, local, reg);
                                self.regs.push(reg);
                            }
                            Operand::Xmm(xmm) => {
                                asm::code!(self.code, Movss, local, xmm);
                                self.xmms.push(xmm);
                            }
                            Operand::Mem(mem) => {
                                let reg = self.regs.take_any(mem.mem_size());
                                asm::code!(self.code, Mov, reg, mem);
                                asm::code!(self.code, Mov, local, reg);
                                self.regs.push(reg);
                                self.regs.try_push(mem.into());
                            }
                            Operand::Imm(imm) => {
                                asm::code!(self.code, Mov, local, imm);
                            }
                            Operand::Lbl(lbl) => {
                                let reg = self.regs.take_any(lbl.mem_size());
                                asm::code!(self.code, Lea, reg, Mem::lbl(lbl, lbl.mem_size()));
                                asm::code!(self.code, Mov, local, reg);
                                self.regs.push(reg);
                            }
                        }
                    }
                    _ => {
                        let mem_size = size.try_into().unwrap_or(MemSize::QWord);
                        let local = self.scope.set_sized_var(index, size, mem_size);
                        match val {
                            Operand::Reg(src) => {
                                // assume src contains a pointer to the struct
                                let dst = self.regs.take_any(MemSize::QWord);
                                let aux = self.regs.take_any(MemSize::QWord);
                                asm::code!(self.code, Lea, dst, local);

                                self.copy_bytes_inline(dst, src, aux, size);

                                self.regs.push(dst);
                                self.regs.push(src);
                                self.regs.push(aux);
                            }
                            Operand::Mem(ret) => {
                                // assume ret points to the start of the struct
                                let dst = self.regs.take_any(MemSize::QWord);
                                let src = self.regs.take_any(MemSize::QWord);
                                let aux = self.regs.take_any(MemSize::QWord);
                                asm::code!(self.code, Lea, dst, local);
                                asm::code!(self.code, Mov, src, ret);

                                self.copy_bytes_inline(dst, src, aux, size);

                                self.regs.push(dst);
                                self.regs.push(src);
                                self.regs.push(aux);
                                self.regs.try_push(ret.into());
                            }
                            Operand::Xmm(..) | Operand::Imm(..) | Operand::Lbl(..) => {
                                unimplemented!()
                            }
                        };
                    }
                }
                self.scope.reset_temps();
            }
            IrNode::Alloc { index, typ } => match &typ {
                Type::Array(typ, size) => {
                    let size = *size as usize;
                    let type_size = self.type_size(&typ);
                    let mem_size = type_size.try_into().unwrap();
                    self.scope.set_sized_var(index, size * type_size, mem_size);
                }
                Type::Int | Type::Real => {
                    self.scope.set_var(index, MemSize::DWord);
                }
                Type::Byte | Type::Char | Type::Bool => {
                    self.scope.set_var(index, MemSize::Byte);
                }
                Type::Void => {}
                _ => {
                    let size = self.type_size(&typ);
                    self.scope.set_sized_var(index, size, MemSize::QWord);
                }
            },
            IrNode::Load { value } => match value {
                IrArg::Var { index, typ } if typ.is_array() => {
                    let mem = self.scope.get_var(index).unwrap();
                    let reg = self.regs.take_any(MemSize::QWord);
                    asm::code!(self.code, Lea, reg, mem);
                    self.scope.push(reg.into());
                }
                IrArg::Var { index, .. } => self.scope.push_var(index).unwrap(),
                IrArg::Literal { value } => match value {
                    IrLiteral::String { label } => {
                        let reg = self.regs.take_any(MemSize::QWord);
                        asm::code!(self.code, Lea, reg, format!("[rel {label}]"));
                        self.scope.push(reg.into());
                    }
                    IrLiteral::Byte(b) => self.scope.push(Operand::Imm(Imm::Byte(b))),
                    IrLiteral::Int(i) => self.scope.push(Operand::Imm(Imm::from_i32(i))),
                    IrLiteral::Float(f) => self.scope.push(Operand::Imm(Imm::from_f32(f))),
                    IrLiteral::Bool(b) => self.scope.push(Operand::Imm(Imm::Byte(b as u8))),
                },
                IrArg::Extern { name, .. } => {
                    let lbl = Lbl::from_label(&name);
                    self.scope.push(Operand::Lbl(lbl));
                }
                IrArg::Global { ns, name, typ } if typ.is_func() => {
                    let magled = format!("{ns}@{name}");
                    let lbl = Lbl::from_label(&magled);
                    self.scope.push(Operand::Lbl(lbl));
                }
                IrArg::Global { ns: _, name, typ } if typ.is_struct() => {}
                IrArg::Global { ns: _, name, typ } => {
                    let lbl = Lbl::from_label(&name);
                    let mem_size = self.type_size(&typ).try_into().unwrap();
                    self.scope.push(Operand::Mem(Mem::lbl(lbl, mem_size)));
                }
            },
            IrNode::Pop => {
                let val = self.scope.pop().unwrap();
                self.regs.try_push(val);
                self.xmms.try_push(val);
                self.scope.reset_temps();
            }
            IrNode::UnaOp { ope, typ } => {
                let value = self.scope.pop().unwrap();
                let value = match ope {
                    UnaOpe::AddressOf => {
                        let Operand::Mem(mem) = value else {
                            panic!("operator AddressOf can only be used on memory");
                        };
                        let reg = self.regs.take_any(MemSize::QWord);
                        asm::code!(self.code, Lea, reg, mem);
                        Operand::Reg(reg)
                    }
                    UnaOpe::Dereference => {
                        assert!(typ.is_ptr(), "expected value to be a pointer type");
                        let inner = typ.inner().unwrap();
                        let size = self.type_size(&inner).try_into().unwrap();
                        match value {
                            Operand::Reg(reg) => Operand::Mem(Mem::reg(reg, size)),
                            Operand::Mem(mem) => {
                                let reg = self.regs.take_any(MemSize::QWord);
                                asm::code!(self.code, Mov, reg, mem);
                                Operand::Mem(Mem::reg(reg, size))
                            }
                            _ => panic!("invalid operand type for dereference"),
                        }
                    }
                    UnaOpe::Minus => {
                        if let Type::Int = typ {
                            match value {
                                Operand::Reg(..) => {
                                    asm::code!(self.code, Neg, value);
                                    value
                                }
                                Operand::Imm(..) | Operand::Mem(..) => {
                                    let reg = self.regs.take_any(value.mem_size());
                                    asm::code!(self.code, Mov, reg, value);
                                    asm::code!(self.code, Neg, reg);
                                    asm::code!(self.code, Mov, value, reg);
                                    self.regs.push(reg);
                                    value
                                }
                                _ => panic!("operator minus could not be applied"),
                            }
                        } else {
                            const MINUS_BIT: u32 = 1 << 31;
                            match value {
                                Operand::Reg(..) | Operand::Mem(..) => {
                                    asm::code!(self.code, Xor, value, Imm::Dword(MINUS_BIT));
                                    value
                                }
                                Operand::Imm(imm) => {
                                    let tmp = self.scope.new_temp(imm.mem_size());
                                    asm::code!(self.code, Mov, tmp, imm);
                                    asm::code!(self.code, Xor, tmp, Imm::Dword(MINUS_BIT));
                                    tmp
                                }
                                Operand::Xmm(xmm) => {
                                    let tmp = self.scope.new_temp(xmm.mem_size());
                                    self.xmms.push(xmm);
                                    asm::code!(self.code, Movss, tmp, xmm);
                                    asm::code!(self.code, Xor, tmp, Imm::Dword(MINUS_BIT));
                                    tmp
                                }
                                _ => panic!("operator minus could not be applied"),
                            }
                        }
                    }
                    UnaOpe::Not => match value {
                        Operand::Reg(..) | Operand::Mem(..) => {
                            asm::code!(self.code, Xor, value, Imm::Byte(0xFF));
                            asm::code!(self.code, And, value, Imm::Byte(0x01));
                            value
                        }
                        Operand::Imm(imm) => {
                            let tmp = self.scope.new_temp(imm.mem_size());
                            asm::code!(self.code, Mov, tmp, imm);
                            asm::code!(self.code, Xor, tmp, Imm::Byte(0xFF));
                            asm::code!(self.code, And, tmp, Imm::Byte(0x01));
                            tmp
                        }
                        Operand::Xmm(..) | Operand::Lbl(..) => unimplemented!(),
                    },
                    UnaOpe::BitwiseNot => match value {
                        Operand::Reg(..) | Operand::Mem(..) => {
                            asm::code!(self.code, Not, value);
                            value
                        }
                        Operand::Imm(imm) => {
                            let reg = self.regs.take_any(imm.mem_size());
                            asm::code!(self.code, Mov, reg, imm);
                            asm::code!(self.code, Not, reg);
                            Operand::Reg(reg)
                        }
                        Operand::Xmm(xmm) => {
                            let tmp = self.scope.new_temp(xmm.mem_size());
                            asm::code!(self.code, Movss, tmp, xmm);
                            asm::code!(self.code, Not, tmp);
                            tmp
                        }
                        Operand::Lbl(..) => unimplemented!(),
                    },
                };
                self.scope.push(value);
            }
            IrNode::BinOp { ope, typ } if typ.is_float() => {
                let mut lhs = self.scope.pop().unwrap();
                let mut rhs = self.scope.pop().unwrap();

                if ope.is_assign() {
                    rhs = self.move_operand_to_reg(&typ, rhs);
                    assert!(lhs.is_mem());
                } else {
                    lhs = self.move_operand_to_reg(&typ, lhs);
                };

                if let Operand::Imm(imm) = rhs {
                    rhs = self.scope.new_temp(imm.mem_size()).into();
                    asm::code!(self.code, Mov, rhs, imm);
                }

                assert!(lhs.mem_size() == rhs.mem_size());

                lhs = match ope {
                    BinOpe::Equal
                    | BinOpe::NotEqual
                    | BinOpe::Greater
                    | BinOpe::GreaterEqual
                    | BinOpe::Less
                    | BinOpe::LessEqual => {
                        let reg = self.regs.take_any(MemSize::Byte);
                        asm::code!(self.code, Comiss, lhs, rhs);

                        match ope {
                            BinOpe::Equal => asm::code!(self.code, Sete, reg),
                            BinOpe::NotEqual => asm::code!(self.code, Setne, reg),
                            BinOpe::Greater => asm::code!(self.code, Seta, reg),
                            BinOpe::GreaterEqual => asm::code!(self.code, Setae, reg),
                            BinOpe::Less => asm::code!(self.code, Setb, reg),
                            BinOpe::LessEqual => asm::code!(self.code, Setbe, reg),
                            _ => unreachable!(),
                        }

                        self.xmms.try_push(lhs);
                        Operand::Reg(reg)
                    }
                    BinOpe::Add => {
                        asm::code!(self.code, Addss, lhs, rhs);
                        lhs
                    }
                    BinOpe::Sub => {
                        asm::code!(self.code, Subss, lhs, rhs);
                        lhs
                    }
                    BinOpe::Mul => {
                        asm::code!(self.code, Mulss, lhs, rhs);
                        lhs
                    }
                    BinOpe::Div => {
                        asm::code!(self.code, Divss, lhs, rhs);
                        lhs
                    }
                    BinOpe::Assign => {
                        assert!(lhs.is_mem() && rhs.is_xmm());
                        asm::code!(self.code, Movss, lhs, rhs);
                        lhs
                    }
                    BinOpe::AddAssign => {
                        assert!(lhs.is_mem() && rhs.is_xmm());
                        // commutative property allow us to change the order of operands
                        asm::code!(self.code, Addss, rhs, lhs);
                        asm::code!(self.code, Movss, lhs, rhs);
                        lhs
                    }
                    BinOpe::SubAssign => {
                        assert!(lhs.is_mem() && rhs.is_xmm());
                        let xmm = self.xmms.take_any(lhs.mem_size());
                        asm::code!(self.code, Movss, xmm, lhs);
                        asm::code!(self.code, Subss, xmm, rhs);
                        asm::code!(self.code, Movss, lhs, xmm);
                        self.xmms.push(xmm);
                        lhs
                    }
                    BinOpe::MulAssign => {
                        assert!(lhs.is_mem() && rhs.is_xmm());
                        // commutative property allow us to change the order of operands
                        asm::code!(self.code, Mulss, rhs, lhs);
                        asm::code!(self.code, Movss, lhs, rhs);
                        lhs
                    }
                    BinOpe::DivAssign => {
                        assert!(lhs.is_mem() && rhs.is_xmm());
                        let xmm = self.xmms.take_any(lhs.mem_size());
                        asm::code!(self.code, Movss, xmm, lhs);
                        asm::code!(self.code, Divss, xmm, rhs);
                        asm::code!(self.code, Movss, lhs, xmm);
                        self.xmms.push(xmm);
                        lhs
                    }
                    _ => unimplemented!("{ope:?}"),
                };

                self.scope.push(lhs);
                self.xmms.try_push(rhs);
            }
            IrNode::BinOp { ope, typ } => {
                let mut lhs = self.scope.pop().unwrap();
                let mut rhs = self.scope.pop().unwrap();

                if ope.is_assign() {
                    rhs = self.move_operand_to_reg(&typ, rhs);
                    assert!(lhs.is_mem());
                } else {
                    lhs = self.move_operand_to_reg(&typ, lhs);
                };

                assert!(ope.is_assign() == lhs.is_mem());
                assert!(lhs.mem_size() == rhs.mem_size());

                lhs = match ope {
                    BinOpe::Equal
                    | BinOpe::NotEqual
                    | BinOpe::Greater
                    | BinOpe::GreaterEqual
                    | BinOpe::Less
                    | BinOpe::LessEqual => {
                        asm::code!(self.code, Cmp, lhs, rhs);
                        let lhs = self.regs.switch_size(lhs.expect_reg(), MemSize::Byte);
                        match ope {
                            BinOpe::Equal => asm::code!(self.code, Sete, lhs),
                            BinOpe::NotEqual => asm::code!(self.code, Setne, lhs),
                            BinOpe::Greater => asm::code!(self.code, Setg, lhs),
                            BinOpe::GreaterEqual => asm::code!(self.code, Setge, lhs),
                            BinOpe::Less => asm::code!(self.code, Setl, lhs),
                            BinOpe::LessEqual => asm::code!(self.code, Setle, lhs),
                            _ => unreachable!(),
                        }
                        lhs.into()
                    }
                    BinOpe::And | BinOpe::BitwiseAnd => {
                        asm::code!(self.code, And, lhs, rhs);
                        lhs
                    }
                    BinOpe::Or | BinOpe::BitwiseOr => {
                        asm::code!(self.code, Or, lhs, rhs);
                        lhs
                    }
                    BinOpe::BitwiseXor => {
                        asm::code!(self.code, Xor, lhs, rhs);
                        lhs
                    }
                    BinOpe::Add => {
                        asm::code!(self.code, Add, lhs, rhs);
                        lhs
                    }
                    BinOpe::Sub => {
                        asm::code!(self.code, Sub, lhs, rhs);
                        lhs
                    }
                    BinOpe::Mul => {
                        asm::code!(self.code, Imul, lhs, rhs);
                        lhs
                    }
                    BinOpe::Div => {
                        let lhs = lhs.expect_reg();
                        let rhs = match rhs {
                            Operand::Imm(imm) => {
                                let tmp = self.scope.new_temp(imm.mem_size());
                                asm::code!(self.code, Mov, tmp, imm);
                                tmp
                            }
                            _ => self.move_operand_to_mem(rhs),
                        };

                        if lhs.is_acc() {
                            let bkp_dta = self.backup_reg(Reg::Rdx);
                            asm::code!(self.code, Xor, Reg::Rdx, Reg::Rdx); // remainder
                            asm::code!(self.code, Idiv, rhs); // divisor (rhs)
                            self.restore_reg(Reg::Rdx, bkp_dta);
                        } else if lhs.is_dta() {
                            let bkp_acc = self.backup_reg(Reg::Rax);
                            asm::code!(self.code, Mov, Reg::acc(lhs.mem_size()), lhs);
                            asm::code!(self.code, Xor, Reg::Rdx, Reg::Rdx); // remainder
                            asm::code!(self.code, Idiv, rhs); // divisor (rhs)
                            asm::code!(self.code, Mov, lhs, Reg::acc(lhs.mem_size()));
                            self.restore_reg(Reg::Rax, bkp_acc);
                        } else {
                            let bkp_dta = self.backup_reg(Reg::Rdx);
                            let bkp_acc = self.backup_reg(Reg::Rax);
                            asm::code!(self.code, Mov, Reg::acc(lhs.mem_size()), lhs);
                            asm::code!(self.code, Xor, Reg::Rdx, Reg::Rdx); // remainder
                            asm::code!(self.code, Idiv, rhs); // divisor (rhs)
                            asm::code!(self.code, Mov, lhs, Reg::acc(lhs.mem_size()));
                            self.restore_reg(Reg::Rdx, bkp_dta);
                            self.restore_reg(Reg::Rax, bkp_acc);
                        }
                        lhs.into()
                    }
                    BinOpe::Mod => {
                        let lhs = lhs.expect_reg();
                        let rhs = match rhs {
                            Operand::Imm(imm) => {
                                let tmp = self.scope.new_temp(imm.mem_size());
                                asm::code!(self.code, Mov, tmp, imm);
                                tmp
                            }
                            _ => self.move_operand_to_mem(rhs),
                        };

                        if lhs.is_acc() {
                            let bkp_dta = self.backup_reg(Reg::Rdx);
                            asm::code!(self.code, Xor, Reg::Rdx, Reg::Rdx); // remainder
                            asm::code!(self.code, Idiv, rhs); // divisor (rhs)
                            asm::code!(self.code, Mov, lhs, Reg::dta(lhs.mem_size()));
                            self.restore_reg(Reg::Rdx, bkp_dta);
                        } else if lhs.is_dta() {
                            let bkp_acc = self.backup_reg(Reg::Rax);
                            asm::code!(self.code, Mov, Reg::acc(lhs.mem_size()), lhs);
                            asm::code!(self.code, Xor, Reg::Rdx, Reg::Rdx); // remainder
                            asm::code!(self.code, Idiv, rhs); // divisor (rhs)
                            self.restore_reg(Reg::Rax, bkp_acc);
                        } else {
                            let bkp_dta = self.backup_reg(Reg::Rdx);
                            let bkp_acc = self.backup_reg(Reg::Rax);
                            asm::code!(self.code, Mov, Reg::acc(lhs.mem_size()), lhs);
                            asm::code!(self.code, Xor, Reg::Rdx, Reg::Rdx); // remainder
                            asm::code!(self.code, Idiv, rhs); // divisor (rhs)
                            asm::code!(self.code, Mov, lhs, Reg::dta(lhs.mem_size()));
                            self.restore_reg(Reg::Rdx, bkp_dta);
                            self.restore_reg(Reg::Rax, bkp_acc);
                        }
                        lhs.into()
                    }
                    BinOpe::Shl | BinOpe::Shr => {
                        let lhs = match lhs {
                            Operand::Reg(cnt) if cnt.is_cnt() => {
                                let reg = self.regs.take_any(lhs.mem_size());
                                self.regs.push(cnt);
                                asm::code!(self.code, Mov, reg, cnt);
                                Operand::Reg(reg)
                            }
                            _ => lhs,
                        };
                        match rhs {
                            Operand::Reg(reg) if reg.is_cnt() => match ope {
                                BinOpe::Shl => asm::code!(self.code, Shl, lhs, Reg::Cl),
                                BinOpe::Shr => asm::code!(self.code, Shr, lhs, Reg::Cl),
                                _ => unreachable!(),
                            },
                            _ => {
                                let bkp = self.backup_reg(Reg::Rcx);
                                asm::code!(self.code, Mov, Reg::cnt(rhs.mem_size()), rhs);
                                match ope {
                                    BinOpe::Shl => asm::code!(self.code, Shl, lhs, Reg::Cl),
                                    BinOpe::Shr => asm::code!(self.code, Shr, lhs, Reg::Cl),
                                    _ => unreachable!(),
                                };
                                self.restore_reg(Reg::Rcx, bkp);
                            }
                        };
                        lhs
                    }
                    BinOpe::Assign => {
                        asm::code!(self.code, Mov, lhs, rhs);
                        lhs
                    }
                    BinOpe::AddAssign => {
                        asm::code!(self.code, Add, lhs, rhs);
                        lhs
                    }
                    BinOpe::SubAssign => {
                        asm::code!(self.code, Sub, lhs, rhs);
                        lhs
                    }
                    BinOpe::MulAssign => {
                        let reg = self.regs.take_any(lhs.mem_size());
                        asm::code!(self.code, Mov, reg, lhs);
                        asm::code!(self.code, Imul, reg, rhs);
                        asm::code!(self.code, Mov, lhs, reg);
                        self.regs.push(reg);
                        lhs
                    }
                    BinOpe::DivAssign => {
                        let reg = self.regs.take_any(lhs.mem_size());
                        asm::code!(self.code, Mov, reg, lhs); // quotient (lhs)
                        asm::code!(self.code, Mov, Reg::Edx, Imm::Dword(0)); // remainder
                        asm::code!(self.code, Idiv, rhs); // divisor (rhs)
                        asm::code!(self.code, Mov, lhs, reg);
                        self.regs.push(reg);
                        lhs
                    }
                };

                self.scope.push(lhs);
                self.regs.try_push(rhs);
            }
            IrNode::Arg => {
                let arg = self.scope.pop().unwrap();
                let arg = self.move_operand_to_mem(arg);
                self.scope.push(arg);
            }
            IrNode::Call { args, ret } => {
                self.clear_all_regs();
                let func = self.scope.pop().unwrap();
                let mut values = Vec::new();
                for arg in args.into_iter().rev() {
                    let value = self.scope.pop().unwrap();
                    values.push((value, arg.arg_type));
                }
                values.reverse();
                let ret = match self.target {
                    CompilerTarget::Linux => linux::compile_call(self, func, values, &ret),
                    CompilerTarget::Windows => windows::compile_call(self, func, values, &ret),
                };
                self.scope.push(ret);
            }
            IrNode::Index { typ } => {
                let index = self.scope.pop().unwrap();
                let array = self.scope.pop().unwrap();

                let size = self.type_size(&typ);
                let reg = match index {
                    Operand::Reg(reg) => reg,
                    offset => {
                        let reg = self.regs.take_any(offset.mem_size());
                        self.regs.try_push(offset);
                        asm::code!(self.code, Mov, reg, offset);
                        reg
                    }
                };
                let reg = self.regs.switch_size(reg, MemSize::QWord);
                asm::code!(self.code, Imul, reg, Imm::Qword(size as u64));
                asm::code!(self.code, Add, reg, array);
                self.regs.try_push(array);

                let mem = Operand::Mem(Mem::reg(reg, size.try_into().unwrap()));
                self.scope.push(mem.into());
            }
            IrNode::Cast { from, to } => {
                let value = self.scope.pop().unwrap();
                if from.is_float() && to.is_int() {
                    match value {
                        Operand::Mem(..) | Operand::Xmm(..) => {
                            let reg = self.regs.take_any(value.mem_size());
                            asm::code!(self.code, Cvtss2si, reg, value);
                            self.scope.push(reg.into());
                            self.regs.try_push(value);
                            self.xmms.try_push(value);
                        }
                        Operand::Imm(imm) => {
                            let reg = self.regs.take_any(value.mem_size());
                            let tmp = self.scope.new_temp(value.mem_size());
                            asm::code!(self.code, Mov, tmp, imm);
                            asm::code!(self.code, Cvtss2si, reg, tmp);
                            self.scope.push(reg.into());
                        }
                        Operand::Reg(reg) => {
                            let tmp = self.scope.new_temp(reg.mem_size());
                            asm::code!(self.code, Mov, tmp, reg);
                            asm::code!(self.code, Cvtss2si, reg, tmp);
                            self.scope.push(reg.into());
                        }
                        Operand::Lbl(..) => unimplemented!(),
                    }
                } else if from.is_int() && to.is_float() {
                    let xmm = self.xmms.take_any(value.mem_size());
                    self.scope.push(xmm.into());
                    match value {
                        Operand::Reg(..) | Operand::Mem(..) => {
                            asm::code!(self.code, Cvtsi2ss, xmm, value);
                            self.regs.try_push(value);
                        }
                        Operand::Imm(imm) => {
                            let tmp = self.scope.new_temp(value.mem_size());
                            asm::code!(self.code, Mov, tmp, imm);
                            asm::code!(self.code, Cvtsi2ss, xmm, tmp);
                        }
                        Operand::Xmm(..) | Operand::Lbl(..) => unimplemented!(),
                    }
                } else if from.is_func() && to.is_void_ptr() {
                    match value {
                        Operand::Lbl(lbl) => {
                            let reg = self.regs.take_any(lbl.mem_size());
                            asm::code!(self.code, Lea, reg, Mem::lbl(lbl, lbl.mem_size()));
                            self.scope.push(reg.into());
                        }
                        Operand::Reg(..) | Operand::Mem(..) | Operand::Xmm(..) => {
                            self.scope.push(value)
                        }
                        Operand::Imm(..) => unimplemented!(),
                    }
                } else if from.is_void_ptr() || to.is_void_ptr() {
                    // assuming the size is correct, anything can be a pointer
                    assert!(value.mem_size() == MemSize::QWord);
                    self.scope.push(value);
                } else {
                    todo!("conversion between {from} to {to} not implemented yet");
                }
            }
            IrNode::Field { mut offset, by_ref } => {
                let item = self.scope.pop().unwrap();
                let field_type = offset.pop().unwrap();
                let type_size = self.type_size(&field_type);
                let mem_size = type_size.try_into().unwrap_or(MemSize::QWord);
                let offset: usize = offset.iter().map(|f| self.type_size(f)).sum();
                let base = if by_ref {
                    self.move_operand_to_reg(&field_type, item).expect_reg()
                } else {
                    let reg = self.regs.take_any(MemSize::QWord);
                    asm::code!(self.code, Lea, reg, item);
                    reg
                };
                let field_mem = Mem::offset(base, offset as isize, mem_size).into();
                self.scope.push(field_mem);
            }
            IrNode::Struct { fields } => {
                let type_size: usize = fields.iter().map(|f| self.type_size(&f)).sum();
                let mut values = Vec::new();
                for field_type in fields.into_iter().rev() {
                    let value = self.scope.pop().unwrap();
                    values.push((value, field_type));
                }
                let base_mem_size = type_size.try_into().unwrap_or(MemSize::QWord);
                let base_reg = self.regs.take_any(MemSize::QWord);
                let base_mem = self.scope.new_sized_temp(type_size, base_mem_size);
                asm::code!(self.code, Lea, base_reg, base_mem);
                let mut offset = 0;
                for (operand, field_type) in values.into_iter().rev() {
                    let operand_reg = self.move_operand_to_reg(&field_type, operand);
                    let mem_size = self.type_size(&field_type).try_into().unwrap();
                    let offset_mem = Mem::offset(base_reg, offset, mem_size);
                    asm::code!(self.code, Mov, offset_mem, operand_reg);
                    self.regs.try_push(operand_reg);
                    offset += self.type_size(&field_type) as isize;
                }
                self.regs.push(base_reg);
                self.scope.push(base_mem);
            }
            IrNode::SizeOf { typ } => {
                let size = match typ {
                    Type::Array(inner, arr_size) => {
                        let size = self.type_size(&inner) as u32;
                        Imm::Dword(size * arr_size).into()
                    }
                    _ => {
                        let size = self.type_size(&typ) as u32;
                        Imm::Dword(size).into()
                    }
                };
                self.scope.push(size);
            }
            IrNode::Inc => {
                let value = self.scope.pop().unwrap();
                asm::code!(self.code, Inc, value);
            }
            IrNode::Label { label } => asm::code!(self.code, ".L{label}:"),
            IrNode::Jmp { label } => asm::code!(self.code, Jmp, format!(".L{label}")),
            IrNode::JmpEq { label } => {
                let lhs = self.scope.pop().unwrap();
                let rhs = self.scope.pop().unwrap();
                let lhs = self.move_operand_to_reg(&Type::Int, lhs);
                asm::code!(self.code, Cmp, lhs, rhs);
                asm::code!(self.code, Je, format!(".L{label}"));
                self.regs.try_push(rhs);
                self.regs.try_push(lhs);
            }
            IrNode::JmpFalse { label } => {
                let mut val = self.scope.pop().unwrap();
                if let Operand::Imm(..) = val {
                    val = self.move_operand_to_reg(&Type::Bool, val);
                }
                asm::code!(self.code, Cmp, val, Imm::Byte(0));
                asm::code!(self.code, Je, format!(".L{label}"));
                self.regs.try_push(val);
            }
            IrNode::Return { typ } if typ.is_void() => {
                asm::code!(self.code, Jmp, ".R");
                self.scope.reset_temps();
            }
            IrNode::Return { typ } => {
                let result = self.scope.pop().unwrap();
                let size = self.type_size(&typ);
                if size > MemSize::QWord as usize {
                    match self.target {
                        CompilerTarget::Linux => linux::compile_ret_struct(self, result, size),
                        CompilerTarget::Windows => windows::compile_ret_struct(self, result, size),
                    }
                } else if typ.is_float() {
                    let xmm0 = Xmm::xmm0(result.mem_size());
                    if !matches!(result, Operand::Xmm(xmm) if xmm.is_xmm0()) {
                        match result {
                            Operand::Reg(reg) => {
                                self.regs.push(reg);
                                asm::code!(self.code, Movd, xmm0, reg);
                            }
                            Operand::Xmm(xmm) => {
                                self.xmms.push(xmm);
                                asm::code!(self.code, Movss, xmm0, xmm);
                            }
                            Operand::Mem(mem) => {
                                self.regs.try_push(result);
                                asm::code!(self.code, Movss, xmm0, mem);
                            }
                            Operand::Imm(imm) => {
                                let tmp = self.scope.new_sized_temp(size, imm.mem_size());
                                asm::code!(self.code, Mov, tmp, imm);
                                asm::code!(self.code, Movss, xmm0, tmp);
                            }
                            Operand::Lbl(..) => unimplemented!(),
                        }
                    } else {
                        self.xmms.push(xmm0);
                    }
                } else {
                    let acc = Reg::acc(result.mem_size());
                    if !matches!(result, Operand::Reg(reg) if reg.is_acc()) {
                        match result {
                            Operand::Reg(reg) => {
                                self.regs.push(reg);
                                asm::code!(self.code, Mov, acc, reg);
                            }
                            Operand::Mem(..) | Operand::Imm(..) => {
                                self.regs.try_push(result);
                                asm::code!(self.code, Mov, acc, result);
                            }
                            Operand::Xmm(xmm) => {
                                self.xmms.push(xmm);
                                let tmp = self.scope.new_sized_temp(size, xmm.mem_size());
                                asm::code!(self.code, Movss, tmp, xmm);
                                asm::code!(self.code, Mov, acc, tmp);
                            }
                            Operand::Lbl(lbl) => {
                                asm::code!(self.code, Lea, acc, Mem::lbl(lbl, lbl.mem_size()));
                            }
                        }
                    } else {
                        self.regs.push(acc);
                    }
                }
                asm::code!(self.code, Jmp, ".R");
                self.scope.reset_temps();
            }
        }
    }

    pub fn to_string(&self) -> String {
        self.code.to_string()
    }
}
