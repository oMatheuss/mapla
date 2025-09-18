use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::asm::{
    self, AsmBuilder, BaseOperandManager, Imm, Lbl, Mem, MemSize, MemSized, Operand,
    OperandManager, Reg, RegManager, Xmm,
};
use crate::ast::{
    Annotated, Annotation, Argument, Ast, AstNode, AstRoot, BinaryOp, Expression, FunctionCall,
    Identifier, Indexing, Operator, TypeAnnot, TypeCast, UnaryOp, UnaryOperator, ValueExpr,
    VarType,
};
use crate::target::CompilerTarget;
use crate::utils::HexSlice;

#[derive(Debug, Clone, Copy, Default)]
struct ScopeContext {
    // actual stack offset of local variables
    local_off: isize,

    // actual stack offset of temp variables
    temp_off: isize,

    // max stack size needed for a function call
    max_func: usize,

    // max stack offset needed temp variables
    max_temp_off: isize,

    // label count used for jumps
    lbl_count: usize,
}

impl ScopeContext {
    pub fn get_max(&self) -> u64 {
        self.local_off.abs() as u64 + self.max_temp_off.abs() as u64 + self.max_func as u64
    }
}

#[derive(Debug, Default)]
struct Scope {
    var: HashMap<String, Mem>,
    ctx: Rc<RefCell<ScopeContext>>,
    sup: Option<Box<Scope>>,
}

impl Scope {
    fn new() -> Self {
        Self::default()
    }

    fn new_inner(&mut self) {
        let cur = std::mem::take(self);
        let mut src = Self::new();
        src.sup = Some(Box::new(cur));
        let _ = std::mem::replace(self, src);
    }

    fn continue_on(&mut self) {
        let cur = std::mem::take(self);
        let src = Self {
            var: HashMap::new(),
            ctx: cur.ctx.clone(),
            sup: Some(Box::new(cur)),
        };
        let _ = std::mem::replace(self, src);
    }

    fn exit(&mut self) {
        let cur = std::mem::take(self);
        let outer = cur.sup.expect("at least global scope should exist");
        let _ = std::mem::replace(self, *outer);
    }

    fn get(&self, ident: &str) -> Mem {
        if let Some(local) = self.var.get(ident) {
            *local
        } else if let Some(sup) = &self.sup {
            sup.get(ident)
        } else {
            panic!("'{ident}' was not found: variable does not exist")
        }
    }

    #[inline]
    fn set(&mut self, ident: &str, operand: Mem) -> Option<Mem> {
        self.var.insert(String::from(ident), operand)
    }

    fn new_local(&mut self, ident: &str, mem_size: MemSize) -> Operand {
        let mut mem = self.ctx.borrow_mut();
        mem.local_off -= mem_size as isize;
        let mem = Mem::offset(Reg::Rbp, mem.local_off, mem_size);
        self.var.insert(String::from(ident), mem);
        Operand::Mem(mem)
    }

    fn new_array(&mut self, ident: &str, size: u32, mem_size: MemSize) -> Operand {
        let mut mem = self.ctx.borrow_mut();
        mem.local_off -= mem_size as isize * size as isize;
        let mem = Mem::offset(Reg::Rbp, mem.local_off, MemSize::QWord);
        self.var.insert(String::from(ident), mem);
        Operand::Mem(mem)
    }

    fn new_temp(&mut self, mem_size: MemSize) -> Mem {
        let mut mem = self.ctx.borrow_mut();
        mem.temp_off -= mem_size as isize;
        if mem.temp_off < mem.max_temp_off {
            mem.max_temp_off = mem.temp_off;
        }
        let offset = mem.local_off + mem.temp_off;
        Mem::offset(Reg::Rbp, offset, mem_size)
    }

    fn new_call(&mut self, call_size: usize) {
        let mut mem = self.ctx.borrow_mut();
        if call_size > mem.max_func {
            mem.max_func = call_size;
        }
    }

    #[inline]
    fn reset_temps(&mut self) {
        self.ctx.borrow_mut().temp_off = 0;
    }

    #[inline]
    fn new_label(&mut self) -> String {
        let mut ctx = self.ctx.borrow_mut();
        ctx.lbl_count += 1;
        format!(".L{cnt}", cnt = ctx.lbl_count)
    }
}

type AsmData = HashMap<Lbl, Vec<u8>>;

impl MemSized for VarType {
    fn mem_size(&self) -> MemSize {
        match self {
            VarType::Int | VarType::Real => MemSize::DWord,
            VarType::Bool | VarType::Char => MemSize::Byte,
            VarType::Void => panic!("void does not have a known size"),
        }
    }
}

impl MemSized for TypeAnnot {
    fn mem_size(&self) -> MemSize {
        match self.annotation() {
            Annotation::Value => self.inner_type().mem_size(),
            Annotation::Pointer(..) | Annotation::Array(..) => MemSize::QWord,
        }
    }
}

pub struct Compiler {
    code: AsmBuilder,
    scope: Scope,
    data: AsmData,
    target: CompilerTarget,
    regs: RegManager<Reg>,
    xmms: RegManager<Xmm>,
}

impl Compiler {
    pub fn new(target: CompilerTarget) -> Self {
        Compiler {
            code: AsmBuilder::new(),
            scope: Scope::new(),
            data: AsmData::new(),
            target,
            regs: RegManager::new(),
            xmms: RegManager::new(),
        }
    }

    pub fn compile(mut self, ast: Ast) -> String {
        asm::code!(self.code, "bits 64");
        asm::code!(self.code, "section .text");
        asm::code!(self.code, "global main");

        for node in ast.iter() {
            compile_root(&mut self, node);
        }

        if !self.data.is_empty() {
            asm::code!(self.code, "section .data");

            for (label, bytes) in self.data.iter() {
                asm::code!(self.code, "  {label}: db {:x}", HexSlice::new(bytes));
            }
        }

        self.code.to_string()
    }
}

fn cdecl(c: &mut Compiler, arg_num: usize, mem_size: MemSize) -> Option<Reg> {
    let reg = match (c.target, arg_num) {
        (CompilerTarget::Linux, 0) => Reg::dst(mem_size),
        (CompilerTarget::Linux, 1) => Reg::src(mem_size),
        (CompilerTarget::Linux, 2) => Reg::dta(mem_size),
        (CompilerTarget::Linux, 3) => Reg::cnt(mem_size),
        (CompilerTarget::Linux, 4) => Reg::r8(mem_size),
        (CompilerTarget::Linux, 5) => Reg::r9(mem_size),

        (CompilerTarget::Windows, 0) => Reg::cnt(mem_size),
        (CompilerTarget::Windows, 1) => Reg::dta(mem_size),
        (CompilerTarget::Windows, 2) => Reg::r8(mem_size),
        (CompilerTarget::Windows, 3) => Reg::r9(mem_size),

        (_, _) => return None,
    };
    Some(reg)
}

fn cdecl_f(c: &mut Compiler, arg_num: usize, mem_size: MemSize) -> Option<Xmm> {
    let xmm = match (c.target, arg_num) {
        (CompilerTarget::Linux | CompilerTarget::Windows, 0) => Xmm::xmm(0, mem_size),
        (CompilerTarget::Linux | CompilerTarget::Windows, 1) => Xmm::xmm(1, mem_size),
        (CompilerTarget::Linux | CompilerTarget::Windows, 2) => Xmm::xmm(2, mem_size),
        (CompilerTarget::Linux | CompilerTarget::Windows, 3) => Xmm::xmm(3, mem_size),
        (CompilerTarget::Linux, 4) => Xmm::xmm(4, mem_size),
        (CompilerTarget::Linux, 5) => Xmm::xmm(5, mem_size),
        (CompilerTarget::Linux, 6) => Xmm::xmm(6, mem_size),
        (CompilerTarget::Linux, 7) => Xmm::xmm(7, mem_size),

        (_, _) => return None,
    };
    Some(xmm)
}

fn cdecl_regs(target: CompilerTarget) -> &'static [Reg] {
    match target {
        CompilerTarget::Linux => &[Reg::Rdi, Reg::Rsi, Reg::Rdx, Reg::Rcx, Reg::R8, Reg::R9],
        CompilerTarget::Windows => &[Reg::Rcx, Reg::Rdx, Reg::R8, Reg::R9],
    }
}

fn reserve_cdecl(c: &mut Compiler) {
    for reg in cdecl_regs(c.target) {
        c.regs.take(*reg);
    }
}

fn release_cdecl(c: &mut Compiler) {
    for reg in cdecl_regs(c.target) {
        c.regs.push(*reg);
    }
}

fn move_operand_to_reg(c: &mut Compiler, annot: TypeAnnot, operand: Operand) -> Operand {
    match operand {
        Operand::Mem(mem) if annot.is_float() => {
            c.regs.try_push(operand);
            let xmm = c.xmms.take_any(mem.mem_size()).expect("register available");
            asm::code!(c.code, Movss, xmm, mem);
            Operand::Xmm(xmm)
        }
        Operand::Reg(reg) if annot.is_float() => {
            c.regs.push(reg);
            let xmm = c.xmms.take_any(reg.mem_size()).expect("register available");
            asm::code!(c.code, Movd, xmm, reg);
            Operand::Xmm(xmm)
        }
        Operand::Imm(imm) if annot.is_float() => {
            let xmm = c.xmms.take_any(imm.mem_size()).expect("register available");
            let tmp = c.scope.new_temp(imm.mem_size());
            asm::code!(c.code, Mov, tmp, imm);
            asm::code!(c.code, Movss, xmm, tmp);
            Operand::Xmm(xmm)
        }
        Operand::Mem(mem) => {
            let reg = c.regs.take_any(mem.mem_size()).expect("register available");
            c.regs.try_push(operand);
            asm::code!(c.code, Mov, reg, mem);
            Operand::Reg(reg)
        }
        Operand::Imm(imm) => {
            let reg = c.regs.take_any(imm.mem_size()).expect("register available");
            asm::code!(c.code, Mov, reg, imm);
            Operand::Reg(reg)
        }
        _ => operand,
    }
}

fn move_reg_to_mem(c: &mut Compiler, annot: TypeAnnot, operand: Operand) -> Operand {
    match operand {
        Operand::Reg(reg) => {
            c.regs.push(reg);
            let mem = c.scope.new_temp(annot.mem_size());
            asm::code!(c.code, Mov, mem, reg);
            Operand::Mem(mem)
        }
        Operand::Mem(mem) => match mem.base() {
            asm::MemBase::Reg(bse) if !bse.is_reserved() => {
                let tmp = c.scope.new_temp(annot.mem_size());
                let reg = c
                    .regs
                    .take_any(annot.mem_size())
                    .expect("register available");
                asm::code!(c.code, Mov, reg, mem);
                asm::code!(c.code, Mov, tmp, reg);
                c.regs.push(reg);
                c.regs.push(bse);
                Operand::Mem(tmp)
            }
            _ => operand,
        },
        _ => operand,
    }
}

fn backup_reg(c: &mut Compiler, reg: Reg) -> Option<Reg> {
    if !c.regs.ensure(reg) {
        let bkp = c.regs.take_any(MemSize::QWord).expect("register available");
        let reg = c.regs.switch_size(reg, MemSize::QWord);
        asm::code!(c.code, Mov, bkp, reg);
        Some(bkp)
    } else {
        None
    }
}

fn restore_reg(c: &mut Compiler, reg: Reg, bkp: Option<Reg>) {
    if let Some(bkp) = bkp {
        asm::code!(c.code, Mov, reg, bkp);
        c.regs.push(bkp);
    }
}

fn compile_value(c: &mut Compiler, value: &ValueExpr) -> Operand {
    match value {
        ValueExpr::String(string) => {
            let label = Lbl::new();
            c.data.insert(label, string.as_bytes().to_vec());
            let reg = c.regs.take_any(MemSize::QWord).expect("register available");
            asm::code!(c.code, Lea, reg, Mem::lbl(label, MemSize::QWord));
            Operand::Reg(reg)
        }
        ValueExpr::Int(value) => Operand::Imm(Imm::from_i32(*value)),
        ValueExpr::Float(value) => Operand::Imm(Imm::from_f32(*value)),
        ValueExpr::Bool(value) => match value {
            true => Operand::Imm(Imm::TRUE),
            false => Operand::Imm(Imm::FALSE),
        },
        ValueExpr::Identifier(annot, id) => match annot.annotation() {
            Annotation::Value | Annotation::Pointer(..) => Operand::Mem(c.scope.get(id)),
            Annotation::Array(..) => {
                let mem = c.scope.get(id);
                let reg = c.regs.take_any(MemSize::QWord).expect("register available");
                asm::code!(c.code, Lea, reg, mem);
                Operand::Reg(reg)
            }
        },
    }
}

fn compile_binop(c: &mut Compiler, bin_op: &BinaryOp) -> Operand {
    let ope = bin_op.operator();
    let lhs_annot = bin_op.lhs().get_annot();
    let rhs_annot = bin_op.rhs().get_annot();

    let (lhs, rhs) = if ope.is_assign() {
        let rhs = compile_expr_rec(c, bin_op.rhs());
        let rhs = move_operand_to_reg(c, rhs_annot, rhs);

        let lhs = compile_expr_rec(c, bin_op.lhs());
        assert!(lhs.is_mem());

        (lhs, rhs)
    } else {
        let lhs = compile_expr_rec(c, bin_op.lhs());
        let lhs = match bin_op.rhs().is_value() {
            true => move_operand_to_reg(c, lhs_annot, lhs),
            false => move_reg_to_mem(c, lhs_annot, lhs),
        };

        let rhs = compile_expr_rec(c, bin_op.rhs());
        let lhs = move_operand_to_reg(c, lhs_annot, lhs);

        (lhs, rhs)
    };

    match bin_op.is_float() {
        true => compile_fop(c, ope, lhs, rhs),
        false => compile_iop(c, ope, lhs, rhs),
    }
}

fn compile_iop(c: &mut Compiler, ope: Operator, lhs: Operand, rhs: Operand) -> Operand {
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
                _ => move_reg_to_mem(c, TypeAnnot::INT, rhs),
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
                _ => move_reg_to_mem(c, TypeAnnot::INT, rhs),
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
                    let reg = c.regs.take_any(lhs.mem_size()).expect("register available");
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
            let reg = c.regs.take_any(lhs.mem_size()).expect("register available");
            asm::code!(c.code, Mov, reg, lhs);
            asm::code!(c.code, Imul, reg, rhs);
            asm::code!(c.code, Mov, lhs, reg);
            c.regs.push(reg);
            lhs
        }
        Operator::DivAssign => {
            let reg = c.regs.take_any(lhs.mem_size()).expect("register available");
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

fn compile_fop(c: &mut Compiler, ope: Operator, lhs: Operand, mut rhs: Operand) -> Operand {
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
            let reg = c.regs.take_any(MemSize::Byte).expect("register available");
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
            let xmm = c.xmms.take_any(lhs.mem_size()).expect("register available");
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
            let xmm = c.xmms.take_any(lhs.mem_size()).expect("register available");
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

fn compile_call(c: &mut Compiler, func: &FunctionCall) -> Operand {
    reserve_cdecl(c);

    let args_len = func.args().len() as isize;
    let word_size = MemSize::QWord as isize;

    let mut stack_offset = match c.target {
        CompilerTarget::Linux => 0.max(args_len - 6) * word_size,
        CompilerTarget::Windows => 32 + 0.max(args_len - 4) * word_size,
    };

    c.scope.new_call(stack_offset as usize);

    for (arg_num, arg) in func.args().iter().enumerate().rev() {
        let annot = arg.get_annot();
        let arg = compile_expr_rec(c, arg);

        let reg = cdecl(c, arg_num, annot.mem_size());
        let xmm = cdecl_f(c, arg_num, annot.mem_size());

        if let Some(reg) = reg {
            match arg {
                Operand::Xmm(xmm) => asm::code!(c.code, Movd, reg, xmm),
                _ => asm::code!(c.code, Mov, reg, arg),
            }
        }

        if annot.is_float() && xmm.is_some() {
            // if expression is float also copy to xmm registers
            let xmm = xmm.unwrap();
            match arg {
                Operand::Xmm(arg) if arg == xmm => {}
                Operand::Imm(imm) => {
                    let reg = c.regs.take_any(imm.mem_size()).expect("register available");
                    asm::code!(c.code, Mov, reg, imm);
                    asm::code!(c.code, Movd, xmm, reg);
                    c.regs.push(reg);
                }
                Operand::Xmm(..) | Operand::Mem(..) => {
                    asm::code!(c.code, Movss, xmm, arg)
                }
                Operand::Reg(reg) => asm::code!(c.code, Movd, xmm, reg),
            }
        }

        if reg.is_none() {
            let mem_size = arg.mem_size();
            stack_offset -= word_size;
            let mem = Mem::offset(Reg::Rsp, stack_offset, mem_size);

            match arg {
                Operand::Xmm(..) => asm::code!(c.code, Movss, mem, arg),
                Operand::Reg(reg) => {
                    c.regs.push(reg);
                    asm::code!(c.code, Mov, mem, reg);
                }
                Operand::Mem(..) => {
                    let reg = c.regs.take_any(mem_size).expect("register available");
                    asm::code!(c.code, Mov, reg, arg);
                    asm::code!(c.code, Mov, mem, reg);
                    c.regs.push(reg);
                }
                _ => asm::code!(c.code, Mov, mem, arg),
            }
        }

        c.regs.try_push(arg);
        c.xmms.try_push(arg);
    }

    asm::code!(c.code, Call, func.name());
    release_cdecl(c);

    let func_annot = func.get_annot();

    if func_annot.is_float() {
        let xmm = Xmm::xmm0(func_annot.mem_size());
        c.xmms.take(xmm);
        Operand::Xmm(xmm)
    } else if !func_annot.is_void() {
        let reg = Reg::acc(func_annot.mem_size());
        c.regs.take(reg);
        Operand::Reg(reg)
    } else {
        Operand::Imm(Imm::Dword(0))
    }
}

fn compile_unaop(c: &mut Compiler, una_op: &UnaryOp) -> Operand {
    let operand = una_op.operand();
    match una_op.operator() {
        UnaryOperator::AddressOf => {
            let Expression::Value(ValueExpr::Identifier(.., id)) = operand else {
                panic!("operator AddressOf can only be used on vars");
            };
            let mem = c.scope.get(id);
            let reg = c.regs.take_any(MemSize::QWord).expect("register available");
            asm::code!(c.code, Lea, reg, mem);
            Operand::Reg(reg)
        }
        UnaryOperator::Dereference => {
            let expr_ty = operand.get_annot().deref();
            let size = expr_ty.mem_size();
            let operand = compile_expr_rec(c, operand);
            match operand {
                Operand::Reg(reg) => Operand::Mem(Mem::reg(reg, size)),
                Operand::Mem(mem) => {
                    let reg = c.regs.take_any(MemSize::QWord).expect("register available");
                    asm::code!(c.code, Mov, reg, mem);
                    Operand::Mem(Mem::reg(reg, size))
                }
                _ => panic!("invalid operand type for dereference"),
            }
        }
        UnaryOperator::Minus => {
            let expr_ty = operand.get_annot();
            let operand = compile_expr_rec(c, operand);
            if expr_ty.is_int() {
                match operand {
                    Operand::Reg(..) => {
                        asm::code!(c.code, Neg, operand);
                        operand
                    }
                    Operand::Imm(..) | Operand::Mem(..) => {
                        let reg = c
                            .regs
                            .take_any(operand.mem_size())
                            .expect("register available");
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
        UnaryOperator::Not => {
            let operand = compile_expr_rec(c, operand);
            match operand {
                Operand::Reg(..) | Operand::Mem(..) => {
                    asm::code!(c.code, Xor, operand, Imm::Byte(0xFF));
                    asm::code!(c.code, And, operand, Imm::Byte(0x01));
                    operand
                }
                Operand::Imm(imm) => {
                    let reg = c.regs.take_any(imm.mem_size()).expect("register available");
                    asm::code!(c.code, Mov, reg, imm);
                    asm::code!(c.code, Xor, reg, Imm::Byte(0xFF));
                    asm::code!(c.code, And, reg, Imm::Byte(0x01));
                    Operand::Reg(reg)
                }
                Operand::Xmm(..) => unimplemented!(),
            }
        }
        UnaryOperator::BitwiseNot => {
            let operand = compile_expr_rec(c, operand);
            match operand {
                Operand::Reg(..) | Operand::Mem(..) => {
                    asm::code!(c.code, Not, operand);
                    operand
                }
                Operand::Imm(imm) => {
                    let reg = c.regs.take_any(imm.mem_size()).expect("register available");
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
            }
        }
    }
}

fn compile_index(c: &mut Compiler, index: &Indexing) -> Operand {
    let size = index.get_annot().mem_size();

    let array = compile_value(c, index.array());
    let reg = match compile_expr_rec(c, index.index()) {
        Operand::Reg(reg) => reg,
        offset => {
            let reg = c
                .regs
                .take_any(offset.mem_size())
                .expect("register available");
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

fn compile_cast(c: &mut Compiler, cast: &TypeCast) -> Operand {
    let value = cast.value();
    let cast_from = value.get_annot();
    let cast_to = cast.get_annot();

    let operand = compile_expr_rec(c, value);
    if cast_from.is_float() && cast_to.is_int() {
        match operand {
            Operand::Mem(..) | Operand::Xmm(..) => {
                let reg = c
                    .regs
                    .take_any(operand.mem_size())
                    .expect("register available");
                asm::code!(c.code, Cvtss2si, reg, operand);
                Operand::Reg(reg)
            }
            Operand::Imm(imm) => {
                let tmp = c.scope.new_temp(operand.mem_size());
                let reg = c
                    .regs
                    .take_any(operand.mem_size())
                    .expect("register available");
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
                let xmm = c
                    .xmms
                    .take_any(operand.mem_size())
                    .expect("register available");
                asm::code!(c.code, Cvtsi2ss, xmm, operand);
                Operand::Xmm(xmm)
            }
            Operand::Imm(imm) => {
                let tmp = c.scope.new_temp(operand.mem_size());
                let xmm = c
                    .xmms
                    .take_any(operand.mem_size())
                    .expect("register available");
                asm::code!(c.code, Mov, tmp, imm);
                asm::code!(c.code, Cvtsi2ss, xmm, tmp);
                Operand::Xmm(xmm)
            }
            Operand::Xmm(..) => unimplemented!(),
        }
    } else {
        todo!("conversion between {cast_from} to {cast_to} not implemented yet");
    }
}

fn compile_expr_rec(c: &mut Compiler, expr: &Expression) -> Operand {
    match expr {
        Expression::Value(value) => compile_value(c, value),
        Expression::UnaOp(una_op) => compile_unaop(c, una_op),
        Expression::BinOp(bin_op) => compile_binop(c, bin_op),
        Expression::Func(func) => compile_call(c, func),
        Expression::Index(index) => compile_index(c, index),
        Expression::Cast(cast) => compile_cast(c, cast),
    }
}

fn compile_expr(c: &mut Compiler, expr: &Expression) -> Operand {
    let operand = compile_expr_rec(c, expr);
    c.scope.reset_temps();
    return operand;
}

fn compile_node(c: &mut Compiler, node: &AstNode) {
    match node {
        AstNode::Var(annot, ident, expr) => {
            let local = if let Annotation::Array(size) = annot.annotation() {
                let mem_size = annot.inner_type().mem_size();
                c.scope.new_array(ident, size, mem_size)
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
                        let reg = c.regs.take_any(mem.mem_size()).expect("register available");
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
                let reg = c.regs.take_any(imm.mem_size()).expect("register available");
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
                let reg = c.regs.take_any(imm.mem_size()).expect("register available");
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
                    move_operand_to_reg(c, expr.get_annot(), value)
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
            if expr.get_annot().is_float() {
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

fn compile_args(c: &mut Compiler, args: &[Argument]) {
    let mut mem_offset = match c.target {
        CompilerTarget::Linux => 16,   // rip + rbp
        CompilerTarget::Windows => 48, // rip + rbp + shadow space
    };
    for (arg_num, arg) in args.iter().enumerate() {
        let Argument { name, annot } = arg;
        let mem_size = annot.mem_size();
        if let Some(reg) = cdecl(c, arg_num, mem_size) {
            let addr = c.scope.new_local(name, mem_size);
            asm::code!(c.code, Mov, addr, reg);
        } else {
            let addr = Mem::offset(Reg::Rbp, mem_offset, mem_size);
            mem_offset += MemSize::QWord as isize;
            c.scope.set(name, addr);
        }
    }
}

fn compile_func(c: &mut Compiler, ident: &Identifier, args: &Vec<Argument>, nodes: &Vec<AstNode>) {
    asm::code!(c.code, "{ident}:");
    asm::code!(c.code, Push, Reg::Rbp);
    asm::code!(c.code, Mov, Reg::Rbp, Reg::Rsp);

    let mut code = std::mem::take(&mut c.code);

    c.scope.new_inner();
    compile_args(c, args);

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

fn compile_root(c: &mut Compiler, node: &AstRoot) {
    match node {
        AstRoot::Func(ident, args, _, ast_nodes) => {
            compile_func(c, ident, args, ast_nodes);
        }
        AstRoot::ExternFunc(name, ..) => asm::code!(c.code, "extern {name}"),
        AstRoot::Global(annot, ident, value) => {
            let label = Lbl::from_label(ident);
            c.scope.set(ident, Mem::lbl(label, annot.mem_size()));
            match value {
                Some(value) => match value {
                    ValueExpr::String(s) => {
                        c.data.insert(label, s.as_bytes().to_vec());
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
    }
}
