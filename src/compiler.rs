use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::asm::{
    self, AsmBuilder, BaseOperandManager, Imm, Lbl, Mem, MemSize, MemSized, Operand,
    OperandManager, Reg, RegManager, Xmm,
};
use crate::ast::{
    Annotated, Annotation, Argument, Ast, AstNode, AstRoot, BinaryOp, Expression, FunctionCall,
    Identifier, Indexing, Operator, TypeAnnot, UnaryOp, UnaryOperator, ValueExpr, VarType,
};
use crate::intrinsic::intrisic;
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
    pub fn get_max(&self) -> i64 {
        self.local_off.abs() as i64 + self.max_temp_off.abs() as i64 + self.max_func as i64
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

    fn new_temp(&mut self, mem_size: MemSize) -> Operand {
        let mut mem = self.ctx.borrow_mut();
        mem.temp_off -= mem_size as isize;
        if mem.temp_off < mem.max_temp_off {
            mem.max_temp_off = mem.temp_off;
        }
        let offset = mem.local_off + mem.temp_off;
        Operand::Mem(Mem::offset(Reg::Rbp, offset, mem_size))
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
            Annotation::Pointer | Annotation::Array(..) => MemSize::QWord,
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

    pub fn prolog(mut self) -> Self {
        asm::code!(self.code, "bits 64");
        asm::code!(self.code, "section .text");
        asm::code!(self.code, "global main");
        self
    }

    pub fn compile(mut self, ast: Ast) -> Self {
        for node in ast.iter() {
            match node {
                AstRoot::Use(ident) => intrisic(&ident, &mut self.code, self.target),
                AstRoot::Func(ident, args, _, ast_nodes) => {
                    compile_func(&mut self, ident, args, ast_nodes);
                }
                AstRoot::ExternFunc(name, ..) => asm::code!(self.code, "extern {name}"),
                AstRoot::Global(annot, ident, value) => {
                    let label = Lbl::from_id(ident);
                    self.scope.set(ident, Mem::lbl(label, annot.mem_size()));
                    match value {
                        Some(value) => match value {
                            ValueExpr::String(s) => {
                                self.data.insert(label, s.as_bytes().to_vec());
                            }
                            ValueExpr::Int(i) => {
                                self.data.insert(label, i.to_le_bytes().to_vec());
                            }
                            ValueExpr::Float(f) => {
                                self.data.insert(label, f.to_le_bytes().to_vec());
                            }
                            ValueExpr::Bool(b) => {
                                self.data.insert(label, [*b as u8].to_vec());
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
        self
    }

    pub fn epilog(mut self) -> Self {
        if !self.data.is_empty() {
            asm::code!(self.code, "section .data");

            for (label, bytes) in self.data.iter() {
                asm::code!(self.code, "  {label}: db {:x}", HexSlice::new(bytes));
            }
        }
        self
    }

    pub fn assembly(self) -> String {
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

fn reserve_cdecl(c: &mut Compiler) {
    let regs: &[Reg] = match c.target {
        CompilerTarget::Linux => &[Reg::Rdi, Reg::Rsi, Reg::Rdx, Reg::Rcx, Reg::R8, Reg::R9],
        CompilerTarget::Windows => &[Reg::Rcx, Reg::Rdx, Reg::R8, Reg::R9],
    };
    for reg in regs {
        c.regs.take(*reg);
    }
}

fn release_cdecl(c: &mut Compiler) {
    let regs: &[Reg] = match c.target {
        CompilerTarget::Linux => &[Reg::Rdi, Reg::Rsi, Reg::Rdx, Reg::Rcx, Reg::R8, Reg::R9],
        CompilerTarget::Windows => &[Reg::Rcx, Reg::Rdx, Reg::R8, Reg::R9],
    };
    for reg in regs {
        c.regs.push(*reg);
    }
}

fn move_operand_to_reg(c: &mut Compiler, annot: TypeAnnot, operand: Operand) -> Operand {
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
            c.regs.try_push(operand);
            let reg = c.regs.take_any(mem.mem_size());
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

fn compile_value(c: &mut Compiler, value: &ValueExpr) -> Operand {
    match value {
        ValueExpr::String(string) => {
            let label = Lbl::from_str(&string);
            c.data.insert(label, string.as_bytes().to_vec());
            let reg = c.regs.take_any(MemSize::QWord);
            asm::code!(c.code, Lea, reg, Mem::lbl(label, MemSize::QWord));
            Operand::Reg(reg)
        }
        ValueExpr::Int(value) => Operand::Imm(Imm::Int32(*value)),
        ValueExpr::Float(value) => Operand::Imm(Imm::Float32(*value)),
        ValueExpr::Bool(value) => match value {
            true => Operand::Imm(Imm::TRUE),
            false => Operand::Imm(Imm::FALSE),
        },
        ValueExpr::Identifier(annot, id) => match annot.annotation() {
            Annotation::Value | Annotation::Pointer => Operand::Mem(c.scope.get(id)),
            Annotation::Array(..) => {
                let mem = c.scope.get(id);
                let reg = c.regs.take_any(MemSize::QWord);
                asm::code!(c.code, Lea, reg, mem);
                Operand::Reg(reg)
            }
        },
    }
}

fn compile_binop(c: &mut Compiler, bin_op: &BinaryOp) -> Operand {
    let ope = bin_op.operator();
    let annot = bin_op.get_annot();

    let (lhs, rhs) = if ope.is_assign() {
        let rhs = compile_expr_rec(c, bin_op.rhs());
        let rhs = move_operand_to_reg(c, annot, rhs);

        let lhs = compile_expr_rec(c, bin_op.lhs());
        assert!(lhs.is_mem());

        (lhs, rhs)
    } else {
        let lhs = compile_expr_rec(c, bin_op.lhs());
        let lhs = move_operand_to_reg(c, annot, lhs);

        let rhs = compile_expr_rec(c, bin_op.rhs());

        (lhs, rhs)
    };

    match annot.is_float() {
        true => compile_fop(c, ope, lhs, rhs),
        false => compile_iop(c, ope, lhs, rhs),
    }
}

fn compile_iop(c: &mut Compiler, ope: Operator, lhs: Operand, mut rhs: Operand) -> Operand {
    assert!(ope.is_assign() == lhs.is_mem());
    assert!(lhs.mem_size() == rhs.mem_size());

    let result = match ope {
        Operator::Equal => {
            asm::code!(c.code, Cmp, lhs, rhs);
            let lhs = c.regs.switch_size(lhs.expect_reg(), MemSize::Byte).into();
            asm::code!(c.code, Sete, lhs);
            lhs
        }
        Operator::NotEqual => {
            asm::code!(c.code, Cmp, lhs, rhs);
            let lhs = c.regs.switch_size(lhs.expect_reg(), MemSize::Byte).into();
            asm::code!(c.code, Setne, lhs);
            lhs
        }
        Operator::Greater => {
            asm::code!(c.code, Cmp, lhs, rhs);
            let lhs = c.regs.switch_size(lhs.expect_reg(), MemSize::Byte).into();
            asm::code!(c.code, Setg, lhs);
            lhs
        }
        Operator::GreaterEqual => {
            asm::code!(c.code, Cmp, lhs, rhs);
            let lhs = c.regs.switch_size(lhs.expect_reg(), MemSize::Byte).into();
            asm::code!(c.code, Setge, lhs);
            lhs
        }
        Operator::Less => {
            asm::code!(c.code, Cmp, lhs, rhs);
            let lhs = c.regs.switch_size(lhs.expect_reg(), MemSize::Byte).into();
            asm::code!(c.code, Setl, lhs);
            lhs
        }
        Operator::LessEqual => {
            asm::code!(c.code, Cmp, lhs, rhs);
            let lhs = c.regs.switch_size(lhs.expect_reg(), MemSize::Byte).into();
            asm::code!(c.code, Setle, lhs);
            lhs
        }
        Operator::And => {
            asm::code!(c.code, And, lhs, rhs);
            lhs
        }
        Operator::Or => {
            asm::code!(c.code, Or, lhs, rhs);
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
            rhs = move_operand_to_reg(c, TypeAnnot::INT, rhs);
            let (acc, bkp) = match lhs {
                Operand::Reg(acc) if acc.is_acc() => (acc, None),
                _ => {
                    let bkp = c.regs.take_any(MemSize::QWord);
                    let acc = Reg::acc(lhs.mem_size());
                    asm::code!(c.code, Mov, bkp, Reg::acc(MemSize::QWord));
                    asm::code!(c.code, Mov, acc, lhs);
                    (acc, Some(bkp))
                }
            };
            // TODO: also backup Edx
            c.regs.ensure(Reg::Edx);
            asm::code!(c.code, Mov, Reg::Edx, Imm::Int32(0)); // remainder
            asm::code!(c.code, Idiv, rhs); // divisor (rhs)
            if let Some(reg) = bkp {
                c.regs.push(reg);
                asm::code!(c.code, Mov, lhs, acc);
                asm::code!(c.code, Mov, Reg::acc(MemSize::QWord), reg);
            }
            lhs
        }
        Operator::Mod => {
            rhs = move_operand_to_reg(c, TypeAnnot::INT, rhs);
            let bkp = match lhs {
                Operand::Reg(acc) if acc.is_acc() => None,
                _ => {
                    let bkp = c.regs.take_any(MemSize::QWord);
                    asm::code!(c.code, Mov, bkp, Reg::acc(MemSize::QWord));
                    asm::code!(c.code, Mov, Reg::acc(lhs.mem_size()), lhs);
                    Some(bkp)
                }
            };
            // TODO: also backup Edx
            c.regs.ensure(Reg::Edx);
            asm::code!(c.code, Mov, Reg::Edx, Imm::Int32(0)); // remainder
            asm::code!(c.code, Idiv, rhs); // divisor (rhs)
            if let Some(reg) = bkp {
                c.regs.push(reg);
                asm::code!(c.code, Mov, Reg::acc(MemSize::QWord), reg);
            }
            asm::code!(c.code, Mov, lhs, Reg::dta(lhs.mem_size()));
            lhs
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
            asm::code!(c.code, Mov, Reg::Edx, Imm::Int32(0)); // remainder
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
        rhs = c.scope.new_temp(imm.mem_size());
        asm::code!(c.code, Mov, rhs, imm);
    }

    assert!(lhs.mem_size() == rhs.mem_size());

    let result = match ope {
        Operator::Equal => todo!(),
        Operator::NotEqual => todo!(),
        Operator::Greater => {
            let reg = c.regs.take_any(MemSize::Byte);
            asm::code!(c.code, Comiss, lhs, rhs);
            asm::code!(c.code, Setg, reg);
            Operand::Reg(reg)
        }
        Operator::GreaterEqual => todo!(),
        Operator::Less => {
            let reg = c.regs.take_any(MemSize::Byte);
            asm::code!(c.code, Comiss, lhs, rhs);
            asm::code!(c.code, Setl, reg);
            Operand::Reg(reg)
        }
        Operator::LessEqual => todo!(),
        Operator::And => todo!(),
        Operator::Or => todo!(),
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
        Operator::Mod => todo!(),
        Operator::Assign => {
            assert!(lhs.is_mem() && rhs.is_xmm());
            asm::code!(c.code, Movss, lhs, rhs);
            lhs
        }
        Operator::AddAssign => {
            assert!(lhs.is_mem() && rhs.is_xmm());
            let xmm = c.xmms.take_any(lhs.mem_size());
            asm::code!(c.code, Movss, xmm, lhs);
            asm::code!(c.code, Addss, xmm, rhs);
            asm::code!(c.code, Movss, lhs, xmm);
            lhs
        }
        Operator::SubAssign => {
            assert!(lhs.is_mem() && rhs.is_xmm());
            let xmm = c.xmms.take_any(lhs.mem_size());
            asm::code!(c.code, Movss, xmm, lhs);
            asm::code!(c.code, Subss, xmm, rhs);
            asm::code!(c.code, Movss, lhs, xmm);
            lhs
        }
        Operator::MulAssign => {
            assert!(lhs.is_mem() && rhs.is_xmm());
            let xmm = c.xmms.take_any(lhs.mem_size());
            asm::code!(c.code, Movss, xmm, lhs);
            asm::code!(c.code, Mulss, xmm, rhs);
            asm::code!(c.code, Movss, lhs, xmm);
            lhs
        }
        Operator::DivAssign => {
            assert!(lhs.is_mem() && rhs.is_xmm());
            let xmm = c.xmms.take_any(lhs.mem_size());
            asm::code!(c.code, Divss, xmm, lhs);
            asm::code!(c.code, Addss, xmm, rhs);
            asm::code!(c.code, Movss, lhs, xmm);
            lhs
        }
    };

    c.xmms.try_push(rhs);

    result
}

fn compile_call(c: &mut Compiler, func: &FunctionCall) -> Operand {
    reserve_cdecl(c);

    let mut stack_offset = match c.target {
        CompilerTarget::Linux => 0,
        CompilerTarget::Windows => 32,
    };

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

        if annot.is_float() {
            // if expression is float also copy to xmm registers
            let xmm = xmm.unwrap();
            match arg {
                Operand::Xmm(arg) if arg == xmm => {}
                Operand::Xmm(..) | Operand::Mem(..) | Operand::Imm(..) => {
                    asm::code!(c.code, Movss, xmm, arg)
                }
                Operand::Reg(reg) => asm::code!(c.code, Movd, xmm, reg),
            }
        }

        if reg.is_none() {
            let mem_size = arg.mem_size();
            let mem = Mem::offset(Reg::Rsp, stack_offset, mem_size);
            stack_offset += MemSize::QWord as isize;

            match arg {
                Operand::Xmm(..) => asm::code!(c.code, Movss, mem, arg),
                Operand::Reg(reg) => {
                    c.regs.push(reg);
                    asm::code!(c.code, Mov, mem, reg);
                }
                Operand::Mem(..) => {
                    let reg = c.regs.take_any(mem_size);
                    asm::code!(c.code, Mov, reg, arg);
                    asm::code!(c.code, Mov, mem, reg);
                    c.regs.push(reg);
                }
                _ => asm::code!(c.code, Mov, mem, arg),
            }
        }

        c.regs.try_push(arg);
    }

    asm::code!(c.code, Call, func.name());
    c.scope.new_call(stack_offset as usize);
    release_cdecl(c);

    let func_annot = func.get_annot();
    if !func_annot.is_void() {
        let reg = Reg::acc(func_annot.mem_size());
        c.regs.take(reg);
        Operand::Reg(reg)
    } else {
        Operand::Imm(Imm::Int32(0))
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
            let reg = c.regs.take_any(MemSize::QWord);
            asm::code!(c.code, Lea, reg, mem);
            Operand::Reg(reg)
        }
        UnaryOperator::Dereference => {
            let Expression::Value(ValueExpr::Identifier(annot, id)) = operand else {
                panic!("operator Dereference can only be used on vars");
            };
            let mem = c.scope.get(id);
            let reg = c.regs.take_any(MemSize::QWord);
            let tmp = c.scope.new_temp(annot.mem_size());
            asm::code!(c.code, Mov, reg, mem);
            asm::code!(c.code, Mov, tmp, Mem::reg(reg, annot.mem_size()));
            c.regs.push(reg);
            tmp
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
                    Operand::Reg(..) => {
                        asm::code!(c.code, Xor, operand, Imm::Int32(MINUS_BIT as i32));
                        operand
                    }
                    Operand::Imm(..) | Operand::Mem(..) => {
                        let reg = c.regs.take_any(operand.mem_size());
                        asm::code!(c.code, Mov, reg, operand);
                        asm::code!(c.code, Xor, reg, Imm::Int32(MINUS_BIT as i32));
                        Operand::Reg(reg)
                    }
                    Operand::Xmm(xmm) => {
                        let tmp = c.scope.new_temp(xmm.mem_size());
                        c.xmms.push(xmm);
                        asm::code!(c.code, Movss, tmp, xmm);
                        asm::code!(c.code, Xor, tmp, Imm::Int32(MINUS_BIT as i32));
                        tmp
                    }
                }
            }
        }
        UnaryOperator::Not => todo!(),
        UnaryOperator::BitwiseNot => {
            let operand = compile_expr_rec(c, operand);
            match operand {
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
                    tmp
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
            let reg = c.regs.take_any(offset.mem_size());
            c.regs.try_push(offset);
            asm::code!(c.code, Mov, reg, offset);
            reg
        }
    };

    let reg = c.regs.switch_size(reg, MemSize::QWord);
    asm::code!(c.code, Mul, reg, Imm::Int64(size as i64));
    asm::code!(c.code, Add, reg, array);
    c.regs.try_push(array);

    Operand::Mem(Mem::reg(reg, size))
}

fn compile_expr_rec(c: &mut Compiler, expr: &Expression) -> Operand {
    match expr {
        Expression::Value(value) => compile_value(c, value),
        Expression::UnaOp(una_op) => compile_unaop(c, una_op),
        Expression::BinOp(bin_op) => compile_binop(c, bin_op),
        Expression::Func(func) => compile_call(c, func),
        Expression::Index(index) => compile_index(c, index),
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
                        let reg = c.regs.take_any(mem.mem_size());
                        asm::code!(c.code, Mov, reg, mem);
                        asm::code!(c.code, Mov, local, reg);
                        c.regs.push(reg);
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
                result = Operand::Reg(c.regs.take_any(imm.mem_size()));
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
                result = Operand::Reg(c.regs.take_any(imm.mem_size()));
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
                Some(expr) => compile_value(c, expr),
                None => Imm::Int32(0).into(),
            };

            asm::code!(c.code, Mov, counter, init);
            asm::code!(c.code, "{startfor_lbl}:");

            c.regs.try_push(init);

            match limit {
                ValueExpr::Int(value) => {
                    asm::code!(c.code, Cmp, counter, Imm::Int32(*value));
                }
                ValueExpr::Identifier(.., id) => {
                    asm::code!(c.code, Mov, Reg::Eax, c.scope.get(id));
                    asm::code!(c.code, Cmp, counter, Reg::Eax);
                }
                _ => unreachable!(),
            };

            asm::code!(c.code, Jg, endfor_lbl);

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
                            asm::code!(c.code, Mov, xmm0, tmp);
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
            mem_offset += MemSize::QWord as isize;
            let addr = Mem::offset(Reg::Rbp, mem_offset, mem_size);
            c.scope.set(name, addr);
        }
    }
}

fn compile_func(c: &mut Compiler, ident: &Identifier, args: &Vec<Argument>, nodes: &Vec<AstNode>) {
    asm::code!(c.code, "{ident}:");
    asm::code!(c.code, Push, Reg::Rbp);
    asm::code!(c.code, Mov, Reg::Rbp, Reg::Rsp);

    c.scope.new_inner();

    compile_args(c, args);

    let code = std::mem::take(&mut c.code);

    for inner in nodes {
        compile_node(c, inner);
    }

    let total_mem = c.scope.ctx.borrow().get_max();
    if total_mem > 0 {
        // align by 16
        let total_mem = total_mem + (16 - total_mem % 16);
        asm::code!(c.code, Sub, Reg::Rsp, Imm::Int64(total_mem));
    }

    let inner_code = std::mem::replace(&mut c.code, code);
    c.code.append(inner_code);

    c.scope.exit();

    asm::code!(c.code, ".R:");
    asm::code!(c.code, Mov, Reg::Rsp, Reg::Rbp);
    asm::code!(c.code, Pop, Reg::Rbp);
    asm::code!(c.code, Ret);
}
