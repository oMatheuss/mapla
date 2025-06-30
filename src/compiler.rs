use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::asm::{AsmBuilder, Imm, Lbl, Mem, MemSize, MemSized, Operand, Reg, Xmm};
use crate::ast::{
    Annotated, Annotation, Argument, Ast, AstNode, BinaryOp, Expression, FunctionCall, Identifier,
    Indexing, Operator, TypeAnnot, UnaryOp, UnaryOperator, ValueExpr, VarType,
};
use crate::intrinsic::intrisic;
use crate::target::CompilerTarget;

#[derive(Debug, Clone, Copy, Default)]
struct MemAlloc {
    local_off: isize,
    temp_off: isize,
    max_temp_off: isize,
}

impl MemAlloc {
    pub fn get_total(&self) -> i64 {
        let total = self.local_off.abs() + self.max_temp_off.abs();
        return total as i64;
    }
}

#[derive(Debug, Default)]
struct Scope {
    var: HashMap<String, Mem>,
    mem: Rc<RefCell<MemAlloc>>,
    lbl: Rc<RefCell<usize>>,
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
            mem: cur.mem.clone(),
            lbl: cur.lbl.clone(),
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
        let mut mem = self.mem.borrow_mut();
        mem.local_off -= mem_size as isize;
        let mem = Mem::offset(Reg::Rbp, mem.local_off, mem_size);
        self.var.insert(String::from(ident), mem);
        Operand::Mem(mem)
    }

    fn new_array(&mut self, ident: &str, size: u32, mem_size: MemSize) -> Operand {
        let mut mem = self.mem.borrow_mut();
        mem.local_off -= mem_size as isize * size as isize;
        let mem = Mem::offset(Reg::Rbp, mem.local_off, MemSize::QWord);
        self.var.insert(String::from(ident), mem);
        Operand::Mem(mem)
    }

    fn new_temp(&mut self, mem_size: MemSize) -> Operand {
        let mut mem = self.mem.borrow_mut();
        mem.temp_off -= mem_size as isize;
        if mem.temp_off < mem.max_temp_off {
            mem.max_temp_off = mem.temp_off;
        }
        let offset = mem.local_off + mem.temp_off;
        Operand::Mem(Mem::offset(Reg::Rbp, offset, mem_size))
    }

    #[inline]
    fn reset_temps(&mut self) {
        self.mem.borrow_mut().temp_off = 0;
    }

    #[inline]
    fn new_label(&mut self) -> String {
        let mut lbl = self.lbl.borrow_mut();
        *lbl += 1;
        format!(".L{lbl}", lbl = *lbl)
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
}

impl Compiler {
    pub fn new(target: CompilerTarget) -> Self {
        Compiler {
            code: AsmBuilder::new(),
            scope: Scope::new(),
            data: AsmData::new(),
            target,
        }
    }

    pub fn prologue(mut self) -> Self {
        self.code.bits(64);
        self.code.section("text");
        self.code.global(&["main"]);
        self
    }

    pub fn compile(mut self, ast: Ast) -> Self {
        for node in ast.iter() {
            match node {
                AstNode::Use(ident) => intrisic(&ident, &mut self.code, self.target),
                AstNode::Func(ident, args, _, ast_nodes) => {
                    compile_func(&mut self, ident, args, ast_nodes);
                }
                _ => {}
            }
        }
        self
    }

    pub fn epilogue(mut self) -> Self {
        if !self.data.is_empty() {
            self.code.section("data");

            for (label, val) in self.data.iter() {
                self.code.db(&label.to_string(), val);
            }
        }
        self
    }

    pub fn assembly(self) -> String {
        self.code.to_string()
    }
}

/// This function will always tries to move any type of operand to the default operation registers (Rax and Xmm0)
fn move_operand_to_reg(c: &mut Compiler, annot: TypeAnnot, operand: Operand) -> Operand {
    match operand {
        Operand::Xmm(xmm) if !xmm.is_xmm0() => {
            let xmm0 = Xmm::xmm0(xmm.mem_size());
            c.code.movss(xmm0, xmm);
            Operand::Xmm(xmm0)
        }
        Operand::Mem(mem) if annot.is_float() => {
            let xmm = Xmm::xmm0(mem.mem_size());
            c.code.movss(xmm, mem);
            Operand::Xmm(xmm)
        }
        Operand::Reg(reg) if annot.is_float() => {
            let mem = c.scope.new_temp(reg.mem_size());
            let xmm = Xmm::xmm0(mem.mem_size());
            c.code.mov(mem, reg);
            c.code.movss(xmm, mem);
            Operand::Xmm(xmm)
        }
        Operand::Imm(imm) if annot.is_float() => {
            let xmm = Xmm::xmm0(imm.mem_size());
            let tmp = c.scope.new_temp(imm.mem_size());
            c.code.mov(tmp, imm);
            c.code.movss(xmm, tmp);
            Operand::Xmm(xmm)
        }
        Operand::Reg(reg) if !reg.is_acc() => {
            let acc = Reg::acc(reg.mem_size());
            c.code.mov(acc, reg);
            Operand::Reg(acc)
        }
        Operand::Mem(..) | Operand::Imm(..) => {
            let reg = Reg::acc(operand.mem_size());
            c.code.mov(reg, operand);
            Operand::Reg(reg)
        }
        _ => operand,
    }
}

/// This function will try to clear the default operation registers (Rax and Xmm0)
fn move_reg_to_mem(c: &mut Compiler, operand: Operand) -> Operand {
    match operand {
        Operand::Xmm(xmm) => {
            let tmp = c.scope.new_temp(xmm.mem_size());
            c.code.movss(tmp, xmm);
            tmp
        }
        Operand::Reg(..) => {
            let tmp = c.scope.new_temp(operand.mem_size());
            c.code.mov(tmp, operand);
            tmp
        }
        _ => operand,
    }
}

fn compile_value(c: &mut Compiler, value: &ValueExpr) -> Operand {
    match value {
        ValueExpr::String(string) => {
            let label = Lbl::from_str(&string);
            c.data.insert(label, string.as_bytes().to_vec());
            c.code.lea(Reg::Rdx, Mem::lbl(label, MemSize::QWord));
            Operand::Reg(Reg::Rdx)
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
                c.code.lea(Reg::Rdx, mem);
                Operand::Reg(Reg::Rdx)
            }
        },
    }
}

fn compile_binop(c: &mut Compiler, bin_op: &BinaryOp) -> Operand {
    let operator = bin_op.operator();
    let annot = bin_op.get_annot();

    let (lhs, rhs) = if operator.is_assign() {
        let rhs = compile_expr_rec(c, bin_op.rhs());
        let rhs = move_operand_to_reg(c, annot, rhs);

        let lhs = compile_expr_rec(c, bin_op.lhs());
        assert!(lhs.is_mem());

        (lhs, rhs)
    } else {
        let mut lhs = compile_expr_rec(c, bin_op.lhs());

        if !bin_op.rhs().is_value() {
            if bin_op.lhs().is_index() {
                lhs = move_operand_to_reg(c, annot, lhs);
            }
            lhs = move_reg_to_mem(c, lhs);
        }

        let rhs = compile_expr_rec(c, bin_op.rhs());
        let rhs = move_reg_to_mem(c, rhs);

        let lhs = move_operand_to_reg(c, annot, lhs);

        (lhs, rhs)
    };

    match annot.is_float() {
        true => compile_fop(c, operator, lhs, rhs),
        false => compile_iop(c, operator, lhs, rhs),
    }
}

fn compile_iop(c: &mut Compiler, ope: Operator, lhs: Operand, mut rhs: Operand) -> Operand {
    assert!(ope.is_assign() == lhs.is_mem());
    assert!(lhs.mem_size() == rhs.mem_size());

    match ope {
        Operator::Equal => {
            c.code.cmp(lhs, rhs);
            c.code.sete(Reg::Al);
            Operand::Reg(Reg::Al)
        }
        Operator::NotEqual => {
            c.code.cmp(lhs, rhs);
            c.code.setne(Reg::Al);
            Operand::Reg(Reg::Al)
        }
        Operator::Greater => {
            c.code.cmp(lhs, rhs);
            c.code.setg(Reg::Al);
            Operand::Reg(Reg::Al)
        }
        Operator::GreaterEqual => {
            c.code.cmp(lhs, rhs);
            c.code.setge(Reg::Al);
            Operand::Reg(Reg::Al)
        }
        Operator::Less => {
            c.code.cmp(lhs, rhs);
            c.code.setl(Reg::Al);
            Operand::Reg(Reg::Al)
        }
        Operator::LessEqual => {
            c.code.cmp(lhs, rhs);
            c.code.setle(Reg::Al);
            Operand::Reg(Reg::Al)
        }
        Operator::And => {
            c.code.and(lhs, rhs);
            lhs
        }
        Operator::Or => {
            c.code.or(lhs, rhs);
            lhs
        }
        Operator::Add => {
            c.code.add(lhs, rhs);
            lhs
        }
        Operator::Sub => {
            c.code.sub(lhs, rhs);
            lhs
        }
        Operator::Mul => {
            c.code.imul(lhs, rhs);
            lhs
        }
        Operator::Div => {
            assert!(matches!(lhs, Operand::Reg(Reg::Eax)));
            if rhs.is_imm() {
                c.code.mov(Reg::Ebx, rhs);
                rhs = Operand::Reg(Reg::Ebx);
            }
            c.code.mov(Reg::Edx, Imm::Int32(0)); // remainder
            c.code.idiv(rhs); // divisor (rhs)
            lhs
        }
        Operator::Mod => {
            assert!(matches!(lhs, Operand::Reg(Reg::Eax)));
            if rhs.is_imm() {
                c.code.mov(Reg::Ebx, rhs);
                rhs = Operand::Reg(Reg::Ebx);
            }
            c.code.mov(Reg::Edx, Imm::Int32(0)); // remainder
            c.code.idiv(rhs); // divisor (rhs)
            c.code.mov(lhs, Reg::Edx);
            lhs
        }
        Operator::Assign => {
            c.code.mov(lhs, rhs);
            lhs
        }
        Operator::AddAssign => {
            c.code.add(lhs, rhs);
            lhs
        }
        Operator::SubAssign => {
            c.code.sub(lhs, rhs);
            lhs
        }
        Operator::MulAssign => {
            let reg = Reg::cnt(lhs.mem_size());
            c.code.mov(reg, lhs);
            c.code.imul(reg, rhs);
            c.code.mov(lhs, reg);
            lhs
        }
        Operator::DivAssign => {
            let reg = Reg::cnt(lhs.mem_size());
            c.code.mov(reg, lhs); // quotient (lhs)
            c.code.mov(Reg::Edx, Imm::Int32(0)); // remainder
            c.code.idiv(rhs); // divisor (rhs)
            c.code.mov(lhs, reg);
            lhs
        }
    }
}

fn compile_fop(c: &mut Compiler, operator: Operator, lhs: Operand, mut rhs: Operand) -> Operand {
    if let Operand::Imm(imm) = rhs {
        rhs = c.scope.new_temp(imm.mem_size());
        c.code.mov(rhs, imm);
    }

    assert!(lhs.mem_size() == rhs.mem_size());

    match operator {
        Operator::Equal => todo!(),
        Operator::NotEqual => todo!(),
        Operator::Greater => {
            c.code.comiss(lhs, rhs);
            c.code.setg(Reg::Al);
            Operand::Reg(Reg::Al)
        }
        Operator::GreaterEqual => todo!(),
        Operator::Less => {
            c.code.comiss(lhs, rhs);
            c.code.setl(Reg::Al);
            Operand::Reg(Reg::Al)
        }
        Operator::LessEqual => todo!(),
        Operator::And => todo!(),
        Operator::Or => todo!(),
        Operator::Add => {
            c.code.addss(lhs, rhs);
            lhs
        }
        Operator::Sub => {
            c.code.subss(lhs, rhs);
            lhs
        }
        Operator::Mul => {
            c.code.mulss(lhs, rhs);
            lhs
        }
        Operator::Div => {
            c.code.divss(lhs, rhs);
            lhs
        }
        Operator::Mod => todo!(),
        Operator::Assign => {
            assert!(lhs.is_mem() && rhs.is_xmm());
            c.code.movss(lhs, rhs);
            lhs
        }
        Operator::AddAssign => {
            assert!(lhs.is_mem() && rhs.is_xmm());
            let xmm = Xmm::xmm1(lhs.mem_size());
            c.code.movss(xmm, lhs);
            c.code.addss(xmm, rhs);
            c.code.movss(lhs, xmm);
            lhs
        }
        Operator::SubAssign => {
            assert!(lhs.is_mem() && rhs.is_xmm());
            let xmm = Xmm::xmm1(lhs.mem_size());
            c.code.movss(xmm, lhs);
            c.code.subss(xmm, rhs);
            c.code.movss(lhs, xmm);
            lhs
        }
        Operator::MulAssign => {
            assert!(lhs.is_mem() && rhs.is_xmm());
            let xmm = Xmm::xmm1(lhs.mem_size());
            c.code.movss(xmm, lhs);
            c.code.mulss(xmm, rhs);
            c.code.movss(lhs, xmm);
            lhs
        }
        Operator::DivAssign => {
            assert!(lhs.is_mem() && rhs.is_xmm());
            let xmm = Xmm::xmm1(lhs.mem_size());
            c.code.divss(xmm, lhs);
            c.code.addss(xmm, rhs);
            c.code.movss(lhs, xmm);
            lhs
        }
    }
}

fn compile_call(c: &mut Compiler, func: &FunctionCall) -> Operand {
    for arg in func.args().iter().rev() {
        let arg = compile_expr_rec(c, arg);
        let mem_size = arg.mem_size();
        let mem = Mem::reg(Reg::Rsp, mem_size);
        // TODO: allocate space for parameters all at once
        c.code.sub(Reg::Rsp, Imm::Int64(mem_size as i64));

        if arg.is_xmm() {
            c.code.movss(mem, arg);
        } else if arg.is_mem() {
            let a_reg = Reg::acc(mem_size);
            c.code.mov(a_reg, arg);
            c.code.mov(mem, a_reg);
        } else {
            c.code.mov(mem, arg);
        }
    }
    c.code.call(func.name());
    if !func.get_annot().is_void() {
        let ret_size = func.get_annot().mem_size();
        Operand::Reg(Reg::acc(ret_size))
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
            c.code.lea(Reg::Rax, mem);
            Operand::Reg(Reg::Rax)
        }
        UnaryOperator::Dereference => {
            let Expression::Value(ValueExpr::Identifier(annot, id)) = operand else {
                panic!("operator Dereference can only be used on vars");
            };
            let mem = c.scope.get(id);
            c.code.mov(Reg::Rax, mem);
            let size = annot.mem_size();
            let temp = c.scope.new_temp(size);
            c.code.mov(temp, Mem::reg(Reg::Rax, size));
            temp
        }
        UnaryOperator::Minus => {
            let expr_ty = operand.get_annot();
            let operand = compile_expr_rec(c, operand);
            if expr_ty.is_int() {
                match operand {
                    Operand::Reg(..) => {
                        c.code.neg(operand);
                        operand
                    }
                    Operand::Imm(..) | Operand::Mem(..) => {
                        let reg = Reg::acc(operand.mem_size());
                        c.code.mov(reg, operand);
                        c.code.neg(reg);
                        Operand::Reg(reg)
                    }
                    _ => panic!("operator minus could not be applied"),
                }
            } else {
                const MINUS_BIT: u32 = 1 << 31;
                match operand {
                    Operand::Reg(..) => {
                        c.code.xor(operand, Imm::Int32(MINUS_BIT as i32));
                        operand
                    }
                    Operand::Imm(..) | Operand::Mem(..) => {
                        let reg = Reg::acc(operand.mem_size());
                        c.code.mov(reg, operand);
                        c.code.xor(reg, Imm::Int32(MINUS_BIT as i32));
                        Operand::Reg(reg)
                    }
                    Operand::Xmm(xmm) => {
                        let tmp = c.scope.new_temp(xmm.mem_size());
                        c.code.movss(tmp, xmm);
                        c.code.xor(tmp, Imm::Int32(MINUS_BIT as i32));
                        tmp
                    }
                    _ => panic!("operator minus could not be applied"),
                }
            }
        }
        UnaryOperator::Not => todo!(),
        UnaryOperator::BitwiseNot => {
            let operand = compile_expr_rec(c, operand);
            match operand {
                Operand::Reg(..) | Operand::Mem(..) => {
                    c.code.not(operand);
                    operand
                }
                Operand::Imm(imm) => {
                    let reg = Reg::acc(imm.mem_size());
                    c.code.mov(reg, imm);
                    c.code.not(reg);
                    Operand::Reg(reg)
                }
                Operand::Xmm(xmm) => {
                    let tmp = c.scope.new_temp(xmm.mem_size());
                    c.code.movss(tmp, xmm);
                    c.code.not(tmp);
                    tmp
                }
                _ => panic!("operator minus could not be applied"),
            }
        }
    }
}

fn compile_index(c: &mut Compiler, index: &Indexing) -> Operand {
    let size = index.get_annot().mem_size();

    let offset = compile_expr_rec(c, index.index());
    c.code.mov(Reg::cnt(offset.mem_size()), offset);
    c.code.imul(Reg::Rcx, Imm::Int64(size as i64));

    let array = compile_value(c, index.array());
    c.code.add(Reg::Rcx, array);

    Operand::Mem(Mem::reg(Reg::Rcx, size))
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
                if result.is_xmm() {
                    c.code.movss(local, result);
                } else if result.is_mem() {
                    let reg_a = Reg::acc(result.mem_size());
                    c.code.mov(reg_a, result);
                    c.code.mov(local, reg_a);
                } else {
                    c.code.mov(local, result);
                }
            }
        }
        AstNode::If(expr, nodes) => {
            let endif_lbl = c.scope.new_label();

            let mut result = compile_expr(c, expr);
            if let Operand::Imm(imm) = result {
                result = Operand::Reg(Reg::acc(imm.mem_size()));
                c.code.mov(result, imm);
            }
            c.code.cmp(result, Imm::FALSE);
            c.code.je(&endif_lbl);

            c.scope.continue_on();

            for inner in nodes {
                compile_node(c, inner);
            }

            c.scope.exit();

            c.code.label(&endif_lbl);
        }
        AstNode::While(expr, nodes) => {
            let startwhile_lbl = c.scope.new_label();
            let endwhile_lbl = c.scope.new_label();

            c.code.label(&startwhile_lbl);

            let mut result = compile_expr(c, expr);
            if let Operand::Imm(imm) = result {
                result = Operand::Reg(Reg::acc(imm.mem_size()));
                c.code.mov(result, imm);
            }
            c.code.cmp(result, Imm::FALSE);
            c.code.je(&endwhile_lbl);

            c.scope.continue_on();

            for inner in nodes {
                compile_node(c, inner);
            }

            c.scope.exit();

            c.code.jmp(&startwhile_lbl);
            c.code.label(&endwhile_lbl);
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

            c.code.mov(counter, init);
            c.code.label(&startfor_lbl);

            match limit {
                ValueExpr::Int(value) => {
                    c.code.cmp(counter, Imm::Int32(*value));
                }
                ValueExpr::Identifier(.., id) => {
                    c.code.mov(Reg::Eax, c.scope.get(id));
                    c.code.cmp(counter, Reg::Eax);
                }
                _ => unreachable!(),
            };

            c.code.jg(&endfor_lbl);

            for inner in nodes {
                compile_node(c, inner);
            }

            c.scope.exit();

            c.code.inc(counter);
            c.code.jmp(&startfor_lbl);
            c.code.label(&endfor_lbl);
        }
        AstNode::Expr(expr) => {
            let _ = compile_expr(c, expr);
        }
        AstNode::Ret(expr) => {
            let result = compile_expr(c, expr);
            if !result.is_reg() {
                let mem_size = result.mem_size();
                let reg = Reg::acc(mem_size);
                c.code.mov(reg, result);
            }
            c.code.jmp(".R");
        }
        _ => {}
    }
}

fn set_args(c: &mut Compiler, args: &[Argument]) -> usize {
    let mem_offset = 16; // rip + rbp
    let mut args_size = 0;
    for Argument { name, annot } in args {
        let mem_size = annot.mem_size();
        let offset = mem_offset + args_size as isize;
        let addr = Mem::offset(Reg::Rbp, offset, mem_size);
        args_size += mem_size as usize;
        c.scope.set(name, addr.into());
    }
    return args_size;
}

fn compile_func(c: &mut Compiler, ident: &Identifier, args: &Vec<Argument>, nodes: &Vec<AstNode>) {
    c.code.label(ident);
    c.code.push_sf();

    c.scope.new_inner();

    let args_size = set_args(c, args);

    let mut code = std::mem::take(&mut c.code);

    for inner in nodes {
        compile_node(c, inner);
    }

    let total_mem = c.scope.mem.borrow().get_total();
    if total_mem > 0 {
        code.sub(Reg::Rsp, Imm::Int64(total_mem));
    }

    let inner_code = std::mem::replace(&mut c.code, code);
    c.code.append(inner_code);

    c.scope.exit();

    c.code.label(".R");
    c.code.pop_sf();
    c.code.ret(args_size);
}
