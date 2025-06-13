use std::cell::RefCell;
use std::collections::HashMap;

use crate::asm::{AsmBuilder, Imm, Lbl, Mem, MemSize, Operand, Reg, Xmm};
use crate::ast::{
    Argument, Ast, AstNode, BinaryOp, Expression, FunctionCall, Identifier, Operator, ValueExpr, VarType
};
use crate::intrinsic::intrisic;
use crate::target::CompilerTarget;

struct MemAlloc {
    local_off: isize,
    temp_off: isize,
    max_temp_off: isize,
}

impl MemAlloc {
    pub fn new() -> Self {
        Self {
            local_off: 0,
            temp_off: 0,
            max_temp_off: 0,
        }
    }

    pub fn new_ref_cell() -> RefCell<Self> {
        RefCell::new(Self::new())
    }

    pub fn get_total(&self) -> i64 {
        let total = self.local_off.abs() + self.max_temp_off.abs();
        return total as i64;
    }
}

struct Scope<'a> {
    var: HashMap<String, Operand>,
    mem: &'a RefCell<MemAlloc>,
    lbl: usize,
    sup: Option<&'a Scope<'a>>,
}

impl<'a> Scope<'a> {
    fn new(mem: &'a RefCell<MemAlloc>) -> Self {
        Self {
            var: HashMap::new(),
            mem,
            lbl: 0,
            sup: None,
        }
    }

    fn new_inner(outer: &'a Self, mem: &'a RefCell<MemAlloc>) -> Self {
        Self {
            var: HashMap::new(),
            mem,
            lbl: 0,
            sup: Some(outer),
        }
    }

    fn continue_on(outer: &'a Self) -> Self {
        Self {
            var: HashMap::new(),
            mem: outer.mem,
            lbl: outer.lbl,
            sup: Some(outer),
        }
    }

    fn get(&self, ident: &str) -> &Operand {
        if let Some(local) = self.var.get(ident) {
            local
        } else if let Some(sup) = &self.sup {
            sup.get(ident)
        } else {
            panic!("'{ident}' was not found: variable does not exist")
        }
    }

    #[inline]
    fn set(&mut self, ident: &str, operand: Operand) -> Option<Operand> {
        self.var.insert(String::from(ident), operand)
    }

    fn new_local(&mut self, ident: &str, mem_size: MemSize) -> Operand {
        let mut mem = self.mem.borrow_mut();
        mem.local_off -= mem_size as isize;
        let mem = Mem::offset(Reg::Rbp, mem.local_off, mem_size);
        self.var.insert(String::from(ident), Operand::Mem(mem));
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
        let mut mem = self.mem.borrow_mut();
        mem.temp_off = 0;
    }

    #[inline]
    fn new_label(&mut self) -> String {
        self.lbl += 1;
        format!(".L{lbl}", lbl = self.lbl)
    }
}

type AsmData = HashMap<Lbl, Vec<u8>>;

pub struct Compiler;

impl Compiler {
    fn get_var_size(var_type: VarType) -> MemSize {
        match var_type {
            VarType::Int | VarType::Real => MemSize::DWord,
            VarType::Bool | VarType::Char => MemSize::Byte,
            VarType::Void => panic!("void does not have a known size"),
        }
    }

    fn compile_value(scope: &mut Scope, data: &mut AsmData, value: &ValueExpr) -> Operand {
        match value {
            ValueExpr::String(string) => {
                let label = Lbl::from_str(&string);
                data.insert(label, string.as_bytes().to_vec());
                Operand::Lbl(label)
            }
            ValueExpr::Int(value) => Operand::Imm(Imm::Int32(*value)),
            ValueExpr::Float(value) => Operand::Imm(Imm::Float32(*value)),
            ValueExpr::Bool(value) => match value {
                true => Operand::Imm(Imm::TRUE),
                false => Operand::Imm(Imm::FALSE),
            },
            ValueExpr::Identifier(ident, ..) => *scope.get(ident),
        }
    }

    fn compile_binop(
        code: &mut AsmBuilder,
        scope: &mut Scope,
        data: &mut AsmData,
        bin_op: &BinaryOp,
    ) -> Operand {
        let is_float_expr = bin_op.is_float_expr();

        // depth-first search
        let mut lhs = Self::compile_expr(code, scope, data, bin_op.lhs());

        // clear register
        if let Operand::Reg(reg) = lhs {
            lhs = scope.new_temp(reg.mem_size());
            code.mov(lhs, reg);
        } else if let Operand::Xmm(xmm) = lhs {
            lhs = scope.new_temp(xmm.mem_size());
            code.movss(lhs, xmm);
        }

        let mut rhs = Self::compile_expr(code, scope, data, bin_op.rhs());

        // clear register
        if let Operand::Reg(reg) = rhs {
            rhs = scope.new_temp(reg.mem_size());
            code.mov(rhs, reg);
        } else if let Operand::Xmm(xmm) = rhs {
            lhs = scope.new_temp(xmm.mem_size());
            code.movss(rhs, xmm);
        }

        match is_float_expr {
            true => Self::compile_fop(code, scope, bin_op.operator(), lhs, rhs),
            false => Self::compile_iop(code, bin_op.operator(), lhs, rhs),
        }
    }

    fn compile_iop(
        code: &mut AsmBuilder,
        operator: Operator,
        mut lhs: Operand,
        mut rhs: Operand,
    ) -> Operand {
        // prepare for operation
        if !(lhs.is_reg() || operator.is_assign()) {
            let a_reg = Reg::get_a(lhs.mem_size());
            code.mov(a_reg, lhs);
            lhs = Operand::Reg(a_reg);
        }

        assert!(operator.is_assign() == lhs.is_mem());
        assert!(lhs.mem_size() == rhs.mem_size());

        match operator {
            Operator::Equal => {
                code.cmp(lhs, rhs);
                code.sete(Reg::Al);
                Operand::Reg(Reg::Al)
            }
            Operator::NotEqual => {
                code.cmp(lhs, rhs);
                code.setne(Reg::Al);
                Operand::Reg(Reg::Al)
            }
            Operator::Greater => {
                code.cmp(lhs, rhs);
                code.setg(Reg::Al);
                Operand::Reg(Reg::Al)
            }
            Operator::GreaterEqual => todo!(),
            Operator::Less => {
                code.cmp(lhs, rhs);
                code.setl(Reg::Al);
                Operand::Reg(Reg::Al)
            }
            Operator::LessEqual => todo!(),
            Operator::And => {
                code.and(lhs, rhs);
                lhs
            }
            Operator::Or => {
                code.or(lhs, rhs);
                lhs
            }
            Operator::Add => {
                code.add(lhs, rhs);
                lhs
            }
            Operator::Sub => {
                code.sub(lhs, rhs);
                lhs
            }
            Operator::Mul => {
                code.imul(lhs, rhs);
                lhs
            }
            Operator::Div => {
                if !matches!(lhs, Operand::Reg(Reg::Eax)) {
                    code.mov(Reg::Eax, lhs); // quotient (lhs)
                }
                code.mov(Reg::Edx, Imm::Int32(0)); // remainder
                if rhs.is_imm() {
                    code.mov(Reg::Ebx, rhs);
                    rhs = Operand::Reg(Reg::Ebx);
                }
                code.idiv(rhs); // divisor (rhs)
                lhs
            }
            Operator::Mod => {
                if !matches!(lhs, Operand::Reg(Reg::Eax)) {
                    code.mov(Reg::Eax, lhs); // quotient (lhs)
                }
                code.mov(Reg::Edx, Imm::Int32(0)); // remainder
                if rhs.is_imm() {
                    code.mov(Reg::Ebx, rhs);
                    rhs = Operand::Reg(Reg::Ebx);
                }
                code.idiv(rhs); // divisor (rhs)
                Operand::Reg(Reg::Edx)
            }
            Operator::Assign => {
                code.mov(lhs, rhs);
                lhs
            }
            Operator::AddAssign => {
                if let Operand::Mem(mem) = rhs {
                    code.mov(Reg::Eax, mem);
                    rhs = Operand::Reg(Reg::Eax)
                }
                code.add(lhs, rhs);
                lhs
            }
            Operator::SubAssign => {
                if let Operand::Mem(mem) = rhs {
                    code.mov(Reg::Eax, mem);
                    rhs = Operand::Reg(Reg::Eax)
                }
                code.sub(lhs, rhs);
                lhs
            }
            Operator::MulAssign => {
                if let Operand::Mem(mem) = lhs {
                    code.mov(Reg::Eax, mem);
                    code.imul(Reg::Eax, rhs);
                    code.mov(mem, Reg::Eax);
                } else {
                    // in other case, it is already eax
                    code.imul(lhs, rhs);
                }
                lhs
            }
            Operator::DivAssign => {
                // lhs must be a memory
                code.mov(Reg::Eax, lhs); // quotient (lhs)
                code.mov(Reg::Edx, Imm::Int32(0)); // remainder
                code.idiv(rhs); // divisor (rhs)
                code.mov(lhs, Reg::Eax);
                lhs
            }
        }
    }

    fn compile_fop(
        code: &mut AsmBuilder,
        scope: &mut Scope,
        operator: Operator,
        mut lhs: Operand,
        mut rhs: Operand,
    ) -> Operand {
        let old_lhs = lhs.clone();

        match lhs {
            Operand::Mem(mem) => {
                let xmm0 = Xmm::get_0(mem.mem_size());
                code.movss(xmm0, mem);
                lhs = Operand::Xmm(xmm0);
            }
            Operand::Imm(imm) => {
                lhs = scope.new_temp(imm.mem_size());
                code.mov(lhs, imm);
                let xmm0 = Xmm::get_0(lhs.mem_size());
                code.movss(xmm0, lhs);
                lhs = Operand::Xmm(xmm0);
            }
            _ => {}
        }

        if let Operand::Imm(imm) = rhs {
            rhs = scope.new_temp(imm.mem_size());
            code.mov(rhs, imm);
        }

        assert!(lhs.mem_size() == rhs.mem_size());

        match operator {
            Operator::Equal => todo!(),
            Operator::NotEqual => todo!(),
            Operator::Greater => todo!(),
            Operator::GreaterEqual => todo!(),
            Operator::Less => todo!(),
            Operator::LessEqual => todo!(),
            Operator::And => todo!(),
            Operator::Or => todo!(),
            Operator::Add => {
                code.addss(lhs, rhs);
                lhs
            }
            Operator::Sub => {
                code.subss(lhs, rhs);
                lhs
            }
            Operator::Mul => {
                code.mulss(lhs, rhs);
                lhs
            }
            Operator::Div => {
                code.divss(lhs, rhs);
                lhs
            }
            Operator::Mod => todo!(),
            Operator::Assign => {
                code.movss(lhs, rhs);
                lhs
            }
            Operator::AddAssign => {
                code.addss(lhs, rhs);
                code.movss(old_lhs, lhs);
                old_lhs
            }
            Operator::SubAssign => {
                code.subss(lhs, rhs);
                code.movss(old_lhs, lhs);
                old_lhs
            }
            Operator::MulAssign => {
                code.mulss(lhs, rhs);
                code.movss(old_lhs, lhs);
                old_lhs
            }
            Operator::DivAssign => {
                code.divss(lhs, rhs);
                code.movss(old_lhs, lhs);
                old_lhs
            }
        }
    }

    fn compile_call(
        code: &mut AsmBuilder,
        scope: &mut Scope,
        data: &mut AsmData,
        func: &FunctionCall,
    ) -> Operand {
        for arg in func.args().iter().rev() {
            let mut arg = Self::compile_expr(code, scope, data, arg);
            let mem_size = arg.mem_size();

            if arg.is_mem() {
                let a_reg = Reg::get_a(mem_size);
                code.mov(a_reg, arg);
                arg = Operand::Reg(a_reg);
            }

            let mem = Mem::reg(Reg::Rsp, mem_size);
            // TODO: allocate space for parameters all at once
            code.sub(Reg::Rsp, Imm::Int64(mem_size as i64));
            code.mov(mem, arg);
        }
        code.call(func.name());
        Operand::Reg(Reg::Eax)
    }

    fn compile_expr(
        code: &mut AsmBuilder,
        scope: &mut Scope,
        data: &mut AsmData,
        expr: &Expression,
    ) -> Operand {
        match expr {
            Expression::Value(value) => Self::compile_value(scope, data, value),
            Expression::BinOp(bin_op) => Self::compile_binop(code, scope, data, bin_op),
            Expression::Cast(..) => todo!(),
            Expression::Func(func) => Self::compile_call(code, scope, data, func),
        }
    }

    fn do_compile_expr(
        code: &mut AsmBuilder,
        scope: &mut Scope,
        data: &mut AsmData,
        expr: &Expression,
    ) -> Operand {
        let operand = Self::compile_expr(code, scope, data, expr);
        scope.reset_temps();
        return operand;
    }

    fn compile_node(code: &mut AsmBuilder, scope: &mut Scope, data: &mut AsmData, node: &AstNode) {
        match node {
            AstNode::Var(_, ident, expr) => {
                let result = Self::do_compile_expr(code, scope, data, expr);
                let local = scope.new_local(ident, result.mem_size());
                if let Operand::Xmm(_) = result {
                    code.movss(local, result);
                } else {
                    code.mov(local, result);
                }
            }
            AstNode::If(expr, nodes) => {
                let endif_lbl = scope.new_label();

                let result = Self::do_compile_expr(code, scope, data, expr);
                code.cmp(result, Imm::FALSE);
                code.je(&endif_lbl);

                let scope = &mut Scope::continue_on(&scope);
                for inner in nodes {
                    Self::compile_node(code, scope, data, inner);
                }

                code.label(&endif_lbl);
            }
            AstNode::While(expr, nodes) => {
                let startwhile_lbl = scope.new_label();
                let endwhile_lbl = scope.new_label();

                code.label(&startwhile_lbl);

                let result = Self::do_compile_expr(code, scope, data, expr);
                code.cmp(result, Imm::FALSE);
                code.je(&endwhile_lbl);

                let scope = &mut Scope::continue_on(&scope);
                for inner in nodes {
                    Self::compile_node(code, scope, data, inner);
                }

                code.jmp(&startwhile_lbl);
                code.label(&endwhile_lbl);
            }
            AstNode::For(ident, limit, nodes) => {
                let startfor_lbl = scope.new_label();
                let endfor_lbl = scope.new_label();

                let scope = &mut Scope::continue_on(&scope);
                let counter = scope.new_local(ident, MemSize::DWord);

                code.mov(counter, Imm::Int32(0));
                code.label(&startfor_lbl);

                match limit {
                    ValueExpr::Int(value) => {
                        code.cmp(counter, Imm::Int32(*value));
                    }
                    ValueExpr::Identifier(ident, ..) => {
                        code.mov(Reg::Eax, *scope.get(ident));
                        code.cmp(counter, Reg::Eax);
                    }
                    _ => unreachable!(),
                };

                code.jg(&endfor_lbl);

                for inner in nodes {
                    Self::compile_node(code, scope, data, inner);
                }

                code.inc(counter);
                code.jmp(&startfor_lbl);
                code.label(&endfor_lbl);
            }
            AstNode::Expr(expr) => {
                let _ = Self::do_compile_expr(code, scope, data, expr);
            }
            AstNode::Ret(expr) => {
                let result = Self::do_compile_expr(code, scope, data, expr);
                if !result.is_reg() {
                    code.mov(Reg::Eax, result);
                }
                code.jmp(".R");
            }
            _ => {}
        }
    }

    fn compile_args(scope: &mut Scope, args: &[Argument]) -> usize {
        let mem_offset = 16; // rip + rbp
        let mut args_size = 0;
        for arg in args {
            let mem_size = Self::get_var_size(arg.arg_type);
            let offset = mem_offset + args_size as isize;
            let addr = Mem::offset(Reg::Rbp, offset, mem_size);
            args_size += mem_size as usize;
            scope.set(&arg.name, addr.into());
        }
        return args_size;
    }

    fn compile_func(code: &mut AsmBuilder, global: &Scope, data: &mut AsmData, ident: &Identifier, args: &Vec<Argument>, nodes: &Vec<AstNode>) {
        code.label(ident);
        code.push_sf();

        let mut inner_code = AsmBuilder::new();
        let mem = MemAlloc::new_ref_cell();
        let mut scope = Scope::new_inner(global, &mem);

        let args_size = Self::compile_args(&mut scope, args);

        for inner in nodes {
            Self::compile_node(&mut inner_code, &mut scope, data, inner);
        }

        let mem = mem.borrow();
        if mem.get_total() > 0 {
            code.sub(Reg::Rsp, Imm::Int64(mem.get_total()));
        }
        code.append(inner_code);

        code.label(".R");
        code.pop_sf();
        code.ret(args_size);
    }

    pub fn compile(ast: Ast, target: CompilerTarget) -> String {
        let mut code = AsmBuilder::new();
        let mem = MemAlloc::new_ref_cell();
        let global = Scope::new(&mem);
        let mut data = AsmData::new();

        code.bits(64);
        code.section("text");
        code.global(&["main"]);

        for node in ast.iter() {
            match node {
                AstNode::Use(ident) => intrisic(&ident, &mut code, target),
                AstNode::Func(ident, args, _, ast_nodes) => {
                    Self::compile_func(&mut code, &global, &mut data, ident, args, ast_nodes);
                }
                _ => {}
            }
        }

        if !data.is_empty() {
            code.section("data");

            for (label, val) in data.iter() {
                code.db(&label.to_string(), val);
            }
        }

        code.to_string()
    }
}
