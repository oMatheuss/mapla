use std::collections::HashMap;

use crate::asm::{AsmBuilder, Imm, Mem, MemSize, Operand, Reg};
use crate::ast::{Argument, Ast, AstNode, Expression, Operator, ValueExpr, VarType};
use crate::intrinsic::intrisic;

#[derive(Debug, Clone)]
struct VarProps {
    operand: Operand,
    var_type: VarType,
}

struct Scope<'a> {
    var: HashMap<String, VarProps>,
    off: isize,
    lbl: usize,
    sup: Option<Box<&'a Scope<'a>>>,
}

impl<'a> Scope<'a> {
    fn new() -> Self {
        Self {
            var: HashMap::new(),
            off: 0,
            lbl: 0,
            sup: None,
        }
    }

    fn new_inner(outer: &'a Self) -> Self {
        Self {
            var: HashMap::new(),
            off: 0,
            lbl: 0,
            sup: Some(Box::new(outer)),
        }
    }

    fn continue_on(outer: &'a Self) -> Self {
        Self {
            var: HashMap::new(),
            off: outer.off,
            lbl: outer.lbl,
            sup: Some(Box::new(outer)),
        }
    }

    fn get(&self, ident: &str) -> &VarProps {
        if let Some(local) = self.var.get(ident) {
            local
        } else if let Some(sup) = &self.sup {
            sup.get(ident)
        } else {
            unreachable!("'{ident}' was not found: variable does not exist")
        }
    }

    fn set(&mut self, ident: &str, props: VarProps) -> Option<VarProps> {
        self.var.insert(String::from(ident), props)
    }

    fn new_label(&mut self) -> String {
        self.lbl += 1;
        format!(".L{lbl}", lbl = self.lbl)
    }
}

pub struct Compiler;

impl Compiler {
    fn compile_expr(code: &mut AsmBuilder, scope: &mut Scope, expr: &Expression) -> Operand {
        match expr {
            Expression::Value(value_expr) => match value_expr {
                ValueExpr::String(_) => todo!(),
                ValueExpr::Int(value) => Operand::Imm(Imm::Int32(*value)),
                ValueExpr::Float(value) => Operand::Imm(Imm::Float32(*value)),
                ValueExpr::Bool(value) => match value {
                    true => Operand::Imm(Imm::TRUE),
                    false => Operand::Imm(Imm::FALSE),
                },
                ValueExpr::Identifier(ident) => {
                    let var = scope.get(ident);
                    var.operand
                }
            },
            Expression::BinOp(operator, sides) => {
                let [lhs, rhs] = &**sides;

                // depth-first search
                let mut lhs = Self::compile_expr(code, scope, lhs);

                // clear register
                if let Operand::Reg(reg) = lhs {
                    scope.off -= reg.mem_size() as isize;
                    lhs = Mem::offset(Reg::Rbp, scope.off, reg.mem_size()).into();
                    code.mov(lhs, reg);
                }

                let mut rhs = Self::compile_expr(code, scope, rhs);

                // clear register
                if let Operand::Reg(reg) = rhs {
                    scope.off -= reg.mem_size() as isize;
                    rhs = Mem::offset(Reg::Rbp, scope.off, reg.mem_size()).into();
                    code.mov(rhs, reg);
                }

                // prepare for operation
                if !lhs.is_reg() && !operator.is_assign() {
                    let mem_size = lhs.mem_size();
                    let a_reg = Reg::get_a(mem_size);
                    code.mov(a_reg, lhs);
                    lhs = Operand::Reg(a_reg);
                }

                assert!(
                    operator.is_assign() == lhs.is_mem(),
                    "cannot assign if lhs is not a memory"
                );

                assert!(
                    lhs.mem_size() == rhs.mem_size(),
                    "cannot operate on different sizes"
                );

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
            Expression::Cast(var_type, expr) => todo!(),
            Expression::Func(ident, args, ret_type) => {
                let _offset = scope.off;
                for arg in args {
                    let mut arg = Self::compile_expr(code, scope, arg);
                    let mem_size = arg.mem_size();
                    if arg.is_mem() {
                        let a_reg = Reg::get_a(mem_size);
                        code.mov(a_reg, arg);
                        arg = Operand::Reg(a_reg);
                    }
                    scope.off -= mem_size as isize;
                    let mem = Mem::offset(Reg::Rbp, scope.off, mem_size);
                    code.mov(mem, arg);
                }
                code.call(&ident);
                scope.off = _offset;
                Operand::Reg(Reg::Eax)
            }
        }
    }

    fn compile_node(code: &mut AsmBuilder, scope: &mut Scope, node: &AstNode) {
        match node {
            AstNode::Var(var_type, ident, expr) => {
                let result = Self::compile_expr(code, scope, expr);
                scope.off -= result.mem_size() as isize;
                let addr = Mem::offset(Reg::Rbp, scope.off, result.mem_size());
                code.mov(addr, result);
                let prop = VarProps {
                    operand: Operand::Mem(addr),
                    var_type: *var_type,
                };
                scope.set(ident, prop);
            }
            AstNode::If(expr, nodes) => {
                let result = Self::compile_expr(code, scope, expr);
                let endif_lbl = scope.new_label();
                code.cmp(result, Imm::FALSE);
                code.je(&endif_lbl);

                let scope = &mut Scope::continue_on(&scope);
                for inner in nodes {
                    Self::compile_node(code, scope, inner);
                }

                code.label(&endif_lbl);
            }
            AstNode::While(expr, nodes) => {
                let startwhile_lbl = scope.new_label();
                let endwhile_lbl = scope.new_label();

                code.label(&startwhile_lbl);
                let result = Self::compile_expr(code, scope, expr);
                code.cmp(result, Imm::FALSE);
                code.je(&endwhile_lbl);

                let scope = &mut Scope::continue_on(&scope);
                for inner in nodes {
                    Self::compile_node(code, scope, inner);
                }

                code.jmp(&startwhile_lbl);
                code.label(&endwhile_lbl);
            }
            AstNode::For(ident, limit, nodes) => {
                let startfor_lbl = scope.new_label();
                let endfor_lbl = scope.new_label();

                scope.off -= 4;
                let ite_addr = Mem::offset(Reg::Rbp, scope.off, MemSize::DWord);
                let ite_prop = VarProps {
                    operand: Operand::Mem(ite_addr),
                    var_type: VarType::Int,
                };

                code.mov(ite_addr, Imm::Int32(0));
                code.label(&startfor_lbl);

                match limit {
                    ValueExpr::Int(value) => {
                        code.cmp(ite_addr, Imm::Int32(*value));
                    }
                    ValueExpr::Identifier(ident) => {
                        let prop = scope.get(ident);
                        code.mov(Reg::Eax, prop.operand);
                        code.cmp(ite_addr, Reg::Eax);
                    }
                    _ => unreachable!(),
                };

                code.jg(&endfor_lbl);

                let scope = &mut Scope::continue_on(&scope);
                scope.set(ident, ite_prop); // define the iterator variable

                for inner in nodes {
                    Self::compile_node(code, scope, inner);
                }

                code.inc(ite_addr);
                code.jmp(&startfor_lbl);
                code.label(&endfor_lbl);
            }
            AstNode::Expr(expr) => {
                let _ = Self::compile_expr(code, scope, expr);
            }
            AstNode::Ret(expr) => {
                let result = Self::compile_expr(code, scope, expr);
                if !result.is_reg() {
                    code.mov(Reg::Eax, result);
                }
                code.jmp(".return");
            }
            _ => {}
        }
    }

    pub fn compile(ast: Ast) -> String {
        let mut code = AsmBuilder::new();
        let global = Scope::new();

        code.bits(64);
        code.section("text");
        code.global(&["_start"]);

        for node in ast.iter() {
            match node {
                AstNode::Use(ident) => intrisic(&ident, &mut code),
                AstNode::Func(ident, args, var_type, ast_nodes) => {
                    code.label(ident);
                    code.push_sf();

                    let mut scope = Scope::new_inner(&global);

                    let mut mem_offset = 16; // rip + rbp
                    for arg in args {
                        let addr = Mem::offset(Reg::Rbp, mem_offset, MemSize::DWord);
                        let props = VarProps {
                            operand: Operand::Mem(addr),
                            var_type: arg.arg_type,
                        };
                        mem_offset += 4; // TODO
                        scope.set(&arg.name, props);
                    }

                    for inner in ast_nodes {
                        Self::compile_node(&mut code, &mut scope, inner);
                    }

                    code.label(".return");
                    code.pop_sf();
                    code.ret(args.len() * 4);
                }
                _ => {}
            }
        }

        code.label("_start");
        code.call("main");
        code.sys_exit(0);

        code.to_string()
    }
}
