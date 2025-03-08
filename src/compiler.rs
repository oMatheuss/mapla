use std::collections::HashMap;

use crate::asm::{AsmBuilder, Imm, Mem, MemSize, Operand, Reg};
use crate::ast::{Argument, Ast, AstNode, Expression, Operator, ValueExpr, VarType};

#[derive(Debug, Clone)]
struct VarProps {
    operand: Operand,
    var_type: VarType,
}

struct Scope<'a> {
    var: HashMap<String, VarProps>,
    off: isize,
    sup: Option<Box<&'a Scope<'a>>>,
}

impl<'a> Scope<'a> {
    fn new() -> Self {
        Self {
            var: HashMap::new(),
            off: 0,
            sup: None,
        }
    }

    fn new_inner(outer: &'a Self) -> Self {
        Self {
            var: HashMap::new(),
            off: 0,
            sup: Some(Box::new(outer)),
        }
    }

    fn continue_on(outer: &'a Self) -> Self {
        Self {
            var: HashMap::new(),
            off: outer.off,
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
}

pub struct Compiler;

impl Compiler {
    fn compile_expr(code: &mut AsmBuilder, scope: &mut Scope, expr: &Expression) -> Operand {
        match expr {
            Expression::Value(value_expr) => match value_expr {
                ValueExpr::String(_) => todo!(),
                ValueExpr::Int(value) => Imm::Int32(*value).into(),
                ValueExpr::Float(value) => Imm::Float32(*value).into(),
                ValueExpr::Identifier(ident) => scope.get(ident).operand,
            },
            Expression::BinOp(operator, sides) => {
                let [lhs, rhs] = &**sides;

                // depth-first search
                let mut lhs = Self::compile_expr(code, scope, lhs);

                // clear register
                if let Operand::Reg(reg) = lhs {
                    scope.off -= 4;
                    lhs = Mem::offset(Reg::Rbp, scope.off, MemSize::DWord).into();
                    code.mov(lhs, reg);
                }

                let mut rhs = Self::compile_expr(code, scope, rhs);

                // clear register
                if let Operand::Reg(reg) = rhs {
                    scope.off -= 4;
                    rhs = Mem::offset(Reg::Rbp, scope.off, MemSize::DWord).into();
                    code.mov(rhs, reg);
                }

                // prepare for operation
                match lhs {
                    Operand::Imm(imm) => {
                        code.mov(Reg::Eax, imm);
                        lhs = Operand::Reg(Reg::Eax);
                    }
                    Operand::Mem(mem) if !operator.is_assign() => {
                        code.mov(Reg::Eax, mem);
                        lhs = Operand::Reg(Reg::Eax)
                    }
                    _ => {}
                }

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
                        code.idiv(rhs); // divisor (rhs)
                        lhs
                    }
                    Operator::Mod => todo!(),
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
                    Operator::DivAssign => todo!(),
                }
            }
            Expression::Cast(var_type, expression) => todo!(),
            Expression::Func(ident, args, ret_type) => {
                let _offset = scope.off;
                for arg in args {
                    let arg = Self::compile_expr(code, scope, arg);
                    scope.off -= 4; // TODO: other types
                    let mem = Mem::offset(Reg::Rbp, scope.off, MemSize::DWord);
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
                scope.off -= 4;
                let addr = Mem::offset(Reg::Rbp, scope.off, MemSize::DWord);
                code.mov(addr, result);
                let prop = VarProps {
                    operand: Operand::Mem(addr),
                    var_type: *var_type,
                };
                scope.set(ident, prop);
            }
            AstNode::If(expression, ast_nodes) => {}
            AstNode::While(expression, ast_nodes) => {
                code.label(".startwhile");
                let result = Self::compile_expr(code, scope, expression);
                code.cmp(result, result); // TODO
                code.je(".endwhile");

                let scope = &mut Scope::continue_on(&scope);
                for inner in ast_nodes {
                    Self::compile_node(code, scope, inner);
                }

                code.jmp(".startwhile");
                code.label(".endwhile");
            }
            AstNode::For(identifier, value_expr, ast_nodes) => {
                scope.off -= 4;
                let ite_addr = Mem::offset(Reg::Rbp, scope.off, MemSize::DWord);
                let ite_prop = VarProps {
                    operand: Operand::Mem(ite_addr),
                    var_type: VarType::Int,
                };

                code.mov(ite_addr, Imm::Int32(0));
                code.label(".startfor");

                match value_expr {
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

                code.jg(".endfor");

                let scope = &mut Scope::continue_on(&scope);
                scope.set(identifier, ite_prop); // define the iterator variable

                for inner in ast_nodes {
                    Self::compile_node(code, scope, inner);
                }

                code.inc(ite_addr);
                code.jmp(".startfor");
                code.label(".endfor");
            }
            AstNode::Expr(expression) => {
                let _ = Self::compile_expr(code, scope, expression);
            }
            AstNode::Ret(expression) => {
                let result = Self::compile_expr(code, scope, expression);
                if !matches!(result, Operand::Reg(Reg::Eax)) {
                    code.mov(Reg::Eax, result);
                }
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
                AstNode::Func(identifier, arguments, var_type, ast_nodes) => {
                    if identifier == "main" {
                        code.label("_start");
                    } else {
                        code.label(identifier);
                        code.push_sf();
                    };

                    let mut scope = Scope::new_inner(&global);

                    let mut mem_offset = 16; // rip + rbp
                    for arg in arguments {
                        let Argument {
                            name,
                            arg_type,
                            default,
                        } = arg;
                        let addr = Mem::offset(Reg::Rbp, mem_offset, MemSize::DWord);
                        let props = VarProps {
                            operand: Operand::Mem(addr),
                            var_type: *arg_type,
                        };
                        mem_offset += 4; // TODO
                        scope.set(name, props);
                    }

                    for inner in ast_nodes {
                        Self::compile_node(&mut code, &mut scope, inner);
                    }

                    if identifier != "main" {
                        code.pop_sf();
                        code.ret(arguments.len() * 4);
                    }
                }
                _ => {}
            }
        }

        code.sys_exit(0);
        code.to_string()
    }
}
