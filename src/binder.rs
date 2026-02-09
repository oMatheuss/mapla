use std::collections::HashMap;

use crate::ast::{Ast, AstNode, AstRoot, Expression, ValueExpr};
use crate::error::Result;
use crate::ir::{IrArg, IrFunc, IrLiteral, IrNode};
use crate::position::Position;
use crate::symbols::{FuncDef, GlobalVar, SymbolTable, TypeDef};
use crate::types::{Argument, Type, TypeCheck};

pub struct Binder {
    pub globals: SymbolTable,
    pub functions: Vec<IrFunc>,
    pub data: HashMap<String, Vec<u8>>,
}

impl Binder {
    pub fn new() -> Self {
        Self {
            globals: SymbolTable::new(),
            functions: Vec::new(),
            data: HashMap::new(),
        }
    }

    pub fn bind_globals(&mut self, ast: &Ast) -> Result<()> {
        for node in ast.nodes.iter() {
            let ns = ast.namespace.clone();
            match node {
                AstRoot::Global(var_type, id, value) => {
                    let symbol = GlobalVar {
                        pos: id.position.clone(),
                        typ: var_type.clone(),
                    };
                    self.globals.set_var(&id.name, ns, symbol)?;

                    if let Some(value) = value {
                        let name = id.name.clone();
                        match value {
                            ValueExpr::Id(_) => todo!(),
                            ValueExpr::String(s) => self.data.insert(name, s.clone().into_bytes()),
                            ValueExpr::Byte(b) => self.data.insert(name, [*b as u8].to_vec()),
                            ValueExpr::Int(i) => self.data.insert(name, i.to_le_bytes().to_vec()),
                            ValueExpr::Float(f) => self.data.insert(name, f.to_le_bytes().to_vec()),
                            ValueExpr::Bool(b) => self.data.insert(name, [*b as u8].to_vec()),
                        };
                    }
                }
                AstRoot::Struct(id, args) => {
                    let symbol = TypeDef {
                        pos: id.position.clone(),
                        fields: args.clone(),
                    };
                    self.globals.set_type(&id.name, ns, symbol)?;
                }
                AstRoot::Func(typ, id, args, ..) => {
                    let symbol = FuncDef {
                        pos: id.position.clone(),
                        args: args.clone(),
                        ret: typ.clone(),
                        extrn: false,
                    };
                    self.globals.set_func(&id.name, ns, symbol)?;
                }
                AstRoot::ExternFunc(typ, id, args, ..) => {
                    let symbol = FuncDef {
                        pos: id.position.clone(),
                        args: args.clone(),
                        ret: typ.clone(),
                        extrn: true,
                    };
                    self.globals.set_func(&id.name, ns, symbol)?;
                }
            }
        }
        Ok(())
    }

    pub fn bind_ast(&mut self, ast: Ast) -> Result<()> {
        for root in ast.nodes {
            match root {
                AstRoot::Func(typ, id, args, ast_nodes) => {
                    let body = FuncBinder::new(self).bind_func(&args, ast_nodes);
                    self.functions.push(IrFunc {
                        name: id.name,
                        namespace: ast.namespace.clone(),
                        args,
                        typ,
                        body,
                    });
                }
                _ => {}
            }
        }

        Ok(())
    }
}

#[derive(Debug, Default)]
pub struct VarStack {
    count: usize,
    saved: Vec<usize>,
    max: usize,
}

impl VarStack {
    pub fn save(&mut self) {
        self.saved.push(self.count);
    }

    pub fn restore(&mut self) {
        self.count = self.saved.pop().unwrap();
    }

    pub fn push(&mut self) -> usize {
        self.count += 1;
        if self.count > self.max {
            self.max = self.count;
        }
        self.count - 1
    }
}

#[derive(Debug)]
pub struct FuncScope {
    symbols: Vec<HashMap<String, (usize, Type)>>,
    stack: VarStack,
}

impl FuncScope {
    pub fn new() -> Self {
        Self {
            symbols: Vec::new(),
            stack: VarStack::default(),
        }
    }

    pub fn enter(&mut self) {
        self.stack.save();
        self.symbols.push(HashMap::new());
    }

    pub fn exit(&mut self) {
        self.stack.restore();
        self.symbols.pop();
    }

    pub fn get(&self, name: &str) -> Option<IrArg> {
        let (index, typ) = self
            .symbols
            .iter()
            .rev()
            .find_map(|scope| scope.get(name))?
            .clone();
        Some(IrArg::Var { index, typ })
    }

    pub fn set(&mut self, name: String, typ: Type) -> usize {
        let index = self.stack.push();
        self.symbols
            .last_mut()
            .expect("at least one entry")
            .insert(name, (index, typ));
        index
    }
}

pub struct FuncBinder<'a> {
    binder: &'a mut Binder,
    nodes: Vec<IrNode>,
    scope: FuncScope,
    labels: usize,
}

impl<'a> FuncBinder<'a> {
    fn new(binder: &'a mut Binder) -> Self {
        Self {
            binder,
            nodes: Vec::new(),
            scope: FuncScope::new(),
            labels: 0,
        }
    }

    fn new_label(&mut self) -> usize {
        self.labels += 1;
        self.labels
    }

    fn bind_value(&mut self, value: ValueExpr) -> IrArg {
        match value {
            ValueExpr::Id(name) => match self.scope.get(&name) {
                Some(var) => var,
                None => match self.binder.globals.get_var(&name, "global").cloned() {
                    Some(sym) => IrArg::Global { name, typ: sym.typ },
                    None => todo!("Symbol not found on scope"),
                },
            },
            ValueExpr::String(s) => {
                let label = format!("L.str.{}", self.binder.data.len());
                self.binder.data.insert(label.clone(), s.into_bytes());
                IrLiteral::String { label }.into()
            }
            ValueExpr::Byte(b) => IrLiteral::Byte(b).into(),
            ValueExpr::Int(i) => IrLiteral::Int(i).into(),
            ValueExpr::Float(f) => IrLiteral::Float(f).into(),
            ValueExpr::Bool(b) => IrLiteral::Bool(b).into(),
        }
    }

    fn bind_expr(&mut self, expr: Expression) -> Type {
        match expr {
            Expression::Value { value } => {
                let value = self.bind_value(value);
                let res = value.get_type();
                self.nodes.push(IrNode::Load { value });
                res
            }
            Expression::UnaOp { operator, operand } => {
                let operand = self.bind_expr(*operand);
                let typ = TypeCheck::check_unaexpr(operator, operand, Position::default()).unwrap();
                let res = typ.clone();
                self.nodes.push(IrNode::UnaOp { ope: operator, typ });
                res
            }
            Expression::BinOp { operator, lhs, rhs } => {
                let rhs = self.bind_expr(*rhs);
                let ope_type = rhs.clone();
                let lhs = self.bind_expr(*lhs);
                let typ =
                    TypeCheck::check_binexpr(operator, lhs, rhs, Position::default()).unwrap();
                let res = typ.clone();
                self.nodes.push(IrNode::BinOp {
                    ope: operator,
                    typ: ope_type,
                });
                res
            }
            Expression::Call { name, args } => {
                let func = self
                    .binder
                    .globals
                    .get_func(&name, "global")
                    .cloned()
                    .unwrap();
                for arg in args {
                    self.bind_expr(arg);
                }
                let args = func.args;
                let ret = func.ret.clone();
                self.nodes.push(IrNode::Call { name, args, ret });
                func.ret
            }
            Expression::Index { array, index } => {
                let array = self.bind_expr(*array);
                let index = self.bind_expr(*index);
                let typ = TypeCheck::check_index(array, index, Position::default()).unwrap();
                self.nodes.push(IrNode::Index { typ: typ.clone() });
                typ
            }
            Expression::Cast { value, as_type } => {
                let from = self.bind_expr(*value);
                let to = as_type.clone();
                TypeCheck::check_cast(&from, &to, Position::default()).unwrap();
                self.nodes.push(IrNode::Cast { from, to });
                as_type
            }
        }
    }

    fn bind_node(&mut self, node: AstNode) {
        match node {
            AstNode::TypedVar(typ, name, Some(expr)) => {
                self.bind_expr(expr);
                let index = self.scope.set(name, typ.clone());
                self.nodes.push(IrNode::Store { index, typ });
            }
            AstNode::TypedVar(typ, name, None) => {
                let index = self.scope.set(name, typ.clone());
                self.nodes.push(IrNode::Alloc { index, typ });
            }
            AstNode::Var(name, expr) => {
                let typ = self.bind_expr(expr);
                let index = self.scope.set(name, typ.clone());
                self.nodes.push(IrNode::Store { index, typ });
            }

            AstNode::If(expr, nodes) => {
                self.scope.enter();
                let label = self.new_label();
                self.bind_expr(expr);
                self.nodes.push(IrNode::JmpFalse { label });
                for node in nodes {
                    self.bind_node(node);
                }
                self.nodes.push(IrNode::Label { label });
                self.scope.exit();
            }
            AstNode::While(expr, nodes) => {
                self.scope.enter();
                let start = self.new_label();
                let end = self.new_label();
                self.nodes.push(IrNode::Label { label: start });
                self.bind_expr(expr);
                self.nodes.push(IrNode::JmpFalse { label: end });
                for node in nodes {
                    self.bind_node(node);
                }
                self.nodes.push(IrNode::Jmp { label: start });
                self.nodes.push(IrNode::Label { label: end });
                self.scope.exit();
            }
            AstNode::For(name, expr_start, expr_end, nodes) => {
                self.scope.enter();
                let start = self.new_label();
                let end = self.new_label();
                let typ = Type::Int;
                let expr_start = match expr_start {
                    Some(expr_start) => self.bind_value(expr_start),
                    None => IrLiteral::Int(0).into(),
                };
                self.nodes.push(IrNode::Load { value: expr_start });
                let index = self.scope.set(name, typ.clone());
                let counter = IrArg::var(index, typ.clone());
                self.nodes.push(IrNode::Store { index, typ });
                self.nodes.push(IrNode::Label { label: start });
                let expr_end = self.bind_value(expr_end);
                self.nodes.push(IrNode::Load {
                    value: counter.clone(),
                });
                self.nodes.push(IrNode::Load { value: expr_end });
                self.nodes.push(IrNode::JmpEq { label: end });
                for node in nodes {
                    self.bind_node(node);
                }
                self.nodes.push(IrNode::Load { value: counter });
                self.nodes.push(IrNode::Inc);
                self.nodes.push(IrNode::Jmp { label: start });
                self.nodes.push(IrNode::Label { label: end });
                self.scope.exit();
            }
            AstNode::Expr(expr) => {
                self.bind_expr(expr);
                self.nodes.push(IrNode::Pop);
            }
            AstNode::Ret(expr) => {
                let typ = self.bind_expr(expr);
                self.nodes.push(IrNode::Return { typ });
            }
        };
    }

    fn bind_func(mut self, args: &Vec<Argument>, nodes: Vec<AstNode>) -> Vec<IrNode> {
        self.scope.enter();
        for Argument { name, arg_type } in args.iter() {
            self.scope.set(name.clone(), arg_type.clone());
        }
        for node in nodes {
            self.bind_node(node);
        }
        self.nodes
    }
}
