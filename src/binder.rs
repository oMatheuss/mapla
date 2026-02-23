use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::{Ast, AstNode, AstRoot, AstType, Expression, Identifier, Literal};
use crate::error::{Error, Result};
use crate::ir::{IrArg, IrFunc, IrLiteral, IrNode};
use crate::position::Position;
use crate::symbols::{FuncDef, GlobalVar, SymbolTable, SymbolValue, TypeDef};
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

    fn bind_type(&self, ast_type: &AstType, ns: &str) -> Result<Type> {
        let typ = match ast_type {
            AstType::Int => Type::Int,
            AstType::Real => Type::Real,
            AstType::Byte => Type::Byte,
            AstType::Char => Type::Char,
            AstType::Bool => Type::Bool,
            AstType::Void => Type::Void,
            AstType::Pointer(ast_type) => Type::Pointer(self.bind_type(ast_type, ns)?.into()),
            AstType::Array(ast_type, size) => {
                Type::Array(self.bind_type(ast_type, ns)?.into(), *size)
            }
            AstType::Named(items) => {
                let mut items = items.clone();
                let type_name = items.pop().unwrap();
                let ns = items.pop().unwrap_or(ns.into());
                let Some(typ) = self.globals.get(&type_name, &ns) else {
                    let msg = format!("type declaration for '{type_name}' was not found");
                    Error::type_err(msg, Position::default())?
                };
                typ.as_type()
            }
        };
        Ok(typ)
    }

    fn bind_args(&self, ast_args: Vec<(String, AstType)>, ns: &str) -> Result<Vec<Argument>> {
        let mut args = Vec::new();
        for (name, ast_type) in ast_args.into_iter() {
            let arg = Argument {
                name: name,
                arg_type: self.bind_type(&ast_type, ns)?,
            };
            args.push(arg);
        }
        Ok(args)
    }

    pub fn bind_globals(&mut self, ast: &Ast) -> Result<()> {
        for node in ast.nodes.iter() {
            let ns = ast.namespace.clone();
            match node {
                AstRoot::Global(var_type, id, value) => {
                    let symbol = GlobalVar {
                        pos: id.position.clone(),
                        typ: self.bind_type(var_type, &ns)?,
                    };
                    self.globals.set_var(&id.name, ns, symbol)?;

                    if let Some(value) = value {
                        let name = id.name.clone();
                        match value {
                            Literal::String(s) => self.data.insert(name, s.clone().into_bytes()),
                            Literal::Byte(b) => self.data.insert(name, [*b as u8].to_vec()),
                            Literal::Int(i) => self.data.insert(name, i.to_le_bytes().to_vec()),
                            Literal::Float(f) => self.data.insert(name, f.to_le_bytes().to_vec()),
                            Literal::Bool(b) => self.data.insert(name, [*b as u8].to_vec()),
                        };
                    }
                }
                AstRoot::Struct(id, args) => {
                    let symbol = TypeDef {
                        pos: id.position.clone(),
                        fields: self.bind_args(args.clone(), &ns)?,
                    };
                    self.globals.set_type(&id.name, ns, symbol)?;
                }
                AstRoot::Func(typ, id, args, ..) => {
                    let symbol = FuncDef {
                        pos: id.position.clone(),
                        args: self.bind_args(args.items.clone(), &ns)?,
                        ret: self.bind_type(typ, &ns)?,
                        extrn: false,
                        variadic: args.variadic,
                    };
                    self.globals.set_func(&id.name, ns, symbol)?;
                }
                AstRoot::ExternFunc(typ, id, args, ..) => {
                    let symbol = FuncDef {
                        pos: id.position.clone(),
                        args: self.bind_args(args.items.clone(), &ns)?,
                        ret: self.bind_type(typ, &ns)?,
                        extrn: true,
                        variadic: args.variadic,
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
                    let args = self.bind_args(args.items, &ast.namespace)?;
                    let body =
                        FuncBinder::new(self, ast.namespace.clone()).bind_func(&args, ast_nodes)?;
                    self.functions.push(IrFunc {
                        name: id.name,
                        namespace: ast.namespace.clone(),
                        args,
                        typ: self.bind_type(&typ, &ast.namespace)?,
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
    namespace: Rc<String>,
}

impl<'a> FuncBinder<'a> {
    fn new(binder: &'a mut Binder, namespace: Rc<String>) -> Self {
        Self {
            binder,
            nodes: Vec::new(),
            scope: FuncScope::new(),
            labels: 0,
            namespace,
        }
    }

    fn new_label(&mut self) -> usize {
        self.labels += 1;
        self.labels
    }

    fn emit(&mut self, node: IrNode, do_emit: bool) {
        if do_emit {
            self.nodes.push(node);
        }
    }

    fn bind_value(&mut self, value: Literal) -> Result<IrArg> {
        let value = match value {
            Literal::String(s) => {
                let label = format!("L.str.{}", self.binder.data.len());
                let size = s.len() as u32 - 1;
                self.binder.data.insert(label.clone(), s.into_bytes());
                IrLiteral::String { label, size }.into()
            }
            Literal::Byte(b) => IrLiteral::Byte(b).into(),
            Literal::Int(i) => IrLiteral::Int(i).into(),
            Literal::Float(f) => IrLiteral::Float(f).into(),
            Literal::Bool(b) => IrLiteral::Bool(b).into(),
        };
        Ok(value)
    }

    fn bind_global(&self, id: Identifier, ns: Rc<String>) -> Result<IrArg> {
        match self.binder.globals.get(&id.name, &ns) {
            Some(val) => match val {
                SymbolValue::FuncDef(func) if func.extrn => Ok(IrArg::Extern {
                    name: id.name,
                    typ: func.as_type(),
                }),
                others => Ok(IrArg::Global {
                    ns,
                    name: id.name,
                    typ: others.as_type(),
                }),
            },
            None => Error::syntatic("symbol not found in scope", id.position)?,
        }
    }

    fn bind_expr(&mut self, expr: Expression, emit: bool) -> Result<Type> {
        match expr {
            Expression::Identifier { id } => {
                let value = match self.scope.get(&id.name) {
                    Some(var) => var,
                    // assume user is trying to access a variable in this namespace
                    None => self.bind_global(id, self.namespace.clone())?,
                };
                let res = value.get_type();
                self.emit(IrNode::Load { value }, emit);
                Ok(res)
            }
            Expression::Literal { lit: value } => {
                let value = self.bind_value(value)?;
                let res = value.get_type();
                self.emit(IrNode::Load { value }, emit);
                Ok(res)
            }
            Expression::UnaOp { ope, val } => {
                let typ = self.bind_expr(*val, emit)?;
                let ope_type = typ.clone();
                let res = TypeCheck::check_unaexpr(ope, ope_type, Position::default())?;
                self.emit(IrNode::UnaOp { ope, typ }, emit);
                Ok(res)
            }
            Expression::BinOp { ope, lhs, rhs } => {
                let rhs = self.bind_expr(*rhs, emit)?;
                let typ = rhs.clone();
                let lhs = self.bind_expr(*lhs, emit)?;
                let res = TypeCheck::check_binexpr(ope, lhs, rhs, Position::default())?;
                self.emit(IrNode::BinOp { ope, typ }, emit);
                Ok(res)
            }
            Expression::Call { func, args } => {
                let mut args_types = Vec::new();
                for arg in args {
                    let arg_type = self.bind_expr(arg, emit)?;
                    self.emit(IrNode::Arg, emit);
                    args_types.push(arg_type);
                }
                match self.bind_expr(*func, emit)? {
                    Type::Func(func_args, ret, variadic) => {
                        TypeCheck::check_callargs(&func_args, &args_types, variadic)?;
                        let args = args_types;
                        let func_ret = *ret.clone();
                        self.emit(IrNode::Call { args, ret: *ret }, emit);
                        Ok(func_ret)
                    }
                    Type::Struct(fields) => {
                        TypeCheck::check_callargs(&fields, &args_types, false)?;
                        let typ = Type::Struct(fields.clone());
                        let fields = fields.into_iter().map(|f| f.arg_type).collect();
                        self.emit(IrNode::Struct { fields }, emit);
                        Ok(typ)
                    }
                    typ => {
                        let msg = format!("symbol has type {typ} which is not a func or struct");
                        Error::syntatic(msg, Position::default())?
                    }
                }
            }
            Expression::Index { array, index } => {
                let array = self.bind_expr(*array, emit)?;
                let index = self.bind_expr(*index, emit)?;
                let typ = TypeCheck::check_index(array, index, Position::default())?;
                self.emit(IrNode::Index { typ: typ.clone() }, emit);
                Ok(typ)
            }
            Expression::Cast { val, typ } => {
                let from = self.bind_expr(*val, emit)?;
                let to = self.binder.bind_type(&typ, &self.namespace)?;
                let res = to.clone();
                TypeCheck::check_cast(&from, &to, Position::default())?;
                self.emit(IrNode::Cast { from, to }, emit);
                Ok(res)
            }
            Expression::Field { expr, field } => {
                let typ = self.bind_expr(*expr, emit)?;
                let (mut fields, by_ref) = if let Type::Pointer(inner) = &typ
                    && let Type::Struct(ref fields) = **inner
                {
                    (fields.clone(), true)
                } else if let Type::Struct(fields) = &typ {
                    (fields.clone(), false)
                } else {
                    let msg = format!("cannot do a member access into the type {typ}");
                    Error::syntatic(msg, Position::default())?
                };
                fields.reverse();
                let mut offset = Vec::new();
                let typ = loop {
                    let Some(item) = fields.pop() else {
                        let msg = format!("field {} does not exists in type {typ}", field.name);
                        Error::syntatic(msg, Position::default())?
                    };
                    offset.push(item.arg_type.clone());
                    if item.name == field.name {
                        break item.arg_type;
                    }
                };
                self.emit(IrNode::Field { offset, by_ref }, emit);
                Ok(typ)
            }
            Expression::Member { ns, member } => {
                let value = self.bind_global(member, ns.name.into())?;
                let typ = value.get_type();
                self.emit(IrNode::Load { value }, emit);
                return Ok(typ);
            }
            Expression::SizeOf { val } => {
                let typ = self.bind_expr(*val, false)?;
                self.emit(IrNode::SizeOf { typ }, emit);
                return Ok(Type::Int);
            }
        }
    }

    fn bind_node(&mut self, node: AstNode) -> Result<()> {
        match node {
            AstNode::TypedVar(typ, name, Some(expr)) => {
                let exp_typ = self.bind_expr(expr, true)?;
                let var_typ = self.binder.bind_type(&typ, &self.namespace)?;
                let typ = TypeCheck::check_assign(var_typ, exp_typ, Position::default())?;
                let index = self.scope.set(name, typ.clone());
                self.nodes.push(IrNode::Store { index, typ });
            }
            AstNode::TypedVar(typ, name, None) => {
                let typ = self.binder.bind_type(&typ, &self.namespace)?;
                let index = self.scope.set(name, typ.clone());
                self.nodes.push(IrNode::Alloc { index, typ });
            }
            AstNode::Var(name, expr) => {
                let typ = self.bind_expr(expr, true)?;
                let index = self.scope.set(name, typ.clone());
                self.nodes.push(IrNode::Store { index, typ });
            }

            AstNode::If(expr, nodes) => {
                self.scope.enter();
                let label = self.new_label();
                self.bind_expr(expr, true)?;
                self.nodes.push(IrNode::JmpFalse { label });
                for node in nodes {
                    self.bind_node(node)?;
                }
                self.nodes.push(IrNode::Label { label });
                self.scope.exit();
            }
            AstNode::While(expr, nodes) => {
                self.scope.enter();
                let start = self.new_label();
                let end = self.new_label();
                self.nodes.push(IrNode::Label { label: start });
                self.bind_expr(expr, true)?;
                self.nodes.push(IrNode::JmpFalse { label: end });
                for node in nodes {
                    self.bind_node(node)?;
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
                    Some(expr_start) => self.bind_value(expr_start)?,
                    None => IrLiteral::Int(0).into(),
                };
                self.nodes.push(IrNode::Load { value: expr_start });
                let index = self.scope.set(name, typ.clone());
                let counter = IrArg::var(index, typ.clone());
                self.nodes.push(IrNode::Store { index, typ });
                self.nodes.push(IrNode::Label { label: start });
                let expr_end = self.bind_expr(expr_end, true)?;
                if !expr_end.is_int() {
                    Error::type_err("expected int expression", Position::default())?;
                }
                self.nodes.push(IrNode::Load {
                    value: counter.clone(),
                });
                self.nodes.push(IrNode::JmpEq { label: end });
                for node in nodes {
                    self.bind_node(node)?;
                }
                self.nodes.push(IrNode::Load { value: counter });
                self.nodes.push(IrNode::Inc);
                self.nodes.push(IrNode::Jmp { label: start });
                self.nodes.push(IrNode::Label { label: end });
                self.scope.exit();
            }
            AstNode::Expr(expr) => {
                self.bind_expr(expr, true)?;
                self.nodes.push(IrNode::Pop);
            }
            AstNode::Ret(None) => {
                let typ = Type::Void;
                self.nodes.push(IrNode::Return { typ });
            }
            AstNode::Ret(Some(expr)) => {
                let typ = self.bind_expr(expr, true)?;
                self.nodes.push(IrNode::Return { typ });
            }
        };
        Ok(())
    }

    fn bind_func(mut self, args: &Vec<Argument>, nodes: Vec<AstNode>) -> Result<Vec<IrNode>> {
        self.scope.enter();
        for Argument { name, arg_type } in args.iter() {
            self.scope.set(name.clone(), arg_type.clone());
        }
        for node in nodes {
            self.bind_node(node)?;
        }
        Ok(self.nodes)
    }
}
