use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::{
    Ast, AstLambda, AstNode, AstRoot, AstSymbol, AstType, Expression, Identifier, Literal,
};
use crate::error::{Error, Result};
use crate::ir::{IrArg, IrFunc, IrLiteral, IrNode};
use crate::position::Position;
use crate::symbols::{FuncDef, GlobalVar, Symbol, SymbolTable, SymbolValue, TypeDef};
use crate::types::{Field, FuncType, StructType, Type, TypeCheck, TypeWithPos};

#[derive(Debug, Default)]
pub struct Binder {
    pub globals: SymbolTable,
    pub functions: Vec<IrFunc>,
    pub data: HashMap<String, BindData>,

    lambdas: HashMap<String, AstLambda>,
    lambda_count: usize,
}

#[derive(Debug)]
pub enum BindData {
    Bytes(Vec<u8>),
    Labels(Vec<String>),
}

impl Binder {
    pub fn new() -> Self {
        Self::default()
    }

    fn bind_type(&self, ast_type: &AstType, ns: &str) -> Result<Type> {
        let typ = match ast_type {
            AstType::Int => Type::Int,
            AstType::Real => Type::Real,
            AstType::Byte => Type::Byte,
            AstType::Char => Type::Char,
            AstType::Bool => Type::Bool,
            AstType::Void => Type::Void,
            AstType::Func(func) => Type::FuncPtr(FuncType {
                args: func
                    .args
                    .iter()
                    .map(|typ| self.bind_type(typ, ns))
                    .collect::<Result<Vec<_>>>()?,
                variadic: func.variadic,
                ret: self.bind_type(&func.ret, ns)?.into(),
            }),
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

    fn bind_args(&self, ast_args: Vec<AstSymbol>, ns: &str) -> Result<Vec<Symbol>> {
        let mut args = Vec::new();
        for AstSymbol { name, typ, pos } in ast_args.into_iter() {
            let typ = self.bind_type(&typ, ns)?;
            let arg = Symbol { name, typ, pos };
            args.push(arg);
        }
        Ok(args)
    }

    fn bind_array(&mut self, name: String, arr: &Vec<Literal>) -> Result<Type> {
        assert!(!arr.is_empty(), "literal array is empty");
        let size = arr.len() as u32;
        let first = core::mem::discriminant(&arr[0]);
        let typ = match &arr[0] {
            Literal::String(_) => Type::ptr_to(Type::Char),
            Literal::Char(_) => Type::Char,
            Literal::Int(_) => Type::Int,
            Literal::Float(_) => Type::Real,
            Literal::Bool(_) => Type::Bool,
            Literal::Array(..) => {
                let pos = Position::default();
                Error::type_err("array of arrays is not supported", pos)?
            }
        };

        if !arr.iter().all(|lit| core::mem::discriminant(lit) == first) {
            let pos = Position::default();
            return Error::type_err("mixed type array is not supported", pos);
        };

        if let Literal::String(_) = &arr[0] {
            let mut labels = Vec::new();
            for lit in arr {
                match lit {
                    Literal::String(s) => {
                        let label = format!("L.str.{}", self.data.len());
                        let data = BindData::Bytes(s.clone().into_bytes());
                        self.data.insert(label.clone(), data);
                        labels.push(label);
                    }
                    _ => unreachable!(),
                };
            }
            self.data.insert(name, BindData::Labels(labels));
        } else {
            let mut bytes = Vec::new();
            for lit in arr {
                let mut data = match lit {
                    Literal::String(_) => unreachable!(),
                    Literal::Char(b) => [*b as u8].to_vec(),
                    Literal::Int(i) => i.to_le_bytes().to_vec(),
                    Literal::Float(f) => f.to_le_bytes().to_vec(),
                    Literal::Bool(b) => [*b as u8].to_vec(),
                    Literal::Array(..) => unimplemented!(),
                };

                bytes.append(&mut data);
            }
            self.data.insert(name, BindData::Bytes(bytes));
        }

        return Ok(Type::Array(typ.into(), size));
    }

    pub fn bind_globals(&mut self, ast: &Ast) -> Result<()> {
        for node in ast.nodes.iter() {
            let ns = ast.namespace.clone();
            match node {
                AstRoot::Global(var_type, id, value) => {
                    let symbol = GlobalVar {
                        pos: id.position.clone(),
                        typ: self.bind_type(var_type, &ns)?,
                        extrn: false,
                        uninit: value.is_none(),
                    };
                    self.globals.set_var(&id.name, ns.clone(), symbol)?;

                    if let Some(value) = value {
                        let name = format!("{ns}@{}", id.name);
                        match value {
                            Literal::String(s) => {
                                let data = BindData::Bytes(s.clone().into_bytes());
                                self.data.insert(name, data);
                            }
                            Literal::Char(b) => {
                                self.data.insert(name, BindData::Bytes([*b as u8].to_vec()));
                            }
                            Literal::Int(i) => {
                                self.data
                                    .insert(name, BindData::Bytes(i.to_le_bytes().to_vec()));
                            }
                            Literal::Float(f) => {
                                self.data
                                    .insert(name, BindData::Bytes(f.to_le_bytes().to_vec()));
                            }
                            Literal::Bool(b) => {
                                self.data.insert(name, BindData::Bytes([*b as u8].to_vec()));
                            }
                            Literal::Array(arr) => {
                                self.bind_array(name, arr)?;
                            }
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
                AstRoot::Func(typ, id, args, variadic, ..) => {
                    let symbol = FuncDef {
                        pos: id.position.clone(),
                        args: self.bind_args(args.clone(), &ns)?,
                        ret: self.bind_type(typ, &ns)?,
                        extrn: false,
                        variadic: *variadic,
                    };
                    self.globals.set_func(&id.name, ns, symbol)?;
                }
                AstRoot::ExternFunc(typ, id, args, variadic) => {
                    let symbol = FuncDef {
                        pos: id.position.clone(),
                        args: self.bind_args(args.clone(), &ns)?,
                        ret: self.bind_type(typ, &ns)?,
                        extrn: true,
                        variadic: *variadic,
                    };
                    self.globals.set_func(&id.name, ns, symbol)?;
                }
                AstRoot::ExternVar(var_type, id) => {
                    let symbol = GlobalVar {
                        pos: id.position.clone(),
                        typ: self.bind_type(var_type, &ns)?,
                        extrn: true,
                        uninit: false,
                    };
                    self.globals.set_var(&id.name, ns, symbol)?;
                }
            }
        }
        Ok(())
    }

    pub fn bind_ast(&mut self, ast: Ast) -> Result<()> {
        for root in ast.nodes {
            match root {
                AstRoot::Func(typ, id, args, _, ast_nodes) => {
                    // TODO: handle variadics
                    let args = self.bind_args(args, &ast.namespace)?;
                    let ns = ast.namespace.clone();
                    let body = FuncBinder::new(self, ns).bind(&args, ast_nodes)?;
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

        // drain and generate all lambdas
        for (name, lambda) in self.lambdas.drain().collect::<Vec<_>>() {
            let args = self.bind_args(lambda.args, &ast.namespace)?;
            let ns = ast.namespace.clone();
            let body = FuncBinder::new(self, ns).bind(&args, lambda.body)?;
            self.functions.push(IrFunc {
                name,
                namespace: ast.namespace.clone(),
                args,
                typ: self.bind_type(&lambda.typ, &ast.namespace)?,
                body,
            });
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
                let typ = Type::Array(Box::new(Type::Char), size);
                let data = BindData::Bytes(s.into_bytes());
                self.binder.data.insert(label.clone(), data);
                IrLiteral::Array { label, typ }.into()
            }
            Literal::Array(arr) => {
                let label = format!("L.arr.{}", self.binder.data.len());
                let typ = self.binder.bind_array(label.clone(), &arr)?;
                IrLiteral::Array { label, typ }.into()
            }
            Literal::Char(b) => IrLiteral::Char(b).into(),
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
                SymbolValue::GlobalVar(var) if var.extrn => Ok(IrArg::Extern {
                    name: id.name,
                    typ: var.typ,
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
        let pos = expr.pos();
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
            Expression::Literal { lit, pos: _ } => {
                let value = self.bind_value(lit)?;
                let res = value.get_type();
                self.emit(IrNode::Load { value }, emit);
                Ok(res)
            }
            Expression::UnaOp { ope, val } => {
                let typ = self.bind_expr(*val, emit)?;
                let ope_type = typ.clone();
                let res = TypeCheck::check_unaexpr(ope, ope_type, pos)?;
                self.emit(IrNode::UnaOp { ope, typ }, emit);
                Ok(res)
            }
            Expression::BinOp { ope, pos, lhs, rhs } => {
                let rhs = self.bind_expr(*rhs, emit)?;
                let typ = rhs.clone();
                let lhs = self.bind_expr(*lhs, emit)?;
                let res = TypeCheck::check_binexpr(ope, lhs, rhs, pos)?;
                self.emit(IrNode::BinOp { ope, typ }, emit);
                Ok(res)
            }
            Expression::Call { func, args } => {
                let mut args_types = Vec::new();
                for arg in args {
                    let pos = arg.pos();
                    let arg_type = self.bind_expr(arg, emit)?;
                    self.emit(IrNode::Arg, emit);
                    args_types.push(TypeWithPos::new(arg_type, pos));
                }
                let pos = func.pos();
                let typ = self.bind_expr(*func, emit)?;
                if let Type::Func(func) | Type::FuncPtr(func) = typ {
                    TypeCheck::check_callargs(&func, &args_types, pos)?;
                    let args = args_types.into_iter().map(|a| a.typ).collect();
                    let ret = *func.ret.clone();
                    self.emit(IrNode::Call { args, ret }, emit);
                    Ok(*func.ret)
                } else {
                    let msg = format!("symbol has type {typ} which is not a func");
                    Error::syntatic(msg, pos)?
                }
            }
            Expression::Index { array, index } => {
                let array = self.bind_expr(*array, emit)?;
                let index = self.bind_expr(*index, emit)?;
                let typ = TypeCheck::check_index(array, index, pos)?;
                self.emit(IrNode::Index { typ: typ.clone() }, emit);
                Ok(typ)
            }
            Expression::Cast { val, typ } => {
                let from = self.bind_expr(*val, emit)?;
                let to = self.binder.bind_type(&typ, &self.namespace)?;
                let res = to.clone();
                TypeCheck::check_cast(&from, &to, pos)?;
                self.emit(IrNode::Cast { from, to }, emit);
                Ok(res)
            }
            Expression::Field { expr, field } => {
                let pos = expr.pos();
                let typ = self.bind_expr(*expr, emit)?;
                let name = field.name.clone();
                let res = TypeCheck::check_field(&typ, field, pos)?;
                self.emit(IrNode::Field { typ, name }, emit);
                Ok(res)
            }
            Expression::Member { ns, member } => {
                let value = self.bind_global(member, ns.name.into())?;
                let typ = value.get_type();
                self.emit(IrNode::Load { value }, emit);
                return Ok(typ);
            }
            Expression::SizeOf { val, pos: _ } => {
                let typ = self.bind_expr(*val, false)?;
                self.emit(IrNode::SizeOf { typ }, emit);
                return Ok(Type::Int);
            }
            Expression::Lambda { lambda } => {
                let ret = Type::Func(FuncType {
                    args: lambda
                        .args
                        .iter()
                        .map(|ast_sym| self.binder.bind_type(&ast_sym.typ, &self.namespace))
                        .collect::<Result<Vec<_>>>()?,
                    variadic: false,
                    ret: self.binder.bind_type(&lambda.typ, &self.namespace)?.into(),
                });
                let name = format!("lambda.{}", self.binder.lambda_count);
                self.binder.lambdas.insert(name.clone(), lambda);
                self.binder.lambda_count += 1;
                let value = IrArg::Global {
                    ns: self.namespace.clone(),
                    name,
                    typ: ret.clone(),
                };
                self.emit(IrNode::Load { value }, emit);
                return Ok(ret);
            }
            Expression::Contructor { fields, pos: _ } => {
                let mut struct_fields = Vec::new();
                for field in fields {
                    let name = field.id.name;
                    let typ = self.bind_expr(*field.value, emit)?;
                    self.emit(IrNode::Arg, emit);
                    struct_fields.push(Field { name, typ });
                }
                let fields = struct_fields;
                let typ = Type::Struct(StructType { fields });
                let res = typ.clone();
                self.emit(IrNode::Struct { typ }, emit);
                Ok(res)
            }
        }
    }

    fn bind_node(&mut self, node: AstNode, jmp_to: Option<usize>) -> Result<()> {
        match node {
            AstNode::TypedVar(typ, name, Some(expr)) => {
                let pos = expr.pos();
                let exp_typ = self.bind_expr(expr, true)?;
                let var_typ = self.binder.bind_type(&typ, &self.namespace)?;
                let typ = TypeCheck::check_assign(var_typ, exp_typ, pos)?;
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

            AstNode::If(expr, nodes, else_branch) => {
                self.scope.enter();
                let end = jmp_to.or_else(|| Some(self.new_label()));
                let label = match else_branch {
                    Some(..) => self.new_label(),
                    None => end.unwrap(),
                };
                self.bind_expr(expr, true)?;
                self.nodes.push(IrNode::JmpFalse { label });
                for node in nodes {
                    self.bind_node(node, None)?;
                }
                if else_branch.is_some() {
                    self.nodes.push(IrNode::Jmp {
                        label: end.unwrap(),
                    });
                }
                self.scope.exit();
                self.nodes.push(IrNode::Label { label });
                if let Some(node) = else_branch {
                    self.bind_node(*node, end)?;
                }
            }
            AstNode::Else(nodes) => {
                for node in nodes {
                    self.bind_node(node, jmp_to)?;
                }
                self.nodes.push(IrNode::Label {
                    label: jmp_to.unwrap(),
                });
            }

            AstNode::While(expr, nodes) => {
                self.scope.enter();
                let start = self.new_label();
                let end = self.new_label();
                self.nodes.push(IrNode::Label { label: start });
                self.bind_expr(expr, true)?;
                self.nodes.push(IrNode::JmpFalse { label: end });
                for node in nodes {
                    self.bind_node(node, None)?;
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
                let expr_end_pos = expr_end.pos();
                let expr_end = self.bind_expr(expr_end, true)?;
                if !expr_end.is_int() {
                    Error::type_err("expected int expression", expr_end_pos)?;
                }
                self.nodes.push(IrNode::Load {
                    value: counter.clone(),
                });
                self.nodes.push(IrNode::JmpEq { label: end });
                for node in nodes {
                    self.bind_node(node, None)?;
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

    fn bind(mut self, args: &Vec<Symbol>, nodes: Vec<AstNode>) -> Result<Vec<IrNode>> {
        self.scope.enter();
        for Symbol { name, typ, .. } in args.iter() {
            self.scope.set(name.clone(), typ.clone());
        }
        for node in nodes {
            self.bind_node(node, None)?;
        }
        Ok(self.nodes)
    }
}
