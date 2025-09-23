use std::collections::HashMap;

use crate::ast::{
    Annotated, Argument, Ast, AstNode, AstRoot, Expression, Operator, TypeAnnot, UnaryOperator,
    ValueExpr, VarType,
};
use crate::error::{Error, PositionResult, Result};
use crate::position::Position;
use crate::source::SourceManager;
use crate::token::{Token, TokenStream};

#[derive(Debug, Clone)]
struct Symbol {
    // TODO: add position back in the future for error reporting
    // position: Position<'a>,
    annot: TypeAnnot,
}

impl Symbol {
    fn new(_: Position<'_>, annot: TypeAnnot) -> Self {
        Self { annot }
    }
}

struct SymbolTable(Vec<HashMap<String, Symbol>>);

impl SymbolTable {
    fn new() -> Self {
        Self(vec![HashMap::new()])
    }

    fn enter_scope(&mut self) {
        self.0.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.0.pop();
    }

    fn find(&self, k: &str) -> Option<&Symbol> {
        self.0.iter().rev().find_map(|scope| scope.get(k))
    }

    fn set(&mut self, k: &str, symbol: Symbol) {
        if let Some(scope) = self.0.last_mut() {
            scope.insert(String::from(k), symbol);
        }
    }

    fn set_global(&mut self, k: &str, symbol: Symbol) {
        if let Some(scope) = self.0.first_mut() {
            scope.insert(String::from(k), symbol);
        }
    }
}

pub struct Parser<'a> {
    sources: SourceManager<'a>,
    symbols: SymbolTable,
}

impl<'a> Parser<'a> {
    pub fn new(sources: SourceManager<'a>) -> Self {
        Self {
            sources,
            symbols: SymbolTable::new(),
        }
    }

    #[inline]
    fn next_or_err<'b>(&self, ts: &mut TokenStream<'b>) -> Result<Token<'b>> {
        if let Some(token) = ts.next() {
            Ok(token)
        } else {
            Error::syntatic("unexpected end of tokens", ts.position)
        }
    }

    fn consume_semi(&self, ts: &mut TokenStream) -> Result<()> {
        let Token::SemiColon = self.next_or_err(ts)? else {
            Error::syntatic("expected semicolon `;`", ts.position)?
        };
        Ok(())
    }

    fn parse_str(&self, ts: &TokenStream, s: &str) -> Result<ValueExpr> {
        let mut chs = s.bytes().peekable();
        let mut acc = Vec::with_capacity(s.len());

        loop {
            let Some(curr) = chs.next() else {
                acc.push(b'\0');
                let escaped = String::from_utf8(acc).with_position(ts.position)?;
                break Ok(ValueExpr::String(escaped));
            };

            let b = match curr {
                b'\\' => match &[chs.next().unwrap()] {
                    b"n" => b'\n',
                    b"r" => b'\r',
                    b"t" => b'\t',
                    b"0" => b'\0',
                    b"\"" => b'"',
                    b"\\" => b'\\',
                    _ => return Error::syntatic("unknown escape sequence", ts.position),
                },
                _ => curr,
            };

            acc.push(b);
        }
    }

    fn parse_value(&self, ts: &mut TokenStream) -> Result<ValueExpr> {
        let expr = match self.next_or_err(ts)? {
            Token::Identifier(id) => {
                let Some(symbol) = self.symbols.find(id) else {
                    Error::syntatic("symbol not found in scope", ts.position)?
                };
                ValueExpr::Identifier(symbol.annot, id.into())
            }
            Token::StrLiteral(string) => self.parse_str(ts, string)?,
            Token::IntLiteral(int) => ValueExpr::Int(int as i32),
            Token::FloatLiteral(float) => ValueExpr::Float(float),
            Token::Sub => match self.next_or_err(ts)? {
                Token::IntLiteral(int) => ValueExpr::Int(-(int as i32)),
                Token::FloatLiteral(float) => ValueExpr::Float(-float),
                _ => Error::syntatic("unexpected token", ts.position)?,
            },
            Token::True => ValueExpr::Bool(true),
            Token::False => ValueExpr::Bool(false),
            _ => Error::syntatic("unexpected token", ts.position)?,
        };

        Ok(expr)
    }

    fn parse_atom(&self, ts: &mut TokenStream) -> Result<Expression> {
        let expr = match self.next_or_err(ts)? {
            Token::StrLiteral(string) => self.parse_str(ts, string)?.into(),
            Token::IntLiteral(int) => Expression::int(int as i32),
            Token::FloatLiteral(float) => Expression::float(float),
            Token::True => Expression::TRUE,
            Token::False => Expression::FALSE,
            Token::Identifier(id) if matches!(ts.peek(), Some(Token::OpenParen)) => {
                let Some(sym) = self.symbols.find(id).cloned() else {
                    Error::syntatic("symbol not found in scope", ts.position)?
                };
                let annot = sym.annot;
                let args = self.parse_callargs(ts)?;
                Expression::func(id.into(), args, annot)
            }
            Token::Identifier(id) if matches!(ts.peek(), Some(Token::OpenBracket)) => {
                self.next_or_err(ts)?; // discard open bracket
                let pos = ts.position;
                let Some(sym) = self.symbols.find(id).cloned() else {
                    Error::syntatic("symbol not found in scope", pos)?
                };
                let annot = sym.annot;
                let index = self.parse_expr(ts, 1)?;
                if !index.get_annot().is_int() {
                    Error::syntatic("array index should be int", ts.position)?
                }
                let Token::CloseBracket = self.next_or_err(ts)? else {
                    Error::syntatic("expected close bracket", ts.position)?
                };
                if !annot.is_ref() {
                    Error::syntatic("indexing can only be applied to arrays and pointers", pos)?
                }
                let array = ValueExpr::Identifier(annot, id.into());
                Expression::index(array, index, annot.deref())
            }
            Token::Identifier(id) => {
                let Some(symbol) = self.symbols.find(id) else {
                    Error::syntatic("symbol not found in scope", ts.position)?
                };
                Expression::identifier(symbol.annot, id)
            }
            Token::OpenParen => {
                let inner_expr = self.parse_expr(ts, 1)?;
                let Token::CloseParen = self.next_or_err(ts)? else {
                    Error::syntatic("expected close parentheses", ts.position)?
                };
                inner_expr
            }
            _ => Error::syntatic("unexpected token", ts.position)?,
        };

        Ok(expr)
    }

    fn check_unaexpr(op: UnaryOperator, operand: &Expression, pos: Position) -> Result<TypeAnnot> {
        let annot = operand.get_annot();
        match op {
            UnaryOperator::AddressOf if annot.is_max_indirection() => {
                Error::syntatic("too much indirection", pos)
            }
            UnaryOperator::AddressOf if operand.is_ident() => Ok(annot.create_ref()),
            UnaryOperator::AddressOf => Error::syntatic("a ref can only be taken from a var", pos),

            UnaryOperator::Minus if annot.is_number() => Ok(annot),
            UnaryOperator::Minus => Error::syntatic("cannot apply unary minus here", pos),

            UnaryOperator::Dereference if annot.is_ref() => Ok(annot.deref()),
            UnaryOperator::Dereference if annot.is_int() => Ok(TypeAnnot::VOID),
            UnaryOperator::Dereference => Error::syntatic("can only dereference addresses", pos),

            UnaryOperator::Not if annot.is_bool() => Ok(annot),
            UnaryOperator::Not => Error::syntatic("cannot apply unary not here", pos),

            UnaryOperator::BitwiseNot if annot.is_int() => Ok(annot),
            UnaryOperator::BitwiseNot => Error::syntatic("cannot apply bitwise not here", pos),
        }
    }

    fn check_binexpr(
        op: Operator,
        lhs: &Expression,
        rhs: &Expression,
        pos: Position,
    ) -> Result<TypeAnnot> {
        let lhs = lhs.get_annot();
        let rhs = rhs.get_annot();
        match op {
            Operator::Equal | Operator::NotEqual if lhs == rhs => Ok(TypeAnnot::BOOL),

            Operator::Greater | Operator::GreaterEqual | Operator::Less | Operator::LessEqual
                if lhs == rhs && lhs.is_number() =>
            {
                Ok(TypeAnnot::BOOL)
            }

            Operator::And | Operator::Or if lhs == rhs && lhs.is_bool() => Ok(TypeAnnot::BOOL),

            Operator::Assign if lhs == rhs => Ok(lhs),

            Operator::Add
            | Operator::Sub
            | Operator::Mul
            | Operator::Div
            | Operator::AddAssign
            | Operator::SubAssign
            | Operator::MulAssign
            | Operator::DivAssign
                if lhs == rhs && lhs.is_number() =>
            {
                Ok(lhs)
            }

            Operator::Mod
            | Operator::Shr
            | Operator::Shl
            | Operator::BitwiseAnd
            | Operator::BitwiseOr
            | Operator::BitwiseXor
                if lhs == rhs && lhs.is_int() =>
            {
                Ok(lhs)
            }

            _ => Error::syntatic("invalid operation between types", pos),
        }
    }

    fn check_cast(value: &Expression, target: TypeAnnot, pos: Position) -> Result<TypeAnnot> {
        let annot = value.get_annot();

        if annot.is_number() && target.is_number() {
            Ok(target)
        } else {
            let msg = format!("cannot cast from {annot} to {target}");
            Error::syntatic(&msg, pos)
        }
    }

    fn parse_expr(&self, ts: &mut TokenStream, min_prec: u8) -> Result<Expression> {
        let mut lhs = match ts.peek_unaop() {
            Some(op) => {
                self.next_or_err(ts)?;
                let prec = op.precedence();
                let operand = self.parse_expr(ts, prec)?;
                let result = Parser::check_unaexpr(op, &operand, ts.position)?;

                Expression::una_op(op, operand, result)
            }
            None => self.parse_atom(ts)?,
        };

        if let Some(Token::As) = ts.peek() {
            self.next_or_err(ts)?;
            let target = self.parse_annot(ts)?;
            let target = Parser::check_cast(&lhs, target, ts.position)?;

            lhs = Expression::cast(lhs, target);
        }

        loop {
            let Some(op) = ts.peek_binop() else {
                break;
            };

            let prec = op.precedence();
            if prec < min_prec {
                break;
            }
            let min_prec = if op.is_right() { prec } else { prec + 1 };

            ts.next(); // consume operator

            let rhs = self.parse_expr(ts, min_prec)?;
            let result = Parser::check_binexpr(op, &lhs, &rhs, ts.position)?;

            lhs = Expression::bin_op(op, lhs, rhs, result);
        }

        Ok(lhs)
    }

    fn consume_expr(&mut self, ts: &mut TokenStream) -> Result<AstNode> {
        let expr = self.parse_expr(ts, 1)?;
        self.consume_semi(ts)?;
        let expr = AstNode::Expr(expr);
        Ok(expr)
    }

    fn consume_for(&mut self, ts: &mut TokenStream) -> Result<AstNode> {
        ts.next(); // discard 'for' token
        let Token::Identifier(ident) = self.next_or_err(ts)? else {
            Error::syntatic("expected identifier", ts.position)?
        };
        self.symbols
            .set(ident, Symbol::new(ts.position, TypeAnnot::INT));
        let init = if let Some(Token::Assign) = ts.peek() {
            self.next_or_err(ts)?;
            Some(self.parse_value(ts)?)
        } else {
            None
        };
        let Token::To = self.next_or_err(ts)? else {
            Error::syntatic("expected token `to`", ts.position)?
        };
        let limit = self.parse_value(ts)?;
        let Token::Do = self.next_or_err(ts)? else {
            Error::syntatic("expected token `do`", ts.position)?
        };
        let inner = self.consume_inner(ts)?;
        AstNode::For(ident.into(), init, limit, inner).ok()
    }

    fn consume_while(&mut self, ts: &mut TokenStream) -> Result<AstNode> {
        ts.next(); // discard 'while' token
        let expr = self.parse_expr(ts, 1)?;
        let Token::Do = self.next_or_err(ts)? else {
            Error::syntatic("expected `do`", ts.position)?
        };
        let inner = self.consume_inner(ts)?;
        AstNode::While(expr, inner).ok()
    }

    fn consume_if(&mut self, ts: &mut TokenStream) -> Result<AstNode> {
        ts.next(); // discard 'if' token
        let expr = self.parse_expr(ts, 1)?;
        let Token::Then = self.next_or_err(ts)? else {
            Error::syntatic("expected `then`", ts.position)?
        };
        let inner = self.consume_inner(ts)?;
        AstNode::If(expr, inner).ok()
    }

    fn parse_annot(&self, ts: &mut TokenStream) -> Result<TypeAnnot> {
        let ty = match self.next_or_err(ts)? {
            Token::Int => VarType::Int,
            Token::Real => VarType::Real,
            Token::Char => VarType::Char,
            Token::Bool => VarType::Bool,
            _ => Error::syntatic("expected type annotation", ts.position)?,
        };
        match ts.peek() {
            Some(Token::OpenBracket) => {
                self.next_or_err(ts)?; // discard open bracket
                let ValueExpr::Int(size) = self.parse_value(ts)? else {
                    Error::syntatic("expected integer literal", ts.position)?
                };
                let Token::CloseBracket = self.next_or_err(ts)? else {
                    Error::syntatic("expected close bracket", ts.position)?
                };
                Ok(TypeAnnot::new_array(ty, size as u32))
            }
            Some(Token::Mul) => {
                self.next_or_err(ts)?; // discard asterisk
                let mut indirection = 1;
                while let Some(Token::Mul) = ts.peek() {
                    self.next_or_err(ts)?; // discard asterisk
                    indirection += 1;
                }
                Ok(TypeAnnot::new_ptr(ty, indirection))
            }
            _ => Ok(TypeAnnot::new(ty)),
        }
    }

    fn consume_var(&mut self, ts: &mut TokenStream) -> Result<AstNode> {
        if let Some(Token::Var) = ts.peek() {
            self.next_or_err(ts)?;
            let Token::Identifier(ident) = self.next_or_err(ts)? else {
                Error::syntatic("expected identifier", ts.position)?
            };
            let sym_pos = ts.position;
            let Token::Assign = self.next_or_err(ts)? else {
                Error::syntatic("expected assign operator", ts.position)?
            };
            let expr = self.parse_expr(ts, 1)?;
            let annot = expr.get_annot();
            self.consume_semi(ts)?;
            self.symbols.set(ident, Symbol::new(sym_pos, annot));
            AstNode::Var(annot, ident.into(), Some(expr)).ok()
        } else {
            let annot = self.parse_annot(ts)?;
            let Token::Identifier(ident) = self.next_or_err(ts)? else {
                Error::syntatic("expected identifier", ts.position)?
            };
            let sym_pos = ts.position;
            let expr = if let Some(Token::Assign) = ts.peek() {
                self.next_or_err(ts)?; // discard assign signal
                let expr_pos = ts.position;
                let expr = self.parse_expr(ts, 1)?;
                let expr_annot = expr.get_annot();
                if expr_annot != annot {
                    Error::syntatic(&format!("cannot assign {expr_annot} to {annot}"), expr_pos)?
                }
                Some(expr)
            } else {
                None
            };
            self.consume_semi(ts)?;
            self.symbols.set(&ident, Symbol::new(sym_pos, annot));
            AstNode::Var(annot, ident.into(), expr).ok()
        }
    }

    fn consume_global(&mut self, ts: &mut TokenStream) -> Result<AstRoot> {
        let annot = self.parse_annot(ts)?;
        let Token::Identifier(ident) = self.next_or_err(ts)? else {
            Error::syntatic("expected identifier", ts.position)?
        };
        let id_pos = ts.position;
        let expr = if let Some(Token::Assign) = ts.peek() {
            self.next_or_err(ts)?; // discard assign signal
            let expr_pos = ts.position;
            let expr = self.parse_value(ts)?;
            let expr_annot = expr.get_annot();
            if expr_annot != annot {
                Error::syntatic(&format!("cannot assign {expr_annot} to {annot}"), expr_pos)?
            }
            Some(expr)
        } else {
            None
        };
        self.consume_semi(ts)?;
        self.symbols.set(ident, Symbol::new(id_pos, annot));
        AstRoot::Global(annot, ident.into(), expr).ok()
    }

    fn parse_args(&mut self, ts: &mut TokenStream) -> Result<Vec<Argument>> {
        let Token::OpenParen = self.next_or_err(ts)? else {
            Error::syntatic("expected open parenthesis `(`", ts.position)?
        };
        let mut state = 1u8;
        let mut args = Vec::new();
        loop {
            match (state, self.next_or_err(ts)?) {
                (1 | 2, Token::CloseParen) => break,
                (1 | 3, Token::Identifier(name)) => {
                    state = 2;
                    let arg_pos = ts.position;
                    let Token::Colon = self.next_or_err(ts)? else {
                        Error::syntatic("expected token `:`", ts.position)?
                    };
                    let annot = self.parse_annot(ts)?;
                    let arg = Argument::new(name, annot);
                    args.push(arg);
                    self.symbols.set(name, Symbol::new(arg_pos, annot));
                }
                (2, Token::Comma) => state = 3,
                (1 | 2, _) => {
                    Error::syntatic("expected close parenthesis `)` or argument", ts.position)?
                }
                (3, _) => Error::syntatic("expected another argument after comma", ts.position)?,
                _ => unreachable!("The state machine is out of control"),
            };
        }
        Ok(args)
    }

    fn parse_callargs(&self, ts: &mut TokenStream) -> Result<Vec<Expression>> {
        let Token::OpenParen = self.next_or_err(ts)? else {
            Error::syntatic("expected open parenthesis `(`", ts.position)?
        };
        let mut state = 1u8;
        let mut args = Vec::new();
        loop {
            match (state, ts.peek()) {
                (1 | 2, Some(Token::CloseParen)) => {
                    _ = ts.next();
                    break;
                }
                (1 | 3, ..) => {
                    state = 2;
                    let expr = self.parse_expr(ts, 1)?;
                    args.push(expr);
                }
                (2, Some(Token::Comma)) => {
                    _ = ts.next();
                    state = 3;
                }
                (2, ..) => {
                    Error::syntatic("expected close parenthesis `)` or comma `,`", ts.position)?
                }
                _ => unreachable!("The state machine is out of control"),
            };
        }
        Ok(args)
    }

    fn check_func(inner: &Vec<AstNode>, annot: TypeAnnot, fn_pos: Position) -> Result<TypeAnnot> {
        for node in inner {
            if let AstNode::Ret(expr) = node {
                if expr.get_annot() == annot {
                    return Ok(annot);
                } else {
                    return Error::syntatic("return type does not match declaration", fn_pos);
                }
            }

            let block_type = match node {
                AstNode::If(.., nodes) => Parser::check_func(nodes, annot, fn_pos)?,
                AstNode::While(.., nodes) => Parser::check_func(nodes, annot, fn_pos)?,
                AstNode::For(.., nodes) => Parser::check_func(nodes, annot, fn_pos)?,
                _ => continue,
            };

            if !block_type.is_void() && block_type != annot {
                return Error::syntatic("return type does not match declaration", fn_pos);
            }
        }

        Ok(TypeAnnot::VOID)
    }

    fn consume_func(&mut self, ts: &mut TokenStream) -> Result<AstRoot> {
        ts.next(); // discard function token
        let Token::Identifier(name) = self.next_or_err(ts)? else {
            Error::syntatic("expected name of the function", ts.position)?
        };
        let fn_pos = ts.position;

        self.symbols.enter_scope(); // args
        let args = self.parse_args(ts)?;

        let annot = match self.next_or_err(ts)? {
            Token::Colon => {
                let ret_type = self.parse_annot(ts)?;
                let Token::Do = self.next_or_err(ts)? else {
                    Error::syntatic("expected `do`", ts.position)?
                };
                ret_type
            }
            Token::Do => TypeAnnot::VOID,
            _ => Error::syntatic("expected type annotation or `do`", ts.position)?,
        };

        self.symbols.set_global(name, Symbol::new(fn_pos, annot));

        let inner = self.consume_inner(ts)?;
        self.symbols.exit_scope(); // args

        Parser::check_func(&inner, annot, fn_pos)?;

        AstRoot::Func(name.into(), args, annot, inner).ok()
    }

    fn consume_ret(&self, ts: &mut TokenStream) -> Result<AstNode> {
        ts.next(); // discard return token
        let expr = self.parse_expr(ts, 1)?;
        self.consume_semi(ts)?;
        let expr = AstNode::Ret(expr);
        Ok(expr)
    }

    fn consume_extern(&mut self, ts: &mut TokenStream) -> Result<AstRoot> {
        self.next_or_err(ts)?; // discard extern token
        let Token::Function = self.next_or_err(ts)? else {
            return Error::syntatic("expected `func` keyword", ts.position);
        };
        let Token::Identifier(name) = self.next_or_err(ts)? else {
            return Error::syntatic("expected name of the function", ts.position);
        };
        let fn_pos = ts.position;

        self.symbols.enter_scope();
        let args = self.parse_args(ts)?;

        let annot = if let Some(Token::Colon) = ts.peek() {
            self.next_or_err(ts)?;
            self.parse_annot(ts)?
        } else {
            TypeAnnot::VOID
        };

        self.consume_semi(ts)?;
        self.symbols.exit_scope();

        self.symbols.set_global(name, Symbol::new(fn_pos, annot));

        AstRoot::ExternFunc(name.into(), args, annot).ok()
    }

    fn consume_use(&mut self, ts: &mut TokenStream) -> Result<Vec<AstRoot>> {
        ts.next(); // discard use token
        let Token::StrLiteral(path) = self.next_or_err(ts)? else {
            Error::syntatic("expected identifier", ts.position)?
        };
        self.consume_semi(ts)?;

        let source = self.sources.source(path)?;
        let mut tokens = source.tokenize()?;

        return self.consume_root(&mut tokens);
    }

    fn consume_inner(&mut self, ts: &mut TokenStream) -> Result<Vec<AstNode>> {
        let mut nodes = Vec::new();

        self.symbols.enter_scope();

        while let Some(token) = ts.peek() {
            if let Token::End = token {
                ts.next();
                break;
            }

            if let Token::Eof = token {
                return Error::syntatic("unexpected end of file", ts.position);
            }

            let node = match token {
                Token::If => self.consume_if(ts),
                Token::While => self.consume_while(ts),
                Token::For => self.consume_for(ts),

                Token::Var | Token::Int | Token::Real | Token::Char | Token::Bool => {
                    self.consume_var(ts)
                }

                Token::OpenParen
                | Token::True
                | Token::False
                | Token::IntLiteral(..)
                | Token::FloatLiteral(..)
                | Token::StrLiteral(..)
                | Token::Identifier(..)
                | Token::Mul => self.consume_expr(ts),

                Token::Return => self.consume_ret(ts),

                Token::Eof | Token::End => unreachable!(),
                _ => Error::syntatic("wrong placement for this token", ts.peek_pos()),
            }?;

            nodes.push(node);
        }

        self.symbols.exit_scope();

        Ok(nodes)
    }

    fn consume_root(&mut self, ts: &mut TokenStream) -> Result<Vec<AstRoot>> {
        let mut nodes = Vec::new();

        while let Some(token) = ts.peek() {
            if let Token::Eof = token {
                break;
            }

            if let Token::Use = token {
                let mut other = self.consume_use(ts)?;
                nodes.append(&mut other);
                continue;
            }

            let node = match token {
                Token::Int | Token::Real | Token::Char | Token::Bool => self.consume_global(ts),

                Token::Function => self.consume_func(ts),
                Token::Extern => self.consume_extern(ts),

                Token::Eof | Token::Use => unreachable!(),
                _ => Error::syntatic("wrong placement for this token", ts.peek_pos()),
            }?;

            nodes.push(node);
        }

        Ok(nodes)
    }

    pub fn parse(&mut self) -> Result<Ast> {
        let source = self.sources.main()?;
        let mut tokens = source.tokenize()?;
        let nodes = self.consume_root(&mut tokens)?;
        Ast::new(nodes).ok()
    }
}
