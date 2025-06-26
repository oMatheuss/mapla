use std::collections::HashMap;
use std::iter::Peekable;
use std::slice::Iter;

use crate::ast::{
    Annotated, Annotation, Argument, Ast, AstNode, Expression, Operator, TypeAnnot, UnaryOperator,
    ValueExpr, VarType,
};
use crate::error::{Error, Result};
use crate::lexer::LexItem;
use crate::position::Position;
use crate::token::Token;

#[derive(Debug, Clone)]
struct Symbol {
    position: Position,
    annot: TypeAnnot,
}

impl Symbol {
    fn new(position: Position, annot: TypeAnnot) -> Self {
        Self { position, annot }
    }

    const fn intrinsic(ty: VarType) -> Self {
        Self {
            position: Position::ZERO,
            annot: TypeAnnot::new(ty),
        }
    }
}

struct SymbolTable<'a>(Vec<HashMap<&'a str, Symbol>>);

const GLOBALS: [(&'static str, Symbol); 4] = [
    ("printInt", Symbol::intrinsic(VarType::Void)),
    ("printChar", Symbol::intrinsic(VarType::Void)),
    ("printString", Symbol::intrinsic(VarType::Void)),
    ("readString", Symbol::intrinsic(VarType::Int)),
];

impl<'a> SymbolTable<'a> {
    fn new() -> Self {
        let global = HashMap::from(GLOBALS);
        Self(vec![global])
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

    fn set(&mut self, k: &'a str, symbol: Symbol) {
        if let Some(scope) = self.0.last_mut() {
            scope.insert(k, symbol);
        }
    }

    fn set_global(&mut self, k: &'a str, symbol: Symbol) {
        if let Some(scope) = self.0.first_mut() {
            scope.insert(k, symbol);
        }
    }
}

pub struct Parser<'a> {
    tokens: Peekable<Iter<'a, LexItem<'a>>>,
    symbols: SymbolTable<'a>,
    pos: Position,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [LexItem<'a>]) -> Self {
        Self {
            tokens: tokens.into_iter().peekable(),
            symbols: SymbolTable::new(),
            pos: Position::default(),
        }
    }

    #[inline]
    fn next(&mut self) -> Option<&'a Token<'a>> {
        match self.tokens.next() {
            Some(item) => {
                self.pos = *item.position();
                Some(item.token())
            }
            None => None,
        }
    }

    #[inline]
    fn peek(&mut self) -> Option<&'a Token<'a>> {
        match self.tokens.peek() {
            Some(some) => Some(some.token()),
            None => None,
        }
    }

    #[inline]
    fn next_or_err(&mut self) -> Result<&'a Token<'a>> {
        if let Some(token) = self.next() {
            Ok(token)
        } else {
            Error::syntatic("unexpected end of tokens", self.pos)
        }
    }

    fn consume_semi(&mut self) -> Result<()> {
        let Token::SemiColon = self.next_or_err()? else {
            Error::syntatic("expected semicolon `;`", self.pos)?
        };
        Ok(())
    }

    fn parse_value(&mut self) -> Result<ValueExpr> {
        let token = self.next_or_err()?;

        let expr = match *token {
            Token::Identifier(id) => {
                let Some(symbol) = self.symbols.find(id) else {
                    Error::syntatic("symbol not found in scope", self.pos)?
                };
                ValueExpr::Identifier(symbol.annot, id.into())
            }
            Token::StrLiteral(string) => ValueExpr::String(string.into()),
            Token::IntLiteral(int) => ValueExpr::Int(int),
            Token::FloatLiteral(float) => ValueExpr::Float(float),
            Token::True => ValueExpr::Bool(true),
            Token::False => ValueExpr::Bool(false),
            _ => Error::syntatic("unexpected token", self.pos)?,
        };

        Ok(expr)
    }

    fn parse_atom(&mut self) -> Result<Expression> {
        let token: &Token<'_> = self.next_or_err()?;

        let expr = match *token {
            Token::Identifier(id) if matches!(self.peek(), Some(Token::OpenParen)) => {
                let Some(symbol) = self.symbols.find(id).cloned() else {
                    Error::syntatic("symbol not found in scope", self.pos)?
                };
                let args = self.parse_callargs()?;
                Expression::func(id.into(), args, symbol.annot)
            }
            Token::Identifier(id) => {
                let Some(symbol) = self.symbols.find(id) else {
                    Error::syntatic("symbol not found in scope", self.pos)?
                };
                Expression::identifier(symbol.annot, id)
            }
            Token::StrLiteral(string) => Expression::string(string),
            Token::IntLiteral(int) => Expression::int(int),
            Token::FloatLiteral(float) => Expression::float(float),
            Token::True => Expression::TRUE,
            Token::False => Expression::FALSE,
            Token::OpenParen => {
                let inner_expr = self.parse_expr(1)?;
                let Token::CloseParen = self.next_or_err()? else {
                    Error::syntatic("expected close parentheses", self.pos)?
                };
                inner_expr
            }
            Token::Ampersand => {
                let Token::Identifier(id) = *self.next_or_err()? else {
                    Error::syntatic("a ref can only be taken from a var", self.pos)?
                };
                let Some(symbol) = self.symbols.find(id).cloned() else {
                    Error::syntatic("symbol not found in scope", self.pos)?
                };
                let operand = ValueExpr::Identifier(symbol.annot, id.into());
                let annot = TypeAnnot::new_ptr(symbol.annot.inner_type());
                Expression::una_op(UnaryOperator::AddressOf, operand.into(), annot)
            }
            Token::Sub => {
                let operand = self.parse_atom()?;
                let annot = operand.get_annot();
                if matches!(
                    annot.annotation(),
                    Annotation::Array(..) | Annotation::Pointer
                ) {
                    Error::syntatic("can only apply unary operator minus to numbers", self.pos)?
                }
                if !matches!(annot.inner_type(), VarType::Int | VarType::Real) {
                    Error::syntatic("can only apply unary operator minus to numbers", self.pos)?
                }
                Expression::una_op(UnaryOperator::Minus, operand, annot)
            }
            _ => Error::syntatic("unexpected token", self.pos)?,
        };

        Ok(expr)
    }

    fn peek_operator(&mut self) -> Option<Operator> {
        let token = self.peek()?;

        let op = match token {
            Token::Equal => Operator::Equal,
            Token::NotEqual => Operator::NotEqual,
            Token::Greater => Operator::Greater,
            Token::GreaterEqual => Operator::GreaterEqual,
            Token::Less => Operator::Less,
            Token::LessEqual => Operator::LessEqual,
            Token::And => Operator::And,
            Token::Or => Operator::Or,
            Token::Add => Operator::Add,
            Token::Sub => Operator::Sub,
            Token::Mul => Operator::Mul,
            Token::Div => Operator::Div,
            Token::Mod => Operator::Mod,
            Token::Assign => Operator::Assign,
            Token::AddAssign => Operator::AddAssign,
            Token::SubAssign => Operator::SubAssign,
            Token::MulAssign => Operator::MulAssign,
            Token::DivAssign => Operator::DivAssign,
            _ => return None,
        };

        Some(op)
    }

    fn check_expr(
        &mut self,
        op: Operator,
        lhs: &Expression,
        rhs: &Expression,
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

            Operator::Add
            | Operator::Sub
            | Operator::Mul
            | Operator::Div
            | Operator::Assign
            | Operator::AddAssign
            | Operator::SubAssign
            | Operator::MulAssign
            | Operator::DivAssign
                if lhs == rhs && lhs.is_number() =>
            {
                Ok(lhs)
            }

            Operator::Mod if lhs.is_int() && lhs == rhs => Ok(lhs),

            _ => Error::syntatic("invalid operation between types", self.pos),
        }
    }

    fn parse_expr(&mut self, min_prec: u8) -> Result<Expression> {
        let mut lhs = self.parse_atom()?;

        loop {
            let Some(op) = self.peek_operator() else {
                break;
            };

            let prec = op.precedence();
            if prec < min_prec {
                break;
            }
            let min_prec = if op.is_right() { prec } else { prec + 1 };

            self.next(); // consume operator

            let rhs = self.parse_expr(min_prec)?;
            let result = self.check_expr(op, &lhs, &rhs)?;

            lhs = Expression::bin_op(op, lhs, rhs, result);
        }

        Ok(lhs)
    }

    fn consume_expr(&mut self) -> Result<AstNode> {
        let expr = self.parse_expr(1)?;
        self.consume_semi()?;
        let expr = AstNode::Expr(expr);
        Ok(expr)
    }

    fn consume_for(&mut self) -> Result<AstNode> {
        self.next(); // discard 'for' token
        let Token::Identifier(ident) = self.next_or_err()? else {
            Error::syntatic("expected identifier", self.pos)?
        };
        self.symbols
            .set(&ident, Symbol::new(self.pos, TypeAnnot::INT));
        let init = if let Some(Token::Assign) = self.peek() {
            self.next_or_err()?;
            Some(self.parse_value()?)
        } else {
            None
        };
        let Token::To = self.next_or_err()? else {
            Error::syntatic("expected token `to`", self.pos)?
        };
        let limit = self.parse_value()?;
        let Token::Then = self.next_or_err()? else {
            Error::syntatic("expected token `then`", self.pos)?
        };
        let inner = self.consume_inner()?;
        AstNode::For((*ident).into(), init, limit, inner).ok()
    }

    fn consume_while(&mut self) -> Result<AstNode> {
        self.next(); // discard 'while' token
        let expr = self.parse_expr(1)?;
        let Token::Then = self.next_or_err()? else {
            Error::syntatic("expected `then`", self.pos)?
        };
        let inner = self.consume_inner()?;
        AstNode::While(expr, inner).ok()
    }

    fn consume_if(&mut self) -> Result<AstNode> {
        self.next(); // discard 'if' token
        let expr = self.parse_expr(1)?;
        let Token::Then = self.next_or_err()? else {
            Error::syntatic("expected `then`", self.pos)?
        };
        let inner = self.consume_inner()?;
        AstNode::If(expr, inner).ok()
    }

    fn consume_var(&mut self, ty: VarType) -> Result<AstNode> {
        self.next(); // discard var type token
        let annot = if let Some(Token::OpenBracket) = self.peek() {
            self.next_or_err()?; // discard open bracket
            let ValueExpr::Int(size) = self.parse_value()? else {
                Error::syntatic("expected integer value", self.pos)?
            };
            let Token::CloseBracket = self.next_or_err()? else {
                Error::syntatic("expected close bracket", self.pos)?
            };
            TypeAnnot::new_array(ty, size as u32)
        } else {
            TypeAnnot::new(ty)
        };
        let id_pos = self.pos;
        let Token::Identifier(ident) = *self.next_or_err()? else {
            Error::syntatic("expected identifier", self.pos)?
        };
        let expr = if let Some(Token::Assign) = self.peek() {
            self.next_or_err()?; // discard assign signal
            let expr_pos = self.pos;
            let expr = self.parse_expr(1)?;
            let expr_annot = expr.get_annot();
            if expr_annot != annot {
                Error::syntatic(&format!("cannot assign {expr_annot} to {annot}"), expr_pos)?
            }
            Some(expr)
        } else {
            None
        };
        self.consume_semi()?;
        self.symbols.set(&ident, Symbol::new(id_pos, annot));
        AstNode::Var(annot, ident.into(), expr).ok()
    }

    fn parse_annot(&mut self) -> Result<TypeAnnot> {
        let ty = match self.next_or_err()? {
            Token::Int => VarType::Int,
            Token::Real => VarType::Real,
            Token::Char => VarType::Char,
            Token::Bool => VarType::Bool,
            _ => Error::syntatic("expected type annotation", self.pos)?,
        };
        let annot = if let Some(Token::Mul) = self.peek() {
            self.next_or_err()?; // discard asterisk
            TypeAnnot::new_ptr(ty)
        } else {
            TypeAnnot::new(ty)
        };
        Ok(annot)
    }

    fn parse_args(&mut self) -> Result<Vec<Argument>> {
        let Token::OpenParen = self.next_or_err()? else {
            Error::syntatic("expected open parenthesis `(`", self.pos)?
        };
        let mut state = 1u8;
        let mut args = Vec::new();
        loop {
            match (state, self.next_or_err()?) {
                (1 | 2, Token::CloseParen) => break,
                (1 | 3, Token::Identifier(name)) => {
                    state = 2;
                    let arg_pos = self.pos;
                    let Token::Colon = self.next_or_err()? else {
                        Error::syntatic("expected token `:`", self.pos)?
                    };
                    let annot = self.parse_annot()?;
                    let arg = Argument::new(name, annot);
                    args.push(arg);
                    self.symbols.set(&name, Symbol::new(arg_pos, annot));
                }
                (2, Token::Comma) => state = 3,
                (1 | 2, _) => {
                    Error::syntatic("expected close parenthesis `)` or argument", self.pos)?
                }
                (3, _) => Error::syntatic("expected another argument after comma", self.pos)?,
                _ => unreachable!("The state machine is out of control"),
            };
        }
        Ok(args)
    }

    fn parse_callargs(&mut self) -> Result<Vec<Expression>> {
        let Token::OpenParen = self.next_or_err()? else {
            Error::syntatic("expected open parenthesis `(`", self.pos)?
        };
        let mut state = 1u8;
        let mut args = Vec::new();
        loop {
            match (state, self.peek()) {
                (1 | 2, Some(Token::CloseParen)) => {
                    _ = self.next();
                    break;
                }
                (1 | 3, ..) => {
                    state = 2;
                    let expr = self.parse_expr(1)?;
                    args.push(expr);
                }
                (2, Some(Token::Comma)) => {
                    _ = self.next();
                    state = 3;
                }
                (2, ..) => {
                    Error::syntatic("expected close parenthesis `)` or comma `,`", self.pos)?
                }
                _ => unreachable!("The state machine is out of control"),
            };
        }
        Ok(args)
    }

    fn check_func(
        &mut self,
        inner: &Vec<AstNode>,
        annot: TypeAnnot,
        fn_pos: Position,
    ) -> Result<TypeAnnot> {
        for node in inner {
            if let AstNode::Ret(expr) = node {
                if expr.get_annot() == annot {
                    return Ok(annot);
                } else {
                    return Error::syntatic("return type does not match declaration", fn_pos);
                }
            }

            let block_type = match node {
                AstNode::If(.., nodes) => self.check_func(nodes, annot, fn_pos)?,
                AstNode::While(.., nodes) => self.check_func(nodes, annot, fn_pos)?,
                AstNode::For(.., nodes) => self.check_func(nodes, annot, fn_pos)?,
                _ => continue,
            };

            if !block_type.is_void() && block_type != annot {
                return Error::syntatic("return type does not match declaration", fn_pos);
            }
        }

        Ok(TypeAnnot::VOID)
    }

    fn consume_func(&mut self) -> Result<AstNode> {
        self.next(); // discard function token
        let Token::Identifier(name) = self.next_or_err()? else {
            Error::syntatic("expected name of the function", self.pos)?
        };
        let fn_pos = self.pos;

        self.symbols.enter_scope(); // args
        let args = self.parse_args()?;

        let annot = match self.next_or_err()? {
            Token::Colon => {
                let ret_type = self.parse_annot()?;
                let Token::Do = self.next_or_err()? else {
                    Error::syntatic("expected `do`", self.pos)?
                };
                ret_type
            }
            Token::Do => TypeAnnot::VOID,
            _ => Error::syntatic("expected type annotation or `do`", self.pos)?,
        };

        self.symbols.set_global(&name, Symbol::new(fn_pos, annot));

        let inner = self.consume_inner()?;
        self.symbols.exit_scope(); // args

        self.check_func(&inner, annot, fn_pos)?;

        AstNode::Func((*name).into(), args, annot, inner).ok()
    }

    fn consume_ret(&mut self) -> Result<AstNode> {
        self.next(); // discard return token
        let expr = self.parse_expr(1)?;
        self.consume_semi()?;
        let expr = AstNode::Ret(expr);
        Ok(expr)
    }

    fn consume_use(&mut self) -> Result<AstNode> {
        self.next(); // discard use token
        let Token::Identifier(ident) = self.next_or_err()? else {
            Error::syntatic("expected identifier", self.pos)?
        };
        self.consume_semi()?;
        AstNode::Use((*ident).into()).ok()
    }

    #[inline]
    fn match_token(&mut self, token: &Token<'_>) -> Result<AstNode> {
        match token {
            Token::Use => self.consume_use(),

            Token::If => self.consume_if(),
            Token::While => self.consume_while(),
            Token::For => self.consume_for(),

            Token::Int => self.consume_var(VarType::Int),
            Token::Real => self.consume_var(VarType::Real),
            Token::Char => self.consume_var(VarType::Char),
            Token::Bool => self.consume_var(VarType::Bool),

            Token::Function => self.consume_func(),
            Token::Return => self.consume_ret(),

            Token::OpenParen
            | Token::True
            | Token::False
            | Token::IntLiteral(..)
            | Token::FloatLiteral(..)
            | Token::StrLiteral(..)
            | Token::Identifier(..) => self.consume_expr(),

            Token::Eof | Token::End => panic!("token not allowed"),
            _ => Error::syntatic("wrong placement for this token", self.pos),
        }
    }

    fn consume_inner(&mut self) -> Result<Vec<AstNode>> {
        let mut nodes = Vec::new();

        self.symbols.enter_scope();

        while let Some(token) = self.peek() {
            if let Token::End = token {
                self.next();
                break;
            }

            if let Token::Eof = token {
                return Error::syntatic("unexpected end of file", self.pos);
            }

            let node = self.match_token(token)?;
            nodes.push(node);
        }

        self.symbols.exit_scope();

        Ok(nodes)
    }

    pub fn parse(mut self) -> Result<Ast> {
        let mut nodes = Vec::new();
        while let Some(token) = self.peek() {
            if let Token::Eof = token {
                break;
            }

            let node = self.match_token(token)?;
            nodes.push(node);
        }

        Ast::new(nodes).ok()
    }
}
