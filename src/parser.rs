use std::iter::Peekable;
use std::slice::Iter;

use crate::ast::{Argument, Ast, AstNode, Expression, Operator, ValueExpr, VarType};
use crate::error::{Error, Result};
use crate::lexer::LexItem;
use crate::position::Position;
use crate::token::Token;

pub struct Parser<'a> {
    tokens: Peekable<Iter<'a, LexItem<'a>>>,
    pos: Position,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [LexItem<'a>]) -> Self {
        Self {
            tokens: tokens.into_iter().peekable(),
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

    fn next_semi(&mut self) -> Result<()> {
        let Token::SemiColon = self.next_or_err()? else {
            Error::syntatic("expected semicolon `;`", self.pos)?
        };
        Ok(())
    }

    fn parse_value(&mut self) -> Result<ValueExpr> {
        let token = self.next_or_err()?;

        let expr = match *token {
            Token::Identifier(id) => ValueExpr::Identifier(id.into()),
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
                let args = self.parse_callargs()?;
                Expression::Func(id.into(), args, VarType::Int)
            }
            Token::Identifier(id) => Expression::identifier(id),
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

            lhs = Expression::BinOp(op, Box::new([lhs, rhs]));
        }

        Ok(lhs)
    }

    fn consume_expr(&mut self) -> Result<AstNode> {
        let expr = self.parse_expr(1)?;
        self.next_semi()?;
        let expr = AstNode::Expr(expr);
        Ok(expr)
    }

    fn consume_for(&mut self) -> Result<AstNode> {
        self.next(); // discard 'for' token
        let Token::Identifier(ident) = self.next_or_err()? else {
            Error::syntatic("expected identifier", self.pos)?
        };
        let Token::To = self.next_or_err()? else {
            Error::syntatic("expected token `to`", self.pos)?
        };
        let value = self.parse_value()?;
        let Token::Then = self.next_or_err()? else {
            Error::syntatic("expected token `then`", self.pos)?
        };
        let inner = self.consume_inner()?;
        AstNode::For((*ident).into(), value, inner).ok()
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
        let Token::Identifier(ident) = self.next_or_err()? else {
            Error::syntatic("expected identifier", self.pos)?
        };
        let Token::Assign = self.next_or_err()? else {
            Error::syntatic("expected assign operator `=`", self.pos)?
        };
        let expr = self.parse_expr(1)?;
        self.next_semi()?;
        AstNode::Var(ty, (*ident).into(), expr).ok()
    }

    fn parse_type(&mut self) -> Result<VarType> {
        let t = match self.next_or_err()? {
            Token::Int => VarType::Int,
            Token::Real => VarType::Real,
            Token::Char => VarType::Char,
            Token::Bool => VarType::Bool,
            _ => Error::syntatic("expected type annotation", self.pos)?,
        };
        Ok(t)
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
                    let Token::Colon = self.next_or_err()? else {
                        Error::syntatic("expected token `:`", self.pos)?
                    };
                    let arg_type = self.parse_type()?;
                    let arg = Argument::new(name, arg_type);
                    args.push(arg);
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

    fn consume_func(&mut self) -> Result<AstNode> {
        self.next(); // discard function token
        let Token::Identifier(name) = self.next_or_err()? else {
            Error::syntatic("expected name of the function", self.pos)?
        };
        let args = self.parse_args()?;
        let ret_type = match self.next_or_err()? {
            Token::Colon => {
                let ret_type = self.parse_type()?;
                let Token::Do = self.next_or_err()? else {
                    Error::syntatic("expected `do`", self.pos)?
                };
                Some(ret_type)
            }
            Token::Do => None,
            _ => Error::syntatic("expected type annotation or `do`", self.pos)?,
        };
        let inner = self.consume_inner()?;
        if let Some(..) = ret_type {
            if !matches!(inner.last(), Some(AstNode::Ret(..))) {
                Error::syntatic("expected a return statement for a typed funcion", self.pos)?;
            }
        }
        AstNode::Func((*name).into(), args, ret_type, inner).ok()
    }

    fn consume_ret(&mut self) -> Result<AstNode> {
        self.next(); // discard return token
        let expr = self.parse_expr(1)?;
        self.next_semi()?;
        let expr = AstNode::Ret(expr);
        Ok(expr)
    }

    fn consume_use(&mut self) -> Result<AstNode> {
        self.next(); // discard use token
        let Token::Identifier(ident) = self.next_or_err()? else {
            Error::syntatic("expected identifier", self.pos)?
        };
        self.next_semi()?;
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
