use std::iter::Peekable;
use std::slice::Iter;

use crate::ast::{Argument, Ast, AstNode, Expression, Operator, ValueExpr, VarType};
use crate::error::{Error, Result};
use crate::token::Token;

pub struct Parser<'a> {
    tokens: Peekable<Iter<'a, Token<'a>>>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token<'a>]) -> Self {
        Self {
            tokens: tokens.into_iter().peekable(),
        }
    }

    #[inline]
    fn next(&mut self) -> Option<&'a Token<'a>> {
        self.tokens.next()
    }

    #[inline]
    fn peek(&mut self) -> Option<&'a Token<'a>> {
        self.tokens.peek().map(|v| *v)
    }

    #[inline]
    fn next_or_err(&mut self) -> Result<&'a Token<'a>> {
        if let Some(token) = self.next() {
            Ok(token)
        } else {
            Error::syntatic("unexpected end of tokens")
        }
    }

    fn parse_value(&mut self) -> Result<ValueExpr> {
        let token = self.next_or_err()?;

        let expr = match token {
            Token::Identifier(i) => ValueExpr::Identifier((*i).into()),
            Token::StrLiteral(i) => ValueExpr::String((*i).into()),
            Token::IntLiteral(i) => ValueExpr::Int(*i),
            Token::FloatLiteral(i) => ValueExpr::Float(*i),
            _ => Error::syntatic("unexpected token")?,
        };

        Ok(expr)
    }

    fn parse_atom(&mut self) -> Result<Expression> {
        let token: &Token<'_> = self.next_or_err()?;

        let expr = match token {
            Token::Identifier(i) => Expression::Value(ValueExpr::Identifier((*i).into())),
            Token::StrLiteral(i) => Expression::Value(ValueExpr::String((*i).into())),
            Token::IntLiteral(i) => Expression::Value(ValueExpr::Int(*i)),
            Token::FloatLiteral(i) => Expression::Value(ValueExpr::Float(*i)),
            Token::Symbol('(') => {
                let inner_expr = self.parse_expr(1)?;
                let Token::Symbol(')') = self.next_or_err()? else {
                    Error::syntatic("expected close parentheses")?
                };
                inner_expr
            }
            _ => Error::syntatic("unexpected token")?,
        };

        Ok(expr)
    }

    fn next_operator(&mut self) -> Option<Operator> {
        if let Some(Token::Symbol(symb1)) = self.peek() {
            if Operator::is_valid_primary(symb1) {
                self.next();
            } else {
                return None;
            }

            if let Some(Token::Symbol(symb2)) = self.peek() {
                if let Ok(op) = Operator::try_from(&[*symb1, *symb2] as &[char]) {
                    self.next();
                    Some(op)
                } else {
                    Operator::try_from(&[*symb1] as &[char]).ok()
                }
            } else {
                Operator::try_from(&[*symb1] as &[char]).ok()
            }
        } else {
            None
        }
    }

    fn parse_expr(&mut self, min_prec: u8) -> Result<Expression> {
        let mut lhs = self.parse_atom()?;

        loop {
            let Some(op) = self.next_operator() else {
                break;
            };

            let prec = op.precedence();
            if prec < min_prec {
                break;
            }
            let min_prec = if op.is_right() { prec } else { prec + 1 };

            let rhs = self.parse_expr(min_prec)?;

            lhs = Expression::BinOp(op, Box::new([lhs, rhs]));
        }

        Ok(lhs)
    }

    fn consume_expr(&mut self) -> Result<AstNode> {
        let expr = self.parse_expr(1)?;
        let expr = AstNode::Expr(expr);
        Ok(expr)
    }

    fn consume_for(&mut self) -> Result<AstNode> {
        self.next(); // discard 'for' token
        let Token::Identifier(ident) = self.next_or_err()? else {
            Error::syntatic("expected identifier")?
        };
        let Token::To = self.next_or_err()? else {
            Error::syntatic("expected token `to`")?
        };
        let value = self.parse_value()?;
        let Token::Then = self.next_or_err()? else {
            Error::syntatic("expected token `then`")?
        };
        let inner = self.consume_inner()?;
        AstNode::For((*ident).into(), value, inner).ok()
    }

    fn consume_while(&mut self) -> Result<AstNode> {
        self.next(); // discard 'while' token
        let expr = self.parse_expr(1)?;
        let Token::Then = self.next_or_err()? else {
            Error::syntatic("expected `then`")?
        };
        let inner = self.consume_inner()?;
        AstNode::While(expr, inner).ok()
    }

    fn consume_if(&mut self) -> Result<AstNode> {
        self.next(); // discard 'if' token
        let expr = self.parse_expr(1)?;
        let Token::Then = self.next_or_err()? else {
            Error::syntatic("expected `then`")?
        };
        let inner = self.consume_inner()?;
        AstNode::If(expr, inner).ok()
    }

    fn consume_var(&mut self, ty: VarType) -> Result<AstNode> {
        self.next(); // discard var type token
        let Token::Identifier(ident) = self.next_or_err()? else {
            Error::syntatic("expected identifier")?
        };
        let Token::Symbol('=') = self.next_or_err()? else {
            Error::syntatic("expected assign operator `=`")?
        };
        let expr = self.parse_expr(1)?;
        AstNode::Var(ty, (*ident).into(), expr).ok()
    }

    fn parse_type(&mut self) -> Result<VarType> {
        let t = match self.next_or_err()? {
            Token::Int => VarType::Int,
            Token::Real => VarType::Real,
            Token::Char => VarType::Char,
            Token::Bool => VarType::Bool,
            _ => Error::syntatic("expected type annotation")?,
        };
        Ok(t)
    }

    fn parse_args(&mut self) -> Result<Vec<Argument>> {
        let Token::Symbol('(') = self.next_or_err()? else {
            Error::syntatic("expected open parenthesis `(`")?
        };
        let mut state = 1u8;
        let mut args = Vec::new();
        loop {
            match (state, self.next_or_err()?) {
                (1 | 2, Token::Symbol(')')) => break,
                (1 | 3, Token::Identifier(name)) => {
                    state = 2;
                    let Token::Symbol(':') = self.next_or_err()? else {
                        Error::syntatic("expected token `:`")?
                    };
                    let arg_type = self.parse_type()?;
                    let arg = Argument::new(name, arg_type);
                    args.push(arg);
                }
                (2, Token::Symbol(',')) => state = 3,
                (1 | 2, _) => Error::syntatic("expected close parenthesis `)` or argument")?,
                (3, _) => Error::syntatic("expected another argument after comma")?,
                _ => unreachable!("The state machine is out of control"),
            };
        }
        Ok(args)
    }

    fn consume_func(&mut self) -> Result<AstNode> {
        self.next(); // discard function token
        let Token::Identifier(name) = self.next_or_err()? else {
            Error::syntatic("expected name of the function")?
        };
        let args = self.parse_args()?;
        let ret_type = match self.next_or_err()? {
            Token::Symbol(':') => {
                let ret_type = self.parse_type()?;
                let Token::Do = self.next_or_err()? else {
                    Error::syntatic("expected `do`")?
                };
                Some(ret_type)
            }
            Token::Do => None,
            _ => Error::syntatic("expected type annotation or `do`")?,
        };
        let inner = self.consume_inner()?;
        if let Some(..) = ret_type {
            if !matches!(inner.last(), Some(AstNode::Ret(..))) {
                Error::syntatic("expected a return statement for a typed funcion")?;
            }
        }
        AstNode::Func((*name).into(), args, ret_type, inner).ok()
    }

    fn consume_ret(&mut self) -> Result<AstNode> {
        self.next(); // discard return token
        let expr = self.parse_expr(1)?;
        let expr = AstNode::Ret(expr);
        Ok(expr)
    }

    #[inline]
    fn match_token(&mut self, token: &Token<'_>) -> Result<AstNode> {
        match token {
            Token::If => self.consume_if(),
            Token::While => self.consume_while(),
            Token::For => self.consume_for(),

            Token::Int => self.consume_var(VarType::Int),
            Token::Real => self.consume_var(VarType::Real),
            Token::Char => self.consume_var(VarType::Char),
            Token::Bool => self.consume_var(VarType::Bool),

            Token::Function => self.consume_func(),
            Token::Return => self.consume_ret(),

            Token::Symbol('(')
            | Token::True
            | Token::False
            | Token::IntLiteral(..)
            | Token::FloatLiteral(..)
            | Token::StrLiteral(..)
            | Token::Identifier(..) => self.consume_expr(),

            Token::Eof | Token::End => panic!("token not allowed"),
            _ => Error::syntatic("wrong placement for this token"),
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
                return Error::syntatic("unexpected end of file");
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
