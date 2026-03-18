use std::path::PathBuf;

use crate::ast::{
    Ast, AstFuncType, AstNode, AstRoot, AstSymbol, AstType, Expression, Identifier, Literal,
};
use crate::error::{Error, PositionResult, Result};
use crate::source::Source;
use crate::token::{Token, TokenStream};

// At some point this was not opaque
pub struct Parser;

impl Parser {
    #[inline]
    fn next_or_err<'b>(&self, ts: &mut TokenStream<'b>) -> Result<Token<'b>> {
        if let Some(token) = ts.next() {
            Ok(token)
        } else {
            Error::syntatic("unexpected end of tokens", ts.pos())
        }
    }

    fn consume_semi(&self, ts: &mut TokenStream) -> Result<()> {
        let Token::SemiColon = self.next_or_err(ts)? else {
            Error::syntatic("expected semicolon `;`", ts.pos())?
        };
        Ok(())
    }

    fn parse_str(&self, ts: &TokenStream, s: &str) -> Result<Literal> {
        let mut chs = s.bytes().peekable();
        let mut acc = Vec::with_capacity(s.len());

        loop {
            let Some(curr) = chs.next() else {
                acc.push(b'\0');
                let escaped = String::from_utf8(acc).with_position(ts.pos())?;
                break Ok(Literal::String(escaped));
            };

            let b = match curr {
                b'\\' => match &[chs.next().unwrap()] {
                    b"n" => b'\n',
                    b"r" => b'\r',
                    b"t" => b'\t',
                    b"0" => b'\0',
                    b"\"" => b'"',
                    b"\\" => b'\\',
                    _ => return Error::syntatic("unknown escape sequence", ts.pos()),
                },
                _ => curr,
            };

            acc.push(b);
        }
    }

    fn parse_array(&self, ts: &mut TokenStream) -> Result<Vec<Literal>> {
        // open bracket should already be consumed
        let mut arr = Vec::new();
        loop {
            arr.push(self.parse_value(ts)?);
            if ts.next_if_eq(Token::Comma).is_some() {
                if ts.next_if_eq(Token::CloseBracket).is_some() {
                    break Ok(arr);
                }
            } else if ts.next_if_eq(Token::CloseBracket).is_some() {
                break Ok(arr);
            } else {
                break Error::syntatic("expected comma or close bracket", ts.pos());
            }
        }
    }

    fn parse_value(&self, ts: &mut TokenStream) -> Result<Literal> {
        let expr = match self.next_or_err(ts)? {
            Token::StrLiteral(string) => self.parse_str(ts, string)?,
            Token::IntLiteral(int) => Literal::Int(int as i32),
            Token::FloatLiteral(float) => Literal::Float(float),
            Token::Sub => match self.next_or_err(ts)? {
                Token::IntLiteral(int) => Literal::Int(-(int as i32)),
                Token::FloatLiteral(float) => Literal::Float(-float),
                _ => Error::syntatic("unexpected token", ts.pos())?,
            },
            Token::True => Literal::Bool(true),
            Token::False => Literal::Bool(false),
            Token::OpenBracket => {
                let arr = self.parse_array(ts)?;
                Literal::Array(arr)
            }
            _ => Error::syntatic("unexpected token", ts.pos())?,
        };

        Ok(expr)
    }

    fn parse_atom(&self, ts: &mut TokenStream) -> Result<Expression> {
        let expr = match self.next_or_err(ts)? {
            Token::StrLiteral(string) => Expression::Literal {
                lit: self.parse_str(ts, string)?,
                pos: ts.pos(),
            },
            Token::IntLiteral(int) => Expression::int(int as i32, ts.pos()),
            Token::FloatLiteral(float) => Expression::float(float, ts.pos()),
            Token::True => Expression::boolean(true, ts.pos()),
            Token::False => Expression::boolean(false, ts.pos()),
            Token::Identifier(ns) if matches!(ts.peek(), Some(Token::DuoColon)) => {
                ts.next(); // consume duo colon
                let ns = Identifier::new(ns, ts.pos());
                let Token::Identifier(member) = self.next_or_err(ts)? else {
                    Error::syntatic("expected namespace member", ts.pos())?
                };
                let member = Identifier::new(member, ts.pos());
                Expression::member(ns, member)
            }
            Token::Identifier(name) => Expression::id(name, ts.pos()),
            Token::OpenBracket => {
                let pos = ts.pos();
                let arr = self.parse_array(ts)?;
                Expression::array(arr, pos)
            }
            Token::OpenParen => {
                let inner_expr = self.parse_expr(ts, 1)?;
                let Token::CloseParen = self.next_or_err(ts)? else {
                    Error::syntatic("expected close parentheses", ts.pos())?
                };
                inner_expr
            }
            _ => Error::syntatic("unexpected token", ts.pos())?,
        };

        Ok(expr)
    }

    fn parse_expr(&self, ts: &mut TokenStream, min_prec: u8) -> Result<Expression> {
        let mut lhs = if let Some(op) = ts.peek_unaop() {
            self.next_or_err(ts)?;
            let prec = op.precedence();
            let operand = self.parse_expr(ts, prec)?;
            Expression::una_op(op, operand)
        } else if let Some(Token::SizeOf) = ts.peek() {
            ts.next();
            let pos = ts.pos();
            let expr = self.parse_expr(ts, 12)?;
            Expression::sizeof(expr, pos)
        } else if let Some(Token::Lambda) = ts.peek() {
            self.consume_lambda(ts)?
        } else {
            self.parse_atom(ts)?
        };

        loop {
            if let Some(op) = ts.peek_binop() {
                let prec = op.precedence();
                if prec < min_prec {
                    break;
                }
                let min_prec = if op.is_assign() { prec } else { prec + 1 };

                ts.next(); // consume operator
                let pos = ts.pos();

                let rhs = self.parse_expr(ts, min_prec)?;
                lhs = Expression::bin_op(op, lhs, rhs, pos);
            } else if let Some(Token::OpenParen) = ts.peek() {
                let args = self.parse_callargs(ts)?;
                lhs = Expression::call(lhs, args);
            } else if let Some(Token::Dot) = ts.peek() {
                ts.next(); // consume dot
                let Token::Identifier(member) = self.next_or_err(ts)? else {
                    Error::syntatic("expected member name after dot", ts.pos())?
                };
                lhs = Expression::field(lhs, member, ts.pos());
            } else if let Some(Token::OpenBracket) = ts.peek() {
                ts.next(); // consume open bracket
                let index = self.parse_expr(ts, 1)?;
                let Token::CloseBracket = self.next_or_err(ts)? else {
                    Error::syntatic("expected close bracket", ts.pos())?
                };
                lhs = Expression::index(lhs, index);
            } else if let Some(Token::As) = ts.peek() {
                if min_prec >= 12 {
                    break; // unary expressions have higher precedence than casts
                }
                ts.next(); // consume as token
                let target = self.parse_annot(ts)?;
                lhs = Expression::cast(lhs, target);
            } else {
                break;
            };
        }

        Ok(lhs)
    }

    fn consume_expr(&self, ts: &mut TokenStream) -> Result<AstNode> {
        let expr = self.parse_expr(ts, 1)?;
        self.consume_semi(ts)?;
        let expr = AstNode::Expr(expr);
        Ok(expr)
    }

    fn consume_for(&self, ts: &mut TokenStream) -> Result<AstNode> {
        ts.next(); // discard 'for' token
        let Token::Identifier(ident) = self.next_or_err(ts)? else {
            Error::syntatic("expected identifier", ts.pos())?
        };

        let init = if let Some(Token::Assign) = ts.peek() {
            self.next_or_err(ts)?;
            Some(self.parse_value(ts)?)
        } else {
            None
        };
        let Token::To = self.next_or_err(ts)? else {
            Error::syntatic("expected token `to`", ts.pos())?
        };
        let limit = self.parse_expr(ts, 1)?;
        let Token::Do = self.next_or_err(ts)? else {
            Error::syntatic("expected token `do`", ts.pos())?
        };
        let inner = self.consume_inner(ts)?;
        AstNode::For(ident.into(), init, limit, inner).ok()
    }

    fn consume_while(&self, ts: &mut TokenStream) -> Result<AstNode> {
        ts.next(); // discard 'while' token
        let expr = self.parse_expr(ts, 1)?;
        let Token::Do = self.next_or_err(ts)? else {
            Error::syntatic("expected `do`", ts.pos())?
        };
        let inner = self.consume_inner(ts)?;
        AstNode::While(expr, inner).ok()
    }

    fn consume_if(&self, ts: &mut TokenStream) -> Result<AstNode> {
        ts.next(); // discard 'if' token
        let expr = self.parse_expr(ts, 1)?;
        let Token::Then = self.next_or_err(ts)? else {
            Error::syntatic("expected `then`", ts.pos())?
        };
        let inner = self.consume_inner(ts)?;
        let else_branch = match ts.peek() {
            Some(Token::Else) => {
                ts.next(); // discard else
                let branch = match ts.peek() {
                    Some(Token::If) => self.consume_if(ts)?,
                    _ => AstNode::Else(self.consume_inner(ts)?),
                };
                Some(Box::new(branch))
            }
            _ => None,
        };
        AstNode::If(expr, inner, else_branch).ok()
    }

    fn parse_annot(&self, ts: &mut TokenStream) -> Result<AstType> {
        let mut base = match self.next_or_err(ts)? {
            Token::Int => AstType::Int,
            Token::Real => AstType::Real,
            Token::Char => AstType::Char,
            Token::Byte => AstType::Byte,
            Token::Bool => AstType::Bool,
            Token::Void => AstType::Void,
            Token::Function => {
                let Token::OpenParen = self.next_or_err(ts)? else {
                    Error::syntatic("expected open parenthesis", ts.pos())?
                };
                let mut args = Vec::new();
                let mut state = 1;
                let mut variadic = false;
                loop {
                    match (state, ts.peek()) {
                        (1 | 2 | 3, Some(Token::CloseParen)) => {
                            ts.next();
                            break;
                        }
                        (1, Some(Token::Ellipsis)) => {
                            ts.next();
                            variadic = true;
                            state = 3;
                        }
                        (1, _) => {
                            args.push(self.parse_annot(ts)?);
                            state = 2;
                        }
                        (2, Some(Token::Comma)) => {
                            ts.next();
                            state = 1;
                        }
                        (2, _) => {
                            let pos = ts.peek_pos();
                            Error::syntatic("expected comma or close parenthesis", pos)?
                        }
                        (3, _) => {
                            let pos = ts.peek_pos();
                            Error::syntatic("expected close parenthesis after variadic", pos)?
                        }
                        (_, _) => unreachable!(),
                    }
                }
                let ret = if let Some(Token::Colon) = ts.peek() {
                    ts.next();
                    self.parse_annot(ts)?.into()
                } else {
                    AstType::Void.into()
                };
                AstType::Func(AstFuncType::new(args, variadic, ret))
            }
            Token::Identifier(name) => {
                let mut segments = vec![name.into()];
                while let Some(Token::DuoColon) = ts.peek() {
                    ts.next();
                    let Token::Identifier(segment) = self.next_or_err(ts)? else {
                        Error::syntatic("expected identifier after `::`", ts.pos())?
                    };
                    segments.push(segment.into());
                }
                AstType::Named(segments)
            }
            Token::OpenParen => {
                let typ = self.parse_annot(ts)?;
                let Token::CloseParen = self.next_or_err(ts)? else {
                    Error::syntatic("expected close parenthesis", ts.pos())?
                };
                typ
            }
            _ => Error::syntatic("expected type annotation", ts.pos())?,
        };
        let ty = match ts.peek() {
            Some(Token::OpenBracket) => {
                self.next_or_err(ts)?; // discard open bracket
                let Literal::Int(size) = self.parse_value(ts)? else {
                    Error::syntatic("expected integer literal", ts.pos())?
                };
                let Token::CloseBracket = self.next_or_err(ts)? else {
                    Error::syntatic("expected close bracket", ts.pos())?
                };
                AstType::Array(Box::new(base), size as u32)
            }
            Some(Token::Mul) => {
                self.next_or_err(ts)?; // discard asterisk
                base = AstType::Pointer(Box::new(base));
                while let Some(Token::Mul) = ts.peek() {
                    self.next_or_err(ts)?; // discard asterisk
                    base = AstType::Pointer(Box::new(base));
                }
                base
            }
            _ => base,
        };
        Ok(ty)
    }

    fn consume_var(&self, ts: &mut TokenStream) -> Result<AstNode> {
        ts.next(); // consume var keyword
        let Token::Identifier(name) = self.next_or_err(ts)? else {
            Error::syntatic("expected identifier", ts.pos())?
        };
        if let Some(Token::Colon) = ts.peek() {
            ts.next();
            let typ = self.parse_annot(ts)?;
            if let Some(Token::Assign) = ts.peek() {
                ts.next();
                let expr = self.parse_expr(ts, 1)?;
                self.consume_semi(ts)?;
                AstNode::TypedVar(typ, name.into(), Some(expr)).ok()
            } else {
                self.consume_semi(ts)?;
                AstNode::TypedVar(typ, name.into(), None).ok()
            }
        } else if let Some(Token::Assign) = ts.peek() {
            ts.next();
            let expr = self.parse_expr(ts, 1)?;
            self.consume_semi(ts)?;
            AstNode::Var(name.into(), expr).ok()
        } else {
            Error::syntatic("expected type annotation or assignment", ts.pos())?
        }
    }

    fn consume_global(&mut self, ts: &mut TokenStream) -> Result<AstRoot> {
        ts.next(); // consume var keyword
        let Token::Identifier(name) = self.next_or_err(ts)? else {
            Error::syntatic("expected identifier", ts.pos())?
        };
        let id = Identifier::new(name, ts.pos());
        let Token::Colon = self.next_or_err(ts)? else {
            Error::syntatic("expected type annotation", ts.pos())?
        };
        let typ = self.parse_annot(ts)?;
        let expr = if let Some(Token::Assign) = ts.peek() {
            ts.next(); // discard assign signal
            let expr = self.parse_value(ts)?;
            Some(expr)
        } else {
            None
        };
        self.consume_semi(ts)?;
        AstRoot::Global(typ, id, expr).ok()
    }

    fn parse_args(&self, ts: &mut TokenStream) -> Result<(Vec<AstSymbol>, bool)> {
        let mut state = 1;
        let mut items = Vec::new();
        let mut variadic = false;
        loop {
            match (state, self.next_or_err(ts)?) {
                (1, Token::OpenParen) => state = 2,
                (2 | 4, Token::Identifier(name)) => {
                    state = 3;
                    let pos = ts.pos();
                    let Token::Colon = self.next_or_err(ts)? else {
                        Error::syntatic("expected token `:`", ts.pos())?
                    };
                    let arg_type = self.parse_annot(ts)?;
                    items.push(AstSymbol::new(name, arg_type, pos));
                }
                (3, Token::Comma) => state = 4,
                (2 | 4, Token::Ellipsis) => {
                    state = 6;
                    variadic = true;
                }
                (2 | 3 | 6, Token::CloseParen) => break,

                (1, _) => Error::syntatic("expected open parenthesis `(`", ts.pos())?,
                (4, _) => Error::syntatic("expected another argument after comma", ts.pos())?,
                (2 | 3 | 6, _) => Error::syntatic("expected close parenthesis `)`", ts.pos())?,

                _ => unreachable!("The state machine is out of control"),
            };
        }
        Ok((items, variadic))
    }

    fn parse_callargs(&self, ts: &mut TokenStream) -> Result<Vec<Expression>> {
        let Token::OpenParen = self.next_or_err(ts)? else {
            Error::syntatic("expected open parenthesis `(`", ts.pos())?
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
                    Error::syntatic("expected close parenthesis `)` or comma `,`", ts.pos())?
                }
                _ => unreachable!("The state machine is out of control"),
            };
        }
        Ok(args)
    }

    fn parse_fields(&self, ts: &mut TokenStream) -> Result<Vec<AstSymbol>> {
        let mut state = 1;
        let mut args = Vec::new();
        loop {
            match (state, self.next_or_err(ts)?) {
                (1, Token::OpenParen) => state = 2,
                (2 | 4, Token::Identifier(name)) => {
                    state = 3;
                    let pos = ts.pos();
                    let Token::Colon = self.next_or_err(ts)? else {
                        Error::syntatic("expected token `:`", ts.pos())?
                    };
                    let arg_type = self.parse_annot(ts)?;
                    args.push(AstSymbol::new(name, arg_type, pos));
                }
                (3, Token::Comma) => state = 4,
                (3, Token::CloseParen) => break,

                (1, _) => Error::syntatic("expected open parenthesis `(`", ts.pos())?,
                (2, _) => Error::syntatic("expected field declaration", ts.pos())?,
                (3, _) => Error::syntatic("expected close parenthesis `)`", ts.pos())?,
                (4, _) => Error::syntatic("expected another field after comma", ts.pos())?,

                _ => unreachable!("The state machine is out of control"),
            };
        }
        Ok(args)
    }

    fn consume_func(&mut self, ts: &mut TokenStream) -> Result<AstRoot> {
        ts.next(); // discard function token
        let Token::Identifier(name) = self.next_or_err(ts)? else {
            Error::syntatic("expected name of the function", ts.pos())?
        };
        let id = Identifier::new(name, ts.pos());
        let (args, variadic) = self.parse_args(ts)?;
        let annot = match self.next_or_err(ts)? {
            Token::Colon => {
                let ret_type = self.parse_annot(ts)?;
                let Token::Do = self.next_or_err(ts)? else {
                    Error::syntatic("expected `do`", ts.pos())?
                };
                ret_type
            }
            Token::Do => AstType::Void,
            _ => Error::syntatic("expected type annotation or `do`", ts.pos())?,
        };
        let inner = self.consume_inner(ts)?;
        AstRoot::Func(annot, id, args, variadic, inner).ok()
    }

    fn consume_lambda(&self, ts: &mut TokenStream) -> Result<Expression> {
        ts.next(); // consume lambda keyword
        let pos = ts.pos();
        let (args, variadic) = self.parse_args(ts)?;
        if variadic {
            return Error::syntatic("lambdas cannot be variadic", pos);
        }
        let typ = match self.next_or_err(ts)? {
            Token::Colon => {
                let ret_type = self.parse_annot(ts)?;
                let Token::Do = self.next_or_err(ts)? else {
                    Error::syntatic("expected `do`", ts.pos())?
                };
                ret_type
            }
            Token::Do => AstType::Void,
            _ => Error::syntatic("expected type annotation or `do`", ts.pos())?,
        };
        let body = self.consume_inner(ts)?;
        Ok(Expression::lambda(typ, pos, args, body))
    }

    fn consume_ret(&self, ts: &mut TokenStream) -> Result<AstNode> {
        ts.next(); // discard return token
        let expr = match ts.peek() {
            Some(Token::SemiColon) => None,
            _ => Some(self.parse_expr(ts, 1)?),
        };
        self.consume_semi(ts)?;
        AstNode::Ret(expr).ok()
    }

    fn consume_extern_func(&mut self, ts: &mut TokenStream) -> Result<AstRoot> {
        ts.next(); // discard func token
        let Token::Identifier(name) = self.next_or_err(ts)? else {
            return Error::syntatic("expected name of the function", ts.pos());
        };
        let id = Identifier::new(name, ts.pos());
        let (args, variadic) = self.parse_args(ts)?;
        let annot = if let Some(Token::Colon) = ts.peek() {
            ts.next(); // discard colon token
            self.parse_annot(ts)?
        } else {
            AstType::Void
        };
        self.consume_semi(ts)?;
        AstRoot::ExternFunc(annot, id, args, variadic).ok()
    }

    fn consume_extern_var(&mut self, ts: &mut TokenStream) -> Result<AstRoot> {
        ts.next(); // discard var token
        let Token::Identifier(name) = self.next_or_err(ts)? else {
            return Error::syntatic("expected name of the function", ts.pos());
        };
        let id = Identifier::new(name, ts.pos());
        let annot = if let Some(Token::Colon) = ts.peek() {
            ts.next(); // discard colon token
            self.parse_annot(ts)?
        } else {
            return Error::syntatic("expected type annotation for external var", ts.pos());
        };
        self.consume_semi(ts)?;
        AstRoot::ExternVar(annot, id).ok()
    }

    fn consume_ns(&mut self, ts: &mut TokenStream) -> Result<String> {
        ts.next(); // discard namespace token
        let Token::Identifier(id) = self.next_or_err(ts)? else {
            Error::syntatic("expected identifier", ts.pos())?
        };
        self.consume_semi(ts)?;
        Ok(String::from(id))
    }

    fn consume_use(&mut self, ts: &mut TokenStream) -> Result<String> {
        ts.next(); // discard use token
        let Token::Identifier(name) = self.next_or_err(ts)? else {
            Error::syntatic("expected file path", ts.pos())?
        };
        self.consume_semi(ts)?;
        Ok(name.into())
    }

    fn consume_import(&mut self, ts: &mut TokenStream) -> Result<PathBuf> {
        ts.next(); // discard use token
        let Token::StrLiteral(path) = self.next_or_err(ts)? else {
            Error::syntatic("expected path string", ts.pos())?
        };
        self.consume_semi(ts)?;
        Ok(PathBuf::from(path))
    }

    fn consume_struct(&mut self, ts: &mut TokenStream) -> Result<AstRoot> {
        ts.next(); // discard struct token
        let Token::Identifier(name) = self.next_or_err(ts)? else {
            return Error::syntatic("expected name of the struct", ts.pos());
        };
        let id = Identifier::new(name, ts.pos());
        let fields = self.parse_fields(ts)?;
        self.consume_semi(ts)?;
        AstRoot::Struct(id, fields).ok()
    }

    fn consume_inner(&self, ts: &mut TokenStream) -> Result<Vec<AstNode>> {
        let mut nodes = Vec::new();
        while let Some(token) = ts.peek() {
            if let Token::End = token {
                ts.next();
                break;
            } else if let Token::Else = token {
                // do not consume the token
                break;
            }

            if let Token::Eof = token {
                return Error::syntatic("unexpected end of file", ts.pos());
            }

            let node = match token {
                Token::If => self.consume_if(ts),
                Token::While => self.consume_while(ts),
                Token::For => self.consume_for(ts),

                Token::Var => self.consume_var(ts),

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
        Ok(nodes)
    }

    fn consume_root(&mut self, ts: &mut TokenStream) -> Result<Ast> {
        let mut ast = Ast::new("global");

        while let Some(token) = ts.peek() {
            if let Token::Eof = token {
                break;
            }

            if let Token::Namespace = token {
                if ast.namespace.as_str() != "global" {
                    Error::syntatic("cannot declare the namespace more than once", ts.pos())?;
                }
                ast.namespace = self.consume_ns(ts)?.into();
                continue;
            }

            if let Token::Import = token {
                let import = self.consume_import(ts)?;
                ast.imports.push(import);
                continue;
            }

            if let Token::Use = token {
                let namespace = self.consume_use(ts)?;
                ast.uses.push(namespace);
                continue;
            }

            let node = match token {
                Token::Struct => self.consume_struct(ts),
                Token::Var => self.consume_global(ts),
                Token::Function => self.consume_func(ts),
                Token::Extern => {
                    ts.next(); // discard extern
                    match ts.peek() {
                        Some(Token::Function) => self.consume_extern_func(ts),
                        Some(Token::Var) => self.consume_extern_var(ts),
                        _ => Error::syntatic("expected func or var keyword", ts.peek_pos()),
                    }
                }

                Token::Eof | Token::Namespace | Token::Import | Token::Use => unreachable!(),
                _ => Error::syntatic("wrong placement for this token", ts.peek_pos()),
            }?;

            ast.nodes.push(node);
        }

        Ok(ast)
    }

    pub fn parse(source: Source) -> Result<Ast> {
        Self.consume_root(&mut source.tokenize()?)
    }
}
