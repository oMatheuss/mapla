use crate::ast::{self, Annotated};

#[derive(Debug, Clone)]
pub enum IrExprOpe {
    Value {
        value: ast::ValueExpr,
    },
    UnaOp {
        operator: ast::UnaryOperator,
        operand: usize,
    },
    BinOp {
        operator: ast::Operator,
        lhs: usize,
        rhs: usize,
        annot: ast::TypeAnnot,
    },
    Func {
        name: ast::Identifier,
        args: Vec<(usize, ast::TypeAnnot)>,
    },
    Index {
        array: ast::ValueExpr,
        index: usize,
    },
    Cast {
        value: usize,
        cast_from: ast::TypeAnnot,
    },
    Alloc {
        args: Vec<(usize, ast::TypeAnnot)>,
    },
}

#[derive(Debug, Clone)]
pub struct IrExpr {
    pub id: usize,
    pub annot: ast::TypeAnnot,
    pub ope: IrExprOpe,
    pub assign: bool,
}

impl IrExpr {
    fn new(id: usize, annot: ast::TypeAnnot, ope: IrExprOpe, assign: bool) -> Self {
        IrExpr {
            id,
            annot,
            ope,
            assign,
        }
    }

    pub fn from_expr(expr: &ast::Expression) -> Vec<IrExpr> {
        let mut ir_exprs = Vec::new();
        parse_expr(expr, &mut ir_exprs, 0, false);
        ir_exprs
    }
}

fn parse_expr(expr: &ast::Expression, exprs: &mut Vec<IrExpr>, max: usize, assign: bool) {
    use ast::Expression::*;

    let id = max;
    let expr = match expr {
        Value(value) => {
            let ope = IrExprOpe::Value {
                value: value.clone(),
            };
            IrExpr::new(id, value.get_annot(), ope, assign)
        }
        UnaOp(unary) => {
            parse_expr(unary.operand(), exprs, max + 1, false);
            let ope = IrExprOpe::UnaOp {
                operator: unary.operator(),
                operand: max + 1,
            };
            IrExpr::new(id, unary.get_annot(), ope, false)
        }
        BinOp(binary) => {
            let operator = binary.operator();
            parse_expr(binary.rhs(), exprs, max + 1, false);
            parse_expr(binary.lhs(), exprs, max + 2, operator.is_assign());
            let ope = IrExprOpe::BinOp {
                operator,
                lhs: max + 2,
                rhs: max + 1,
                annot: binary.lhs().get_annot(),
            };
            IrExpr::new(id, binary.get_annot(), ope, false)
        }
        Func(call) => {
            let mut args = Vec::new();
            let mut max = max;
            for arg in call.args() {
                max += 1;
                parse_expr(arg, exprs, max, false);
                args.push((max, arg.get_annot()));
            }
            let ope = IrExprOpe::Func {
                name: call.name().into(),
                args,
            };
            IrExpr::new(id, call.get_annot(), ope, false)
        }
        Index(index) => {
            parse_expr(index.index(), exprs, max + 1, false);
            let ope = IrExprOpe::Index {
                array: index.array().clone(),
                index: max + 1,
            };
            IrExpr::new(id, index.get_annot(), ope, assign)
        }
        Cast(cast) => {
            let value = cast.value();
            parse_expr(value, exprs, max + 1, false);
            let ope = IrExprOpe::Cast {
                value: max + 1,
                cast_from: value.get_annot(),
            };
            IrExpr::new(id, cast.get_annot(), ope, false)
        }
        Alloc(alloc) => {
            let mut args = Vec::new();
            let mut max = max;
            for arg in alloc.args() {
                max += 1;
                parse_expr(arg, exprs, max, false);
                args.push((max, arg.get_annot()));
            }
            let ope = IrExprOpe::Alloc { args };
            IrExpr::new(id, alloc.get_annot(), ope, false)
        }
    };

    exprs.push(expr);
}
