use kelp_message::{Error, MessageDispatcher, MessageLevel};

use crate::*;
use std::cell::RefCell;
use std::rc::Rc;

pub struct Composer {
    message_dispatcher: MessageDispatcher,
    parse_tree: kelp_parser::Expr,
}

impl Composer {
    pub fn new(message_dispatcher: &MessageDispatcher, parse_tree: kelp_parser::Expr) -> Self {
        Self {
            message_dispatcher: message_dispatcher.clone(),
            parse_tree,
        }
    }

    pub fn compose(&mut self) -> Result<Vec<Lambda>, Error> {
        let stree = 3;
        todo!();
    }

    fn compose_fun_signature(
        &mut self,
        typ: kelp_parser::Typ,
    ) -> Result<(Vec<Variable>, Type), Error> {
        todo!();
    }

    fn compose_type(&mut self, typ: kelp_parser::Typ) -> Result<Type, Error> {
        todo!()
    }

    fn compose_group(&mut self, expr: kelp_parser::Expr) -> Result<Vec<Expr>, Error> {
        todo!()
    }

    fn compose_literal(&mut self, expr: kelp_parser::Expr) -> Result<Literal, Error> {
        todo!()
    }

    fn compose_variable(&mut self, expr: kelp_parser::Expr) -> Result<Variable, Error> {
        let span = expr.span();
        let mut is_corrupted = false;
        let kind = match expr.kind() {
            kelp_parser::ExprKind::Expression(_, _, _) => match self.compose_expr(expr) {
                Ok(t) => VariableKind::Body(t.0),
                Err(e) => {
                    self.message_dispatcher.dispatch(e, MessageLevel::Error);
                    is_corrupted = true;
                    VariableKind::Corrupted
                }
            },
            kelp_parser::ExprKind::Lambda(_, _) => match self.compose_lambda(expr) {
                Ok(l) => VariableKind::Lambda(l),
                Err(e) => {
                    self.message_dispatcher.dispatch(e, MessageLevel::Error);
                    is_corrupted = true;
                    VariableKind::Corrupted
                }
            },
            kelp_parser::ExprKind::Sym(s) => VariableKind::Symbol(s),
            kelp_parser::ExprKind::SymTyp(sym, typ) => match self.compose_type(typ) {
                Ok(t) => VariableKind::SymbolType(sym, t),
                Err(e) => {
                    self.message_dispatcher.dispatch(e, MessageLevel::Error);
                    is_corrupted = true;
                    VariableKind::Corrupted
                }
            },
            kelp_parser::ExprKind::Group(_) => match self.compose_group(expr) {
                Ok(g) => VariableKind::Group(g),
                Err(e) => {
                    self.message_dispatcher.dispatch(e, MessageLevel::Error);
                    is_corrupted = true;
                    VariableKind::Corrupted
                }
            },
            kelp_parser::ExprKind::Lit(_) => match self.compose_literal(expr) {
                Ok(l) => VariableKind::Literal(l),
                Err(e) => {
                    self.message_dispatcher.dispatch(e, MessageLevel::Error);
                    is_corrupted = true;
                    VariableKind::Corrupted
                }
            },
            kelp_parser::ExprKind::Body(_) => match self.compose_body(expr) {
                Ok(e) => VariableKind::Body(e),
                Err(e) => {
                    self.message_dispatcher.dispatch(e, MessageLevel::Error);
                    is_corrupted = true;
                    VariableKind::Corrupted
                }
            },
            kelp_parser::ExprKind::Object(_) => match self.compose_object(expr) {
                Ok(o) => VariableKind::Object(o),
                Err(e) => {
                    self.message_dispatcher.dispatch(e, MessageLevel::Error);
                    is_corrupted = true;
                    VariableKind::Corrupted
                }
            },
            kelp_parser::ExprKind::Operator(o) => VariableKind::Symbol(o),
            kelp_parser::ExprKind::Corrupted => {
                self.message_dispatcher
                    .dispatch(Error::default(), MessageLevel::Error);
                VariableKind::Corrupted
            }
            _ => return Err(Error::default()),
        };

        Ok(Variable {
            kind,
            span,
            is_corrupted,
        })
    }

    fn compose_expr(
        &mut self,
        expr: kelp_parser::Expr,
    ) -> Result<(Vec<Expr>, Variable, Variable), Error> {
        todo!("This won't work, flattening is a bad idea you dumb ass stupid ass human");
        match expr.kind() {
            kelp_parser::ExprKind::Expression(lhs, op, rhs) => {
                let mut exprs: Vec<Expr> = vec![];
                let (mut new_lhs, mut new_op, mut new_rhs): (Variable, Variable, Variable);
                if let kelp_parser::ExprKind::Expression(_, _, _) = lhs.kind() {
                    let (expr_children, child_lhs, _child_rhs) = self.compose_expr(lhs.clone())?;
                    new_lhs = child_lhs;
                    exprs = expr_children;
                } else {
                    new_lhs = self.compose_variable(lhs.clone())?;
                }

                new_op = if let kelp_parser::ExprKind::Operator(op_string) = op.kind() {
                    Variable {
                        kind: VariableKind::Symbol(op_string),
                        span: op.span(),
                        is_corrupted: false,
                    }
                } else {
                    return Err(Error::default());
                };

                if let kelp_parser::ExprKind::Expression(_, _, _) = rhs.kind() {
                    let (expr_children, _child_lhs, child_rhs) = self.compose_expr(rhs)?;

                    new_rhs = child_rhs;
                    exprs.push(Expr {
                        lhs: new_lhs.clone(),
                        operator: Some(new_op),
                        rhs: Some(new_rhs.clone()),
                        span: expr.span(),
                        is_corrupted: false,
                    });
                    exprs.extend(expr_children);
                } else {
                    new_rhs = self.compose_variable(rhs)?;
                }

                Ok((exprs, new_lhs, new_rhs))
            }
            _ => Err(Error::default()),
        }
    }

    fn compose_body(&mut self, body: kelp_parser::Expr) -> Result<Vec<Expr>, Error> {
        match body.kind() {
            kelp_parser::ExprKind::Body(body) => {
                let mut has_errored = false;
                let body = body
                    .iter()
                    .map(|expr| self.compose_expr(expr.clone()))
                    .collect::<Vec<Result<(Vec<Expr>, Variable, Variable), Error>>>();
                Ok(body
                    .iter()
                    .filter_map(|expr| match expr {
                        Ok(T) => Some(T.0.clone()),
                        Err(e) => {
                            has_errored = true;
                            self.message_dispatcher.dispatch(e, MessageLevel::Error);
                            None
                        }
                    })
                    .flatten()
                    .collect())
            }
            _ => Err(Error::default()),
        }
    }

    fn compose_object(&mut self, expr: kelp_parser::Expr) -> Result<Object, Error> {
        let mut has_errored = false;
        match expr.kind() {
            kelp_parser::ExprKind::Object(body) => {
                let body = match self.compose_body(body) {
                    Ok(body) => body,
                    Err(e) => {
                        self.message_dispatcher.dispatch(e, MessageLevel::Error);
                        has_errored = true;
                        vec![]
                    }
                };
                Ok(Object {
                    body,
                    span: expr.span(),
                    is_corrupted: has_errored,
                })
            }
            _ => Err(Error::default()),
        }
    }

    fn compose_lambda(&mut self, expr: kelp_parser::Expr) -> Result<Lambda, Error> {
        match expr.kind() {
            kelp_parser::ExprKind::Lambda(typ, body) => {
                let mut has_errored = false;
                let (args, ret) = self.compose_fun_signature(typ)?;

                let body = match self.compose_body(body) {
                    Ok(body) => body,
                    Err(e) => {
                        self.message_dispatcher.dispatch(e, MessageLevel::Error);
                        has_errored = true;
                        vec![]
                    }
                };
                let span = expr.span();

                Ok(Lambda {
                    args,
                    ret,
                    body,
                    span,
                    is_corrupted: has_errored,
                })
            }
            _ => Err(Error::default()),
        }
    }
}
