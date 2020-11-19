use crate::ast::Span;
use pest::iterators::Pair;
use pest::Parser;

use crate::{
    ast::{Expr, ExprKind},
    message::{Error, ErrorType, MessageDispatcher},
    operator::{Associativity, Operator},
    parser::{KelpParser, Rule},
};

pub enum OperatorDef {
    Operator {
        targets_up: Vec<Operator>,
        targets_down: Vec<Operator>,
        assoc: Associativity,
        operator: Operator,
    },
    Layer {
        target: Operator,
        assoc: Associativity,
        operator: Operator,
    },
}

enum OperatorDefExpr {
    After(Operator),
    Before(Operator),
    Same(Operator),
    Assoc(Associativity),
}

#[derive(Debug, Clone)]
pub struct OperatorBuilder {
    operator_defs: Vec<Expr>,
    msg_dispatcher: MessageDispatcher,
}

impl OperatorBuilder {
    fn parse_assoc(&mut self, pair: Pair<Rule>) -> Result<Associativity, Error> {
        let span = pair.as_span().into();
        Ok(if let Some(expr) = pair.into_inner().next() {
            match expr.as_str() {
                "left" => Associativity::Left,
                "neutral" => Associativity::Neutral,
                "right" => Associativity::Right,
                &_ => {
                    return Err(Error::default()
                        .with_message("invalid association type".to_string())
                        .with_type(ErrorType::OperatorDefinitionError)
                        .with_span(expr.as_span().into())
                        .build())
                }
            }
        } else {
            return Err(Error::default()
                .with_message("operator definition entry must have a child".to_string())
                .with_type(ErrorType::OperatorDefinitionError)
                .with_span(span)
                .build());
        })
    }
    fn parse_bod(pair: Pair<Rule>) -> Result<String, Error> {
        let span = pair.as_span().into();
        Ok(if let Some(expr) = pair.into_inner().next() {
            expr.as_str().to_string()
        } else {
            return Err(Error::default()
                .with_message("operator definition entry must have a child".to_string())
                .with_type(ErrorType::OperatorDefinitionError)
                .with_span(span)
                .build());
        })
    }
    fn parse_op(&mut self, pair: Pair<Rule>) -> Result<OperatorDefExpr, Error> {
        let mut pairs = pair.into_inner();
        let op_def = pairs.next().unwrap();
        let op = match op_def.as_rule() {
            Rule::operator_def => op_def.to_string(),
            Rule::sym => return Ok(OperatorDefExpr::Assoc(self.parse_assoc(op_def)?)),
            _ => {
                return Err(Error::default()
                    .with_message("invalid token at this point".to_string())
                    .with_type(ErrorType::ParsingError)
                    .with_span(op_def.as_span().into())
                    .build())
            }
        };
        let pipe = pairs.next().unwrap();
        if pipe.as_str() != "|" {
            return Err(Error::default()
                .with_message(format!(
                    "invalid operator, expected: \"|\" found: \"{}\"",
                    pipe.as_str()
                ))
                .with_type(ErrorType::ParsingError)
                .with_span(pipe.as_span().into())
                .build());
        }
        let bod = pairs.next().unwrap();
        let bod_span = bod.as_span().into();
        let key = match bod.as_rule() {
            Rule::fun_bod => OperatorBuilder::parse_bod(bod)?,
            Rule::sym => bod.as_str().to_string(),
            _ => {
                return Err(Error::default()
                    .with_message(
                        "in op block, only function bodies or symbols are allowed".to_string(),
                    )
                    .with_type(ErrorType::ParsingError)
                    .with_span(bod_span)
                    .build())
            }
        };
        Ok(match key.to_lowercase().as_str() {
            "same" => OperatorDefExpr::Same(op),
            "after" => OperatorDefExpr::After(op),
            "before" => OperatorDefExpr::Before(op),
            &_ => {
                return Err(Error::default()
                    .with_message(
                        "invalid keyword at this point, only same or after/before are allowed"
                            .to_string(),
                    )
                    .with_type(ErrorType::ParsingError)
                    .with_span(bod_span)
                    .build())
            }
        })
    }

    fn parse_expr(&mut self, expr: &Expr) -> Result<OperatorDefExpr, Error> {
        let span = expr.span();
        Ok(if let ExprKind::UnresolvedOp(unresolved) = expr.kind() {
            let pair = parse_unwrap!(
                KelpParser::parse(Rule::op, &unresolved)?.next(),
                "missing op members, in operator definition",
                span
            );
            match pair.as_rule() {
                Rule::op => self.parse_op(pair.clone())?,
                _ => return Err(Error::default()),
            }
        } else {
            return Err(Error::default()
                .with_message("expression is not an operation".to_string())
                .with_type(ErrorType::ParsingError)
                .with_span(span)
                .build());
        })
    }

    fn build_operator_def(
        &mut self,
        exprs: &Vec<Expr>,
        op: Operator,
        span: Span,
    ) -> Result<OperatorDef, Error> {
        let def_exprs: Vec<OperatorDefExpr> = exprs
            .iter()
            .filter_map(|expr| match self.parse_expr(&expr) {
                Ok(op_def_expr) => Some(op_def_expr),
                Err(e) => {
                    self.msg_dispatcher.dispatch(e);
                    None
                }
            })
            .collect();

        let (same, assoc, after, before): (
            Vec<Operator>,
            Vec<Associativity>,
            Vec<Operator>,
            Vec<Operator>,
        ) = {
            (
                def_exprs
                    .iter()
                    .filter_map(|expr| {
                        if let OperatorDefExpr::Same(same) = expr {
                            Some(same.clone())
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<_>>(),
                def_exprs
                    .iter()
                    .filter_map(|expr| {
                        if let OperatorDefExpr::Assoc(assoc) = expr {
                            Some(assoc.clone())
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<_>>(),
                def_exprs
                    .iter()
                    .filter_map(|expr| {
                        if let OperatorDefExpr::After(after) = expr {
                            Some(after.clone())
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<_>>(),
                def_exprs
                    .iter()
                    .filter_map(|expr| {
                        if let OperatorDefExpr::Before(before) = expr {
                            Some(before.clone())
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<_>>(),
            )
        };

        let (same_count, assoc_count, after_count, before_count) =
            (same.len(), assoc.len(), after.len(), before.len());

        if same_count > 1 {
            return Err(Error::default()
                .with_message("invalid operator definition, there can be only one same".to_string())
                .with_type(ErrorType::OperatorDefinitionError)
                .with_span(span)
                .build());
        } else if assoc_count != 1 {
            return Err(Error::default()
                .with_message(
                    "invalid operator definition, there must be one (and only) assoc".to_string(),
                )
                .with_type(ErrorType::OperatorDefinitionError)
                .with_span(span)
                .build());
        } else if same_count == 1 && (after_count + before_count) > 0 {
            return Err(Error::default()
                .with_message(
                    "invalid operator definition, operator can either be same as, or before/after"
                        .to_string(),
                )
                .with_type(ErrorType::OperatorDefinitionError)
                .with_span(span)
                .build());
        }

        Ok(if same_count == 1 {
            OperatorDef::Layer {
                target: same.first().unwrap().clone(),
                assoc: assoc.first().unwrap().clone(),
                operator: op,
            }
        } else {
            OperatorDef::Operator {
                targets_up: after,
                targets_down: before,
                assoc: assoc.first().unwrap().clone(),
                operator: op,
            }
        })
    }

    fn parse_fun_blk(
        &mut self,
        name: &Expr,
        exprs: &Vec<Expr>,
        op: Operator,
        span: Span,
    ) -> Result<OperatorDef, Error> {
        Ok(match name.kind() {
            ExprKind::Sym(name_str) => {
                if name_str == "op" {
                    self.build_operator_def(exprs, op, span)?
                } else {
                    return Err(Error::default()
                        .with_type(ErrorType::OperatorDefinitionError)
                        .with_message(format!(
                            "{} is invalid at this poin, only \"op\" is allowed",
                            name_str
                        ))
                        .with_span(name.span())
                        .build());
                }
            }
            _ => {
                return Err(Error::default()
                    .with_type(ErrorType::OperatorDefinitionError)
                    .with_message("invalid token".to_string())
                    .with_span(name.span())
                    .build())
            }
        })
    }

    fn parse_operator_def(
        &mut self,
        op_to_def: &Expr,
        op: &Expr,
        val: &Expr,
    ) -> Result<Option<OperatorDef>, Error> {
        Ok(match (op_to_def.kind(), op.kind(), val.kind()) {
            (
                ExprKind::Operator(op_to_def),
                ExprKind::Operator(op_str),
                ExprKind::FunBlk(name, exprs),
            ) => match op_str.as_str() {
                "=" => {
                    Some(self.parse_fun_blk(&name, &exprs, op_to_def.to_string(), val.span())?)
                }
                &_ => None,
            },
            _ => None,
        })
    }

    pub fn build(ast: Expr, msg_dispatcher: MessageDispatcher) -> Result<Vec<OperatorDef>, Error> {
        let mut slf = Self {
            operator_defs: vec![],
            msg_dispatcher,
        };
        Ok(match ast.kind() {
            ExprKind::Body(children) => children
                .iter()
                .filter_map(|def_op| match def_op.kind() {
                    ExprKind::Def(op_to_def, op, val) => {
                        match slf.parse_operator_def(&op_to_def, &op, &val) {
                            Ok(op_def) => op_def,
                            Err(e) => {
                                slf.msg_dispatcher.dispatch(e);
                                None
                            }
                        }
                    }
                    _ => None,
                })
                .collect(),
            _ => {
                return Err(Error::default()
                    .with_message(
                        "non-root operator definitions are currently not supported".to_string(), // and maybe never will be
                    )
                    .with_type(ErrorType::UnsupportedError)
                    .build());
            }
        })
    }
}
