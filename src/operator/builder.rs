use pest::iterators::Pair;
use pest::Parser;

use crate::{error::ErrorType, parser::KelpParser};
use crate::{
    ast::{Expr, AST},
    error::Error,
    operator::{Associativity, Operator},
    parser::Rule,
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

#[derive(Default, Debug)]
pub struct OperatorBuilder {
    operator_defs: Vec<Expr>,
    had_error: bool,
}

impl OperatorBuilder {
    fn parse_assoc(&mut self, pair: Pair<Rule>) -> Result<Associativity, Error> {
        Ok(if let Some(expr) = pair.into_inner().next() {
            match expr.as_str() {
                "left" => Associativity::Left,
                "neutral" => Associativity::Neutral,
                "right" => Associativity::Right,
                &_ => {
                    return Err(Error::default()
                        .with_message("invalid association type".to_string())
                        .with_type(ErrorType::OperatorDefinitionError)
                        .build())
                }
            }
        } else {
            return Err(Error::default()
                .with_message("operator definition entry must have a child".to_string())
                .with_type(ErrorType::OperatorDefinitionError)
                .build());
        })
    }
    fn parse_bod(pair: Pair<Rule>) -> Result<String, Error> {
        Ok(if let Some(expr) = pair.into_inner().next() {
            expr.as_str().to_string()
        } else {
            return Err(Error::default()
                .with_message("operator definition entry must have a child".to_string())
                .with_type(ErrorType::OperatorDefinitionError)
                .build());
        })
    }
    fn parse_op(&mut self, pair: Pair<Rule>) -> Result<OperatorDefExpr, Error> {
        let mut pairs = pair.into_inner();
        let op_def = pairs.next().unwrap();
        let op = match op_def.as_rule() {
            Rule::operator_def => op_def.to_string(),
            Rule::sym => {
                return Ok(OperatorDefExpr::Assoc(self.parse_assoc(
                    op_def,
                )?))
            }
            _ => return Err(Error::default()),
        };
        if pairs.next().unwrap().to_string() != "|" {
            return Err(Error::default());
        }
        let bod = pairs.next().unwrap();
        let key = match bod.as_rule() {
            Rule::fun_bod => OperatorBuilder::parse_bod(bod)?,
            _ => return Err(Error::default()),
        };
        Ok(match key.to_lowercase().as_str() {
            "same" => OperatorDefExpr::Same(op),
            "after" => OperatorDefExpr::After(op),
            "before" => OperatorDefExpr::Before(op),
            &_ => return Err(Error::default()),
        })
    }

    fn parse_expr(&mut self, expr: &Expr) -> Result<OperatorDefExpr, Error> {
        Ok(if let Expr::UnresolvedOp(unresolved) = expr {
            let pair = parse_unwrap!(KelpParser::parse(Rule::op, &unresolved)?.next(), "missing op members, in operator definition", self);
            match pair.as_rule() {
                Rule::op => self.parse_op(pair.clone())?,
                _ => return Err(Error::default()),
            }
        } else {
            return Err(Error::default()
                .with_message("expression is not an operation".to_string())
                .with_type(ErrorType::ParsingError)
                .build());
        })
    }

    fn build_operator_def(&mut self, exprs: &Vec<Expr>, op: Operator) -> Result<OperatorDef, Error> {
        let def_exprs = exprs
            .iter()
            .map(|expr| self.parse_expr(&expr))
            .collect::<Result<Vec<_>, _>>()?;

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
                .build());
        } else if assoc_count != 1 {
            return Err(Error::default()
                .with_message(
                    "invalid operator definition, there must be one (and only) assoc".to_string(),
                )
                .build());
        } else if same_count == 1 && (after_count + before_count) > 0 {
            return Err(Error::default()
                .with_message(
                    "invalid operator definition, operator can either be same as, or before/after"
                        .to_string(),
                )
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
    ) -> Result<OperatorDef, Error> {
        Ok(match name {
            Expr::Sym(name) => {
                if name == "op" {
                    self.build_operator_def(exprs, op)?
                } else {
                    return Err(Error::default()
                    .with_type(ErrorType::OperatorDefinitionError)
                    .with_message(format!(
                        "{} is invalid at this poin, only \"op\" is allowed",
                        name
                    ))
                    .build())
                }
            }
            _ => {
                return Err(Error::default()
                    .with_type(ErrorType::OperatorDefinitionError)
                    .with_message("invalid token".to_string())
                    .build())
            }
        })
    }

    fn parse_operator_def(
        &mut self,
        expr: &Expr,
        op: Operator,
    ) -> Result<OperatorDef, Error> {
        Ok(match expr {
            Expr::FunBlk(name, exprs) => self.parse_fun_blk(&name, exprs, op)?,
            _ => return Err(Error::default()),
        })
    }

    pub fn build(&mut self, ast: AST) -> Result<Vec<OperatorDef>, Error> {
        Ok(match ast {
            Expr::Root(children) => children
                .iter()
                .filter_map(|def_op| match def_op {
                    Expr::DefOp(op, expr) => Some(self.parse_operator_def(
                        &expr,
                        op.to_string(),
                    )),
                    _ => None,
                })
                .collect::<Result<_, _>>()?,
            _ => {
                return Err(Error::default()
                    .with_message(
                        "non-root operator definitions are currently not supported".to_string(),
                    )
                    .with_type(ErrorType::UnsupportedError)
                    .build())
            }
        })
    }
}
