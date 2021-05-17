use crate::parser::*;
use pest::iterators::Pair;

use crate::{
    corrupt_expr, corrupt_typ,
    parse_tree::{Expr, ExprKind, Typ},
    parse_unwrap,
};

use kelp_message::*;
use kelp_span::Span;

use super::Literal;
use kelp_origin::Origin;

#[derive(Debug, Clone)]
pub struct ParseTree {
    ast: Expr,
    message_dispatcher: MessageDispatcher,
    origin: Origin,
}

impl ParseTree {
    pub fn new(message_dispatcher: &MessageDispatcher, origin: Origin) -> Self {
        Self {
            ast: Expr::corrupted(),
            message_dispatcher: message_dispatcher.clone(),
            origin,
        }
    }

    pub fn build(&mut self) -> Self {
        let ast = std::mem::take(self);
        ParseTree {
            ast: ast.ast,
            message_dispatcher: ast.message_dispatcher,
            origin: ast.origin,
        }
    }

    pub fn get_parse_tree(&self) -> Expr {
        self.ast.clone()
    }

    pub fn build_parse_tree(&mut self, pair: Pair<Rule>) -> Result<&mut Self, Error> {
        let span = (pair.as_span(), self.origin.clone()).into();
        let body = pair
            .into_inner()
            .map(|pair| corrupt_expr!(self.build_expr(pair), self.message_dispatcher))
            .collect::<Vec<Expr>>();
        self.ast = Expr::new(ExprKind::Body(body), span);
        Ok(self)
    }

    fn build_literal(&mut self, pair: Pair<Rule>) -> Result<Literal, Error> {
        Ok(match pair.as_rule() {
            Rule::int => Literal::Int(pair.as_str().trim().parse::<i32>()?),
            Rule::rea => Literal::Real(pair.as_str().trim().parse::<f32>()?),
            Rule::stg => Literal::Str(
                /*unescape_str(pair.as_str())*/ pair.as_str().to_string(),
            ),
            Rule::chr => Literal::Char(
                pair.as_str()
                    .chars()
                    .next()
                    .expect("char definition is empty"),
            ),
            _ => {
                return Err(Error::default()
                    .with_type(ErrorType::ParsingError)
                    .with_span((pair.as_span(), self.origin.clone()).into())
                    .with_message(format!(
                        "wrong rule {} when parsing a literal {}",
                        pair,
                        pair.as_str()
                    ))
                    .build())
            }
        })
    }

    fn build_group(&mut self, pair: Pair<Rule>) -> Result<Expr, Error> {
        let span = (pair.as_span(), self.origin.clone()).into();
        //println!("{:#?}", pair);
        let pairs = pair.into_inner();
        Ok(Expr::new(
            ExprKind::Group(
                pairs
                    .map(|pair| corrupt_expr!(self.build_expr_elm(pair), self.message_dispatcher))
                    .collect(),
            ),
            span,
        ))
    }

    fn build_fun_typ(&mut self, pair: Pair<Rule>) -> Result<Typ, Error> {
        let span = (pair.as_span(), self.origin.clone()).into();
        let mut pairs = pair.into_inner();

        let group = corrupt_expr!(
            self.build_group(parse_unwrap!(
                pairs.next(),
                "missing group in fun typ",
                span
            )),
            self.message_dispatcher
        );
        let ret = if let Some(pair) = pairs.next() {
            corrupt_expr!(self.build_expr_elm(pair), self.message_dispatcher)
        } else {
            Expr::new(ExprKind::Typ(Typ::Unknown), span)
        };

        Ok(Typ::Fun(group, ret))
    }

    fn build_typ(&mut self, pair: Pair<Rule>) -> Result<Typ, Error> {
        let span = (pair.as_span(), self.origin.clone()).into();
        match pair.as_rule() {
            Rule::sym => Ok(Typ::Simple(pair.as_str().to_string())),
            //Rule::typ => ASTBuilder::build_typ(pair)?,
            Rule::fun_typ => self.build_fun_typ(pair),
            Rule::group => Ok(Typ::Group(corrupt_expr!(
                self.build_group(pair),
                self.message_dispatcher
            ))),
            _ => {
                return Err(Error::default()
                    .with_type(ErrorType::ParsingError)
                    .with_span(span)
                    .with_message(
                        format!("invalid token when parsing type: {}", pair.to_string())
                            .to_string(),
                    )
                    .build())
            }
        }
    }

    fn build_sym_typ(&mut self, pair: Pair<Rule>) -> Result<Expr, Error> {
        let span = (pair.as_span(), self.origin.clone()).into();
        let mut pairs = pair.into_inner();
        let sym = parse_unwrap!(
            pairs.next(),
            "missing symbol, when parsing symbol:type",
            span
        )
        .as_str()
        .to_string();

        let typ = if let Some(typ_pair) = pairs.next() {
            match self.build_typ(typ_pair) {
                Ok(typ) => typ,
                Err(e) => {
                    self.message_dispatcher.dispatch(e, MessageLevel::Error);
                    Typ::Corrupted
                }
            }
        } else {
            Typ::Unknown
        };
        Ok(Expr::new(ExprKind::SymTyp(sym, typ), span))
    }

    fn build_lambda(&mut self, pair: Pair<Rule>) -> Result<Expr, Error> {
        let span = (pair.as_span(), self.origin.clone()).into();
        let mut pairs = pair.into_inner();
        //println!("{:#?}", pairs);
        let fun_typ = corrupt_typ!(
            self.build_fun_typ(parse_unwrap!(
                pairs.next(),
                "missing function type, when parsing lambda",
                span
            )),
            self.message_dispatcher
        );

        let body = self.build_body(parse_unwrap!(
            pairs.next(),
            "missing function body, when parsing lambda",
            span
        ));

        Ok(Expr::new(ExprKind::Lambda(fun_typ, body), span))
    }

    fn build_body(&mut self, pair: Pair<Rule>) -> Expr {
        let span = (pair.as_span(), self.origin.clone()).into();
        Expr::new(
            ExprKind::Body(
                pair.into_inner()
                    .map(|expr| corrupt_expr!(self.build_expr(expr), self.message_dispatcher))
                    .collect(),
            ),
            span,
        )
    }

    fn build_expr_elm(&mut self, pair: Pair<Rule>) -> Result<Expr, Error> {
        let span = (pair.as_span(), self.origin.clone()).into();
        Ok(match pair.as_rule() {
            Rule::body => self.build_body(pair),
            Rule::lambda => corrupt_expr!(self.build_lambda(pair), self.message_dispatcher),
            Rule::group => corrupt_expr!(self.build_group(pair), self.message_dispatcher),
            Rule::sym_typ => corrupt_expr!(self.build_sym_typ(pair), self.message_dispatcher),
            Rule::typ => match self.build_typ(pair) {
                Ok(typ) => Expr::new(ExprKind::Typ(typ), span),
                Err(e) => {
                    self.message_dispatcher.dispatch(e, MessageLevel::Error);
                    Expr::corrupted()
                }
            },

            Rule::sym => Expr::new(ExprKind::Sym(pair.as_str().to_string()), span),
            Rule::int | Rule::stg | Rule::rea | Rule::chr => match self.build_literal(pair) {
                Ok(literal) => Expr::new(ExprKind::Lit(literal), span),
                Err(e) => {
                    self.message_dispatcher.dispatch(e, MessageLevel::Error);
                    Expr::corrupted()
                }
            },
            _ => {
                return Err(Error::default()
                    .with_type(ErrorType::ParsingError)
                    .with_span(span)
                    .with_message(format!(
                        "invalid token when parsing expr element: {}",
                        pair.to_string()
                    ))
                    .build());
            }
        })
    }

    fn build_op(&mut self, pair: Pair<Rule>) -> Result<Expr, Error> {
        let span = (pair.as_span(), self.origin.clone()).into();
        Ok(match pair.as_rule() {
            Rule::op => Expr::new(ExprKind::Operator(pair.as_str().to_string()), span),
            _ => {
                return Err(Error::default()
                    .with_type(ErrorType::ParsingError)
                    .with_span(span)
                    .with_message(format!("wrong rule when building op {:#?}", pair))
                    .build())
            }
        })
    }

    pub(crate) fn build_expr(&mut self, pair: Pair<Rule>) -> Result<Expr, Error> {
        //println!("{:#?}", pair);
        let span = (pair.as_span(), self.origin.clone()).into();
        let mut pairs = pair.into_inner();
        let mut carry: Option<Expr> = None;

        while let Some(lhs_pair) = pairs.peek() {
            let lhs = if let Some(lhs) = carry {
                lhs
            } else {
                pairs.next();
                corrupt_expr!(self.build_expr_elm(lhs_pair), self.message_dispatcher)
            };

            let op = corrupt_expr!(
                self.build_op(parse_unwrap!(
                    pairs.next(),
                    "incomplete expr, missing operator",
                    span
                )),
                self.message_dispatcher
            );
            let rhs = corrupt_expr!(
                self.build_expr_elm(parse_unwrap!(
                    pairs.next(),
                    "incomplete expr, missing right hand side",
                    span
                )),
                self.message_dispatcher
            );

            let new_span = Span {
                start: span.start.clone(),
                end: span.end.clone(),
                content: "".to_string(), //format!("{} {} {}", lhs, op, rhs), overflows
                origin: self.origin.clone(),
            };

            let expr = Expr::new(ExprKind::Expression(lhs, op, rhs), new_span);

            carry = Some(expr);
        }

        return Ok(parse_unwrap!(carry, "expression is empty", span));
    }
}

impl Default for ParseTree {
    fn default() -> Self {
        Self {
            ast: Expr::corrupted(),
            message_dispatcher: MessageDispatcher::new(MessageLevel::Info, MessageOutput::Stderr),
            origin: Default::default(),
        }
    }
}
