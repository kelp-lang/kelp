use pest::iterators::Pair;

use crate::{
    ast::{Expr, ExprKind, Typ},
    corrupt_expr, corrupt_typ,
    message::ErrorType,
    message::MessageDispatcher,
    parse_unwrap,
    parser::Rule,
    Error, Span,
};

use super::Literal;

#[derive(Debug, Clone)]
pub struct ASTBuilder {
    ast: Expr,
    msg_dispatcher: MessageDispatcher,
}

impl ASTBuilder {
    pub fn new(msg_dispatcher: MessageDispatcher) -> Self {
        Self {
            ast: Expr::corrupted(),
            msg_dispatcher,
        }
    }

    pub fn build(&mut self) -> Self {
        let ast = std::mem::take(self);
        ASTBuilder {
            ast: ast.ast,
            msg_dispatcher: ast.msg_dispatcher,
        }
    }

    pub fn get_ast(&self) -> Expr {
        self.ast.clone()
    }

    pub fn build_ast(&mut self, pair: Pair<Rule>) -> Result<&mut Self, Error> {
        let span = pair.as_span().into();
        let body = pair
            .into_inner()
            .map(|pair| corrupt_expr!(self.build_expr(pair), self.msg_dispatcher))
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
                    .with_span(pair.as_span().into())
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
        let span = pair.as_span().into();
        //println!("{:#?}", pair);
        let pairs = pair.into_inner();
        Ok(Expr::new(
            ExprKind::Group(
                pairs
                    .map(|pair| corrupt_expr!(self.build_expr_elm(pair), self.msg_dispatcher))
                    .collect(),
            ),
            span,
        ))
    }

    fn build_fun_typ(&mut self, pair: Pair<Rule>) -> Result<Typ, Error> {
        let span = pair.as_span().into();
        let mut pairs = pair.into_inner();

        let group = corrupt_expr!(
            self.build_group(parse_unwrap!(
                pairs.next(),
                "missing group in fun typ",
                span
            )),
            self.msg_dispatcher
        );
        let ret = if let Some(pair) = pairs.next() {
            corrupt_expr!(self.build_expr_elm(pair), self.msg_dispatcher)
        } else {
            Expr::new(ExprKind::Typ(Typ::Unknown), span)
        };

        Ok(Typ::Fun(group, ret))
    }

    fn build_typ(&mut self, pair: Pair<Rule>) -> Result<Typ, Error> {
        let span = pair.as_span().into();
        match pair.as_rule() {
            Rule::sym => Ok(Typ::Simple(pair.as_str().to_string())),
            //Rule::typ => ASTBuilder::build_typ(pair)?,
            Rule::fun_typ => self.build_fun_typ(pair),
            Rule::group => Ok(Typ::Group(corrupt_expr!(
                self.build_group(pair),
                self.msg_dispatcher
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
        let span = pair.as_span().into();
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
                    self.msg_dispatcher.dispatch(e, crate::MessageLevel::Error);
                    Typ::Corrupted
                }
            }
        } else {
            Typ::Unknown
        };
        Ok(Expr::new(ExprKind::SymTyp(sym, typ), span))
    }

    fn build_lambda(&mut self, pair: Pair<Rule>) -> Result<Expr, Error> {
        let span = pair.as_span().into();
        let mut pairs = pair.into_inner();
        //println!("{:#?}", pairs);
        let fun_typ = corrupt_typ!(
            self.build_fun_typ(parse_unwrap!(
                pairs.next(),
                "missing function type, when parsing lambda",
                span
            )),
            self.msg_dispatcher
        );

        let body = self.build_body(parse_unwrap!(
            pairs.next(),
            "missing function body, when parsing lambda",
            span
        ));

        Ok(Expr::new(ExprKind::Lambda(fun_typ, body), span))
    }

    fn build_body(&mut self, pair: Pair<Rule>) -> Expr {
        let span = pair.as_span().into();
        Expr::new(
            ExprKind::Body(
                pair.into_inner()
                    .map(|expr| corrupt_expr!(self.build_expr(expr), self.msg_dispatcher))
                    .collect(),
            ),
            span,
        )
    }

    fn build_expr_elm(&mut self, pair: Pair<Rule>) -> Result<Expr, Error> {
        let span = pair.as_span().into();
        Ok(match pair.as_rule() {
            Rule::body => self.build_body(pair),
            Rule::lambda => corrupt_expr!(self.build_lambda(pair), self.msg_dispatcher),
            Rule::group => corrupt_expr!(self.build_group(pair), self.msg_dispatcher),
            Rule::sym_typ => corrupt_expr!(self.build_sym_typ(pair), self.msg_dispatcher),
            Rule::typ => match self.build_typ(pair) {
                Ok(typ) => Expr::new(ExprKind::Typ(typ), span),
                Err(e) => {
                    self.msg_dispatcher.dispatch(e, crate::MessageLevel::Error);
                    Expr::corrupted()
                }
            },

            Rule::sym => Expr::new(ExprKind::Sym(pair.as_str().to_string()), span),
            Rule::int | Rule::stg | Rule::rea | Rule::chr => match self.build_literal(pair) {
                Ok(literal) => Expr::new(ExprKind::Lit(literal), span),
                Err(e) => {
                    self.msg_dispatcher.dispatch(e, crate::MessageLevel::Error);
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
        let span = pair.as_span().into();
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
        let span = pair.as_span().into();
        let mut pairs = pair.into_inner();
        let mut carry: Option<Expr> = None;

        while let Some(lhs_pair) = pairs.peek() {
            let lhs = if let Some(lhs) = carry {
                lhs
            } else {
                pairs.next();
                corrupt_expr!(self.build_expr_elm(lhs_pair), self.msg_dispatcher)
            };

            let op = corrupt_expr!(
                self.build_op(parse_unwrap!(
                    pairs.next(),
                    "incomplete expr, missing operator",
                    span
                )),
                self.msg_dispatcher
            );
            let rhs = corrupt_expr!(
                self.build_expr_elm(parse_unwrap!(
                    pairs.next(),
                    "incomplete expr, missing right hand side",
                    span
                )),
                self.msg_dispatcher
            );

            let new_span = Span {
                start: span.start.clone(),
                end: span.end.clone(),
                content: "".to_string(), //format!("{} {} {}", lhs, op, rhs), overflows
            };

            let expr = Expr::new(ExprKind::Expression(lhs, op, rhs), new_span);

            carry = Some(expr);
        }

        return Ok(parse_unwrap!(carry, "expression is empty", span));
    }
}

impl Default for ASTBuilder {
    fn default() -> Self {
        Self {
            ast: Expr::corrupted(),
            msg_dispatcher: MessageDispatcher::new(
                crate::message::MessageLevel::Info,
                crate::message::MessageOutput::Stderr,
            ),
        }
    }
}
