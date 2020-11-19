use pest::Parser;

use crate::{
    ast::Expr,
    ast::ExprKind,
    ast::{Pair, Span},
    corrupt_expr, corrupt_vec,
    message::ErrorType,
    message::MessageDispatcher,
    operator::{build_operators, OperatorList},
    parse_unwrap,
    parser::{KelpParser, Rule},
    Error,
};

#[derive(Debug, Clone)]
pub struct ASTBuilder {
    ast: Expr,
    parse_tree: Option<Pair<'static, Rule>>,
    operator_list: OperatorList,
    msg_dispatcher: MessageDispatcher,
}

impl ASTBuilder {
    pub fn new(input: &'static str, msg_dispatcher: MessageDispatcher) -> Result<Self, Error> {
        let span = Span::default();
        Ok(Self {
            parse_tree: Some(parse_unwrap!(
                KelpParser::parse(Rule::root, input)?.next(),
                "missing root, the file is not valid/empty",
                span
            )),
            ast: Expr::corrupted(),
            operator_list: OperatorList::default(),
            msg_dispatcher,
        })
    }

    pub fn first_pass(&mut self) -> &mut Self {
        let span: Span = self.parse_tree.as_ref().unwrap().as_span().into();
        let body = self
            .clone()
            .parse_tree
            .unwrap()
            .into_inner()
            .map(|expr| corrupt_expr!(self.build_expr(expr), self.msg_dispatcher))
            .collect();

        self.ast = Expr::new(ExprKind::Body(body), span);

        self
    }

    pub fn build_operators(&mut self) -> &mut Self {
        self.operator_list = match build_operators(self.ast.clone(), self.msg_dispatcher.clone()) {
            Ok(operator_list) => operator_list,
            Err(e) => {
                self.msg_dispatcher.dispatch(e);
                return self;
            }
        };
        self
    }

    pub fn second_pass(&mut self) -> Result<&mut Self, Error> {
        panic!();
    }

    pub fn build(&mut self) -> Self {
        let ast = std::mem::take(self);
        ASTBuilder {
            ast: ast.ast,
            parse_tree: ast.parse_tree,
            operator_list: ast.operator_list,
            msg_dispatcher: ast.msg_dispatcher,
        }
    }

    pub fn get_ast(&self) -> Expr {
        self.ast.clone()
    }

    // first pass
    fn build_fun_typ(&mut self, pair: Pair<'static, Rule>) -> Result<Expr, Error> {
        let span = pair.as_span().into();
        let mut pairs = pair.into_inner();
        let args = corrupt_vec!(
            self.build_fields(parse_unwrap!(
                pairs.next(),
                "missing arguments when defining function type",
                span
            )),
            self.msg_dispatcher
        );
        let return_typ = corrupt_expr!(
            self.build_typ(parse_unwrap!(
                pairs.next(),
                "missing return type when defining function type",
                span
            )),
            self.msg_dispatcher
        );
        Ok(Expr::new(ExprKind::FunTyp(args, return_typ), span))
    }

    fn build_group_typ(&mut self, pair: Pair<'static, Rule>) -> Result<Expr, Error> {
        let fields = corrupt_vec!(self.build_fields(pair.clone()), self.msg_dispatcher);
        let valid_fields = fields
            .into_iter()
            .map(|field| match field.kind() {
                ExprKind::Sym(s) => Expr::new(ExprKind::Typ(s.to_string()), field.span()),
                _ => {
                    let e = Error::default()
                        .with_type(ErrorType::ParsingError)
                        .with_span(pair.as_span().into())
                        .with_message(
                            "group type can only contain types not data or symbol: type"
                                .to_string(),
                        )
                        .build();
                    self.msg_dispatcher.dispatch(e);
                    Expr::new(ExprKind::Corrupted, Span::default())
                }
            })
            .collect();
        Ok(Expr::new(
            ExprKind::Group(valid_fields),
            pair.as_span().into(),
        ))
    }

    fn build_typ(&mut self, pair: Pair<'static, Rule>) -> Result<Expr, Error> {
        let span = pair.as_span().into();
        Ok(match pair.as_rule() {
            Rule::sym => Expr::new(ExprKind::Typ(pair.as_str().to_string()), span),
            //Rule::typ => ASTBuilder::build_typ(pair)?,
            Rule::fun_typ => corrupt_expr!(self.build_fun_typ(pair), self.msg_dispatcher),
            Rule::group => corrupt_expr!(self.build_group_typ(pair), self.msg_dispatcher),
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
        })
    }

    fn build_sym_typ(&mut self, pair: Pair<'static, Rule>) -> Result<Expr, Error> {
        let span = pair.as_span().into();
        let mut pairs = pair.into_inner();
        let sym = parse_unwrap!(
            pairs.next(),
            "missing symbol, when parsing symbol:type",
            span
        )
        .as_str()
        .to_string();
        let typ = corrupt_expr!(
            self.build_typ(parse_unwrap!(
                pairs.next(),
                "missing type, when parsing symbol:type",
                span
            )),
            self.msg_dispatcher
        );
        Ok(Expr::new(ExprKind::SymTyp(sym, typ), span))
    }

    fn build_field(&mut self, pair: Pair<'static, Rule>) -> Result<Expr, Error> {
        let span = pair.as_span().into();
        match pair.as_rule() {
            Rule::field => self.build_expr(parse_unwrap!(
                pair.into_inner().next(),
                "missing a field, when parsing field",
                span
            )),
            _ => {
                return Err(Error::default()
                    .with_type(ErrorType::ParsingError)
                    .with_span(span)
                    .with_message(format!(
                        "invalid token when parsing group field: {}",
                        pair.to_string()
                    ))
                    .build())
            }
        }
    }

    fn build_fields(&mut self, pair: Pair<'static, Rule>) -> Result<Vec<Expr>, Error> {
        Ok(pair
            .into_inner()
            .map(|field| corrupt_expr!(self.build_field(field.clone()), self.msg_dispatcher))
            .collect())
    }

    fn build_lambda(&mut self, pair: Pair<'static, Rule>) -> Result<Expr, Error> {
        let span = pair.as_span().into();
        let mut pairs = pair.into_inner();
        //println!("{:#?}", pairs);
        let fun_typ = corrupt_expr!(
            self.build_fun_typ(parse_unwrap!(
                pairs.next(),
                "missing function type, when parsing lambda",
                span
            )),
            self.msg_dispatcher
        );

        let body = self.build_fun_bod(parse_unwrap!(
            pairs.next(),
            "missing function body, when parsing lambda",
            span
        ));

        Ok(Expr::new(ExprKind::Lambda(fun_typ, body), span))
    }

    fn build_fun_bod(&mut self, pair: Pair<'static, Rule>) -> Vec<Expr> {
        pair.into_inner()
            .map(|expr| corrupt_expr!(self.build_expr(expr), self.msg_dispatcher))
            .collect()
    }

    fn build_fun_blk(&mut self, pair: Pair<'static, Rule>) -> Result<Expr, Error> {
        let span = pair.as_span().into();
        let mut pairs = pair.into_inner();
        let sym = corrupt_expr!(
            self.build_expr(parse_unwrap!(
                pairs.next(),
                "missing symbol, when parsing function blok",
                span
            )),
            self.msg_dispatcher
        );
        let fun_bod = self.build_fun_bod(parse_unwrap!(
            pairs.next(),
            "missing function body, when parsing function block",
            span
        ));
        Ok(Expr::new(ExprKind::FunBlk(sym, fun_bod), span))
    }

    fn build_sym_or_op_def(&mut self, pair: Pair<'static, Rule>) -> Result<Expr, Error> {
        let span = pair.as_span().into();
        Ok(match pair.as_rule() {
            Rule::operator => Expr::new(ExprKind::Operator(pair.as_str().to_string()), span),
            Rule::sym_typ => corrupt_expr!(self.build_sym_typ(pair), self.msg_dispatcher),
            Rule::sym => Expr::new(ExprKind::Sym(pair.as_str().to_string()), span),
            _ => {
                return Err(Error::default()
                    .with_type(ErrorType::ParsingError)
                    .with_span(span)
                    .with_message(
                        "let expression must use either operator or symbol(+:type)".to_string(),
                    )
                    .build());
            }
        })
    }

    fn build_def(&mut self, pair: Pair<'static, Rule>) -> Result<Expr, Error> {
        let span = pair.as_span().into();
        let mut pairs = pair.clone().into_inner();
        let sym_or_operator = parse_unwrap!(
            pairs.next(),
            "missing symbol/operator, when parsing their definition",
            span
        );
        Ok(match sym_or_operator.as_rule() {
            Rule::sym | Rule::operator_def => Expr::new(
                ExprKind::Def(
                    corrupt_expr!(
                        self.build_sym_or_op_def(sym_or_operator),
                        self.msg_dispatcher
                    ),
                    corrupt_expr!(
                        self.build_sym_or_op_def(parse_unwrap!(
                            pairs.next(),
                            "missing operator, when parsing symbol definition",
                            span
                        )),
                        self.msg_dispatcher
                    ),
                    corrupt_expr!(
                        self.build_expr(parse_unwrap!(
                            pairs.next(),
                            "missing rest of definition, when parsing symbol definition",
                            span
                        )),
                        self.msg_dispatcher
                    ),
                ),
                span,
            ),
            _ => {
                return Err(Error::default()
                    .with_type(ErrorType::ParsingError)
                    .with_span(span)
                    .with_message("invalid token definition".to_string())
                    .build());
            }
        })
    }

    pub(super) fn build_expr(&mut self, pair: Pair<'static, Rule>) -> Result<Expr, Error> {
        let span = pair.as_span().into();
        Ok(match pair.as_rule() {
            Rule::def => corrupt_expr!(self.build_def(pair), self.msg_dispatcher),
            Rule::op => Expr::new(ExprKind::UnresolvedOp(pair.as_str().to_string()), span),
            Rule::fun_blk => corrupt_expr!(self.build_fun_blk(pair), self.msg_dispatcher),
            Rule::def_fun => corrupt_expr!(self.build_lambda(pair), self.msg_dispatcher),
            Rule::def_val => corrupt_expr!(
                self.build_expr(parse_unwrap!(
                    pair.into_inner().next(),
                    "value definition is invalid",
                    span
                )),
                self.msg_dispatcher
            ),
            Rule::group => Expr::new(
                ExprKind::Group(corrupt_vec!(self.build_fields(pair), self.msg_dispatcher)),
                span,
            ),
            Rule::sym_typ => corrupt_expr!(self.build_sym_typ(pair), self.msg_dispatcher),
            Rule::typ => corrupt_expr!(self.build_typ(pair), self.msg_dispatcher),
            Rule::sym => Expr::new(ExprKind::Sym(pair.as_str().to_string()), span),
            Rule::dec => Expr::new(ExprKind::Dec(pair.as_str().parse::<f64>()?), span),
            Rule::int => Expr::new(ExprKind::Int(pair.as_str().parse::<i64>()?), span),
            Rule::str => Expr::new(ExprKind::Str(pair.as_str().to_string()), span),
            _ => {
                return Err(Error::default()
                    .with_type(ErrorType::ParsingError)
                    .with_span(span)
                    .with_message(format!(
                        "invalid token when parsing expr: {}",
                        pair.to_string()
                    ))
                    .build());
            }
        })
    }
}

impl Default for ASTBuilder {
    fn default() -> Self {
        Self {
            ast: Expr::corrupted(),
            parse_tree: None,
            operator_list: OperatorList::default(),
            msg_dispatcher: MessageDispatcher::new(
                crate::message::MessageLevel::Info,
                crate::message::MessageOutput::Stderr,
            ),
        }
    }
}
