use std::rc::Rc;

use crate::{
    ast::*,
    corrupt_expr, corrupt_vec,
    error::{Error, ErrorType},
    parse_unwrap,
    parser::Rule,
};
use pest::iterators::Pair;

impl ASTBuilder {
    fn build_fun_typ(&mut self, pair: Pair<'static, Rule>) -> Result<Expr, Error> {
        let mut pairs = pair.into_inner();
        let args = corrupt_vec!(
            self.build_fields(parse_unwrap!(
                pairs.next(),
                "missing arguments when defining function type",
                self
            )),
            self
        );
        let return_typ = corrupt_expr!(
            self.build_typ(parse_unwrap!(
                pairs.next(),
                "missing return type when defining function type",
                self
            )),
            self
        );
        Ok(Expr::FunTyp(args, Rc::new(return_typ)))
    }

    fn build_group_typ(&mut self, pair: Pair<'static, Rule>) -> Result<Expr, Error> {
        let fields = corrupt_vec!(self.build_fields(pair.clone()), self);

        Ok(Expr::Group(
            fields
                .into_iter()
                .map(|field| match field {
                    Expr::Sym(s) => Ok(Expr::Typ(s)),
                    _ => {
                        return Err(Error::default()
                            .with_type(ErrorType::ParsingError)
                            .with_position(pair.as_span().start(), pair.as_span().end())
                            .with_message(
                                "group type can only contain types not data or symbol: type"
                                    .to_string(),
                            )
                            .build())
                    }
                })
                .collect::<Result<Vec<_>, _>>()?,
        ))
    }

    fn build_typ(&mut self, pair: Pair<'static, Rule>) -> Result<Expr, Error> {
        Ok(match pair.as_rule() {
            Rule::sym => Expr::Typ(pair.as_str().to_string()),
            //Rule::typ => ASTBuilder::build_typ(pair)?,
            Rule::fun_typ => corrupt_expr!(self.build_fun_typ(pair), self),
            Rule::group => corrupt_expr!(self.build_group_typ(pair), self),
            _ => {
                return Err(Error::default()
                    .with_type(ErrorType::ParsingError)
                    .with_position(pair.as_span().start(), pair.as_span().end())
                    .with_message(
                        format!("invalid token when parsing type: {}", pair.to_string())
                            .to_string(),
                    )
                    .build())
            }
        })
    }

    fn build_sym_typ(&mut self, pair: Pair<'static, Rule>) -> Result<Expr, Error> {
        let mut pairs = pair.into_inner();
        let sym = parse_unwrap!(
            pairs.next(),
            "missing symbol, when parsing symbol:type",
            self
        )
        .as_str()
        .to_string();
        let typ = corrupt_expr!(
            self.build_typ(parse_unwrap!(
                pairs.next(),
                "missing type, when parsing symbol:type",
                self
            )),
            self
        );
        Ok(Expr::SymTyp(sym, Rc::new(typ)))
    }

    fn build_field(&mut self, pair: Pair<'static, Rule>) -> Result<Expr, Error> {
        match pair.as_rule() {
            Rule::field => self.build_expr(parse_unwrap!(
                pair.into_inner().next(),
                "missing a field, when parsing field",
                self
            )),
            _ => {
                return Err(Error::default()
                    .with_type(ErrorType::ParsingError)
                    .with_position(pair.as_span().start(), pair.as_span().end())
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
            .map(|field| corrupt_expr!(self.build_field(field.clone()), self))
            .collect())
    }

    fn build_lambda(&mut self, pair: Pair<'static, Rule>) -> Result<Expr, Error> {
        let mut pairs = pair.into_inner();
        //println!("{:#?}", pairs);
        let fun_typ = corrupt_expr!(
            self.build_fun_typ(parse_unwrap!(
                pairs.next(),
                "missing function type, when parsing lambda",
                self
            )),
            self
        );

        let body = self.build_fun_bod(parse_unwrap!(
            pairs.next(),
            "missing function body, when parsing lambda",
            self
        ));

        Ok(Expr::Lambda {
            fun_typ: Rc::new(fun_typ),
            body: body,
        })
    }

    fn build_fun_bod(&mut self, pair: Pair<'static, Rule>) -> Vec<Expr> {
        pair.into_inner()
            .map(|expr| corrupt_expr!(self.build_expr(expr), self))
            .collect()
    }

    fn build_fun_blk(&mut self, pair: Pair<'static, Rule>) -> Result<Expr, Error> {
        let mut pairs = pair.into_inner();
        let sym = corrupt_expr!(
            self.build_expr(parse_unwrap!(
                pairs.next(),
                "missing symbol, when parsing function blok",
                self
            )),
            self
        );
        let fun_bod = self.build_fun_bod(parse_unwrap!(
            pairs.next(),
            "missing function body, when parsing function block",
            self
        ));
        Ok(Expr::FunBlk(Rc::new(sym), fun_bod))
    }

    fn build_def_val(&mut self, pair: Pair<'static, Rule>) -> Result<Expr, Error> {
        let pair = parse_unwrap!(
            pair.into_inner().next(),
            "missing value, when parsing value definition",
            self
        );
        Ok(match pair.as_rule() {
            Rule::def => corrupt_expr!(self.build_def(pair), self),
            Rule::op => Expr::UnresolvedOp(pair.clone().as_str().to_string()),
            //Rule::fun_blk => ASTBuilder::build_fun_blk(pair)?,
            //Rule::def_fun => ASTBuilder::build_lambda(pair)?,
            //Rule::def_val => ASTBuilder::build_def_val(pair)?,
            Rule::group => Expr::Group(corrupt_vec!(self.build_fields(pair), self)),
            Rule::sym_typ => corrupt_expr!(self.build_sym_typ(pair), self),
            Rule::typ => corrupt_expr!(self.build_typ(pair), self),
            Rule::sym => Expr::Sym(pair.as_str().to_string()),
            Rule::dec => Expr::Dec(pair.as_str().parse::<f64>()?),
            Rule::int => Expr::Int(pair.as_str().parse::<i64>()?),
            Rule::str => Expr::Str(pair.as_str().to_string()),
            _ => {
                return Err(Error::default()
                    .with_type(ErrorType::ParsingError)
                    .with_position(pair.as_span().start(), pair.as_span().end())
                    .with_message(format!(
                        "invalid token when parsing definition of value: {}",
                        pair.to_string()
                    ))
                    .build())
            }
        })
    }

    fn build_def_sym(
        &mut self,
        sym: Pair<'static, Rule>,
        operator: Pair<'static, Rule>,
        rest: Pair<'static, Rule>,
    ) -> Expr {
        match operator.as_str() {
            "=" => Expr::Def(
                Rc::new(corrupt_expr!(self.build_expr(sym), self)),
                Rc::new(corrupt_expr!(self.build_expr(rest), self)),
            ),
            &_ => Expr::DefWOp(
                Rc::new(corrupt_expr!(self.build_expr(sym), self)),
                operator.as_str().to_string(),
                Rc::new(corrupt_expr!(self.build_expr(rest), self)),
            ),
        }
    }

    fn build_def_operator(
        &mut self,
        operator_to_def: Pair<'static, Rule>,
        operator: Pair<'static, Rule>,
        rest: Pair<'static, Rule>,
    ) -> Result<Expr, Error> {
        Ok(match operator.as_str() {
            "=" => Expr::DefOp(
                operator_to_def.as_str().to_string(),
                Rc::new(corrupt_expr!(self.build_fun_blk(rest), self)),
            ),
            "<<" => Expr::DefOpApp(
                operator_to_def.as_str().to_string(),
                Rc::new(corrupt_expr!(self.build_lambda(rest), self)),
            ),
            &_ => {
                return Err(Error::default()
                    .with_type(ErrorType::ParsingError)
                    .with_position(operator.as_span().start(), operator.as_span().end())
                    .with_message(format!(
                        "invalid definition operator: {}",
                        operator.as_str()
                    ))
                    .build());
            }
        })
    }

    fn build_def(&mut self, pair: Pair<'static, Rule>) -> Result<Expr, Error> {
        let mut pairs = pair.clone().into_inner();
        let sym_or_operator = parse_unwrap!(
            pairs.next(),
            "missing symbol/operator, when parsing their definition",
            self
        );
        Ok(match sym_or_operator.as_rule() {
            Rule::sym => self.build_def_sym(
                sym_or_operator,
                parse_unwrap!(
                    pairs.next(),
                    "missing operator, when parsing symbol definition",
                    self
                ),
                parse_unwrap!(
                    pairs.next(),
                    "missing rest of definition, when parsing symbol definition",
                    self
                ),
            ),
            Rule::operator_def => self.build_def_operator(
                sym_or_operator,
                parse_unwrap!(
                    pairs.next(),
                    "missing operator, when parsing operator definition",
                    self
                ),
                parse_unwrap!(
                    pairs.next(),
                    "missing rest of definition, when parsing operator definition",
                    self
                ),
            )?,
            _ => {
                return Err(Error::default()
                    .with_type(ErrorType::ParsingError)
                    .with_position(pair.as_span().start(), pair.as_span().end())
                    .with_message("invalid token definition".to_string())
                    .build());
            }
        })
    }

    pub(super) fn build_expr(&mut self, pair: Pair<'static, Rule>) -> Result<Expr, Error> {
        Ok(match pair.as_rule() {
            Rule::def => corrupt_expr!(self.build_def(pair), self),
            Rule::op => Expr::UnresolvedOp(pair.as_str().to_string()),
            Rule::fun_blk => corrupt_expr!(self.build_fun_blk(pair), self),
            Rule::def_fun => corrupt_expr!(self.build_lambda(pair), self),
            Rule::def_val => corrupt_expr!(self.build_def_val(pair), self),
            Rule::group => Expr::Group(corrupt_vec!(self.build_fields(pair), self)),
            Rule::sym_typ => corrupt_expr!(self.build_sym_typ(pair), self),
            Rule::typ => corrupt_expr!(self.build_typ(pair), self),
            Rule::sym => Expr::Sym(pair.as_str().to_string()),
            Rule::dec => Expr::Dec(pair.as_str().parse::<f64>()?),
            Rule::int => Expr::Int(pair.as_str().parse::<i64>()?),
            Rule::str => Expr::Str(pair.as_str().to_string()),
            _ => {
                return Err(Error::default()
                    .with_type(ErrorType::ParsingError)
                    .with_position(pair.as_span().start(), pair.as_span().end())
                    .with_message(format!(
                        "invalid token when parsing expr: {}",
                        pair.to_string()
                    ))
                    .build());
            }
        })
    }
}
