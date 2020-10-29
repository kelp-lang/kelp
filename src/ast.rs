use std::rc::Rc;

use pest::iterators::Pair;
use pest::iterators::Pairs;

use crate::error::Error;
use crate::error::ErrorType;
use crate::parser::Rule;

pub struct ASTBuilder;

#[derive(Debug)]
pub enum Expr<'a> {
    Def(Rc<Expr<'a>>, Rc<Expr<'a>>), // def {name + type?} = {value}
    DefWOp(Rc<Expr<'a>>, Operator<'a>, Rc<Expr<'a>>), // def {name + type?} [{operator}] {Lambda/Value}
    DefOp(Operator<'a>, Rc<Expr<'a>>),                // def [{operator}] = {FunBlk}
    DefOpApp(Operator<'a>, Rc<Expr<'a>>),             // def [{operator}] << {Lamba}
    Op(Vec<(Expr<'a>, Operator<'a>, Expr<'a>)>),      // not going to touch this
    UnresolvedOp(Pair<'a, Rule>),                     // neither this
    FunBlk(Rc<Expr<'a>>, Vec<Expr<'a>>),              // {name} ({expressions})
    Lambda {
        fun_typ: Rc<Expr<'a>>,
        body: Vec<Expr<'a>>,
    },
    Sym(&'a str),
    SymTyp(&'a str, Rc<Expr<'a>>),       // {symbol}: {type}
    FunTyp(Vec<Expr<'a>>, Rc<Expr<'a>>), // [{args}]: {return_type}
    Typ(&'a str),
    Group(Vec<Expr<'a>>),
    Int(i64),
    Dec(f64),
    Str(&'a str),
}

type Operator<'a> = &'a str;
type Sym<'a> = &'a str;

impl ASTBuilder {
    fn build_fun_typ(pair: Pair<Rule>) -> Result<Expr, Error> {
        let mut pairs = pair.into_inner();
        Ok(Expr::FunTyp(
            ASTBuilder::build_fields(pairs.next().unwrap())?,
            Rc::new(ASTBuilder::build_typ(pairs.next().unwrap())?),
        ))
    }

    fn build_typ(pair: Pair<Rule>) -> Result<Expr, Error> {
        Ok(match pair.as_rule() {
            Rule::sym => Expr::Typ(pair.as_str()),
            //Rule::typ => ASTBuilder::build_typ(pair)?,
            Rule::fun_typ => ASTBuilder::build_fun_typ(pair)?,
            _ => {
                return Err(Error {
                    err_type: ErrorType::ParsingError,
                    start: pair.as_span().start(),
                    end: pair.as_span().end(),
                    msg: format!("invalid token when parsing type: {}", pair.to_string())
                        .to_string(),
                })
            }
        })
    }

    fn build_sym_typ(pair: Pair<Rule>) -> Result<Expr, Error> {
        let mut pairs = pair.into_inner();
        Ok(Expr::SymTyp(
            pairs.next().unwrap().as_str(),
            Rc::new(ASTBuilder::build_typ(pairs.next().unwrap())?),
        ))
    }

    fn build_field(pair: Pair<Rule>) -> Result<Expr, Error> {
        match pair.as_rule() {
            Rule::field => ASTBuilder::build_expr(pair.into_inner().next().unwrap()),
            _ => {
                return Err(Error {
                    err_type: ErrorType::ParsingError,
                    start: pair.as_span().start(),
                    end: pair.as_span().end(),
                    msg: format!(
                        "invalid token when parsing group field: {}",
                        pair.to_string()
                    )
                    .to_string(),
                })
            }
        }
    }

    fn build_fields(pair: Pair<Rule>) -> Result<Vec<Expr>, Error> {
        Ok(pair
            .into_inner()
            .map(ASTBuilder::build_field)
            .collect::<Result<Vec<_>, _>>()?)
    }

    fn build_lambda(pair: Pair<Rule>) -> Result<Expr, Error> {
        let mut pairs = pair.into_inner();
        Ok(Expr::Lambda {
            fun_typ: Rc::new(ASTBuilder::build_fun_typ(pairs.next().unwrap())?),
            body: ASTBuilder::build_fun_bod(pairs.next().unwrap())?,
        })
    }

    fn build_fun_bod(pair: Pair<Rule>) -> Result<Vec<Expr>, Error> {
        Ok(pair
            .into_inner()
            .map(ASTBuilder::build_expr)
            .collect::<Result<Vec<_>, _>>()?)
    }

    fn build_fun_blk(pair: Pair<Rule>) -> Result<Expr, Error> {
        let mut pairs = pair.into_inner();
        Ok(Expr::FunBlk(
            Rc::new(ASTBuilder::build_expr(pairs.next().unwrap())?),
            ASTBuilder::build_fun_bod(pairs.next().unwrap())?,
        ))
    }

    fn build_def_val(pair: Pair<Rule>) -> Result<Expr, Error> {
        let pair = pair.into_inner().next().unwrap();
        Ok(match pair.as_rule() {
            Rule::def => ASTBuilder::build_def(pair)?,
            Rule::op => Expr::UnresolvedOp(pair.clone()),
            //Rule::fun_blk => ASTBuilder::build_fun_blk(pair)?,
            //Rule::def_fun => ASTBuilder::build_lambda(pair)?,
            //Rule::def_val => ASTBuilder::build_def_val(pair)?,
            Rule::group => Expr::Group(ASTBuilder::build_fields(pair)?),
            Rule::sym_typ => ASTBuilder::build_sym_typ(pair)?,
            Rule::typ => ASTBuilder::build_typ(pair)?,
            Rule::sym => Expr::Sym(pair.as_str()),
            Rule::dec => Expr::Dec(pair.as_str().parse::<f64>()?),
            Rule::int => Expr::Int(pair.as_str().parse::<i64>()?),
            Rule::str => Expr::Str(pair.as_str()),
            _ => {
                return Err(Error {
                    err_type: ErrorType::ParsingError,
                    start: pair.as_span().start(),
                    end: pair.as_span().end(),
                    msg: format!(
                        "invalid token when parsing definition of value: {}",
                        pair.to_string()
                    )
                    .to_string(),
                })
            }
        })
    }

    fn build_def_sym<'a>(
        sym: Pair<'a, Rule>,
        operator: Pair<'a, Rule>,
        rest: Pair<'a, Rule>,
    ) -> Result<Expr<'a>, Error> {
        Ok(match operator.as_str() {
            "=" => Expr::Def(
                Rc::new(ASTBuilder::build_expr(sym)?),
                Rc::new(ASTBuilder::build_expr(rest)?),
            ),
            &_ => Expr::DefWOp(
                Rc::new(ASTBuilder::build_expr(sym)?),
                operator.as_str(),
                Rc::new(ASTBuilder::build_expr(rest)?),
            ),
        })
    }

    fn build_def_operator<'a>(
        operator_to_def: Pair<'a, Rule>,
        operator: Pair<'a, Rule>,
        rest: Pair<'a, Rule>,
    ) -> Result<Expr<'a>, Error> {
        Ok(match operator.as_str() {
            "=" => Expr::DefOp(
                operator_to_def.as_str(),
                Rc::new(ASTBuilder::build_fun_blk(rest)?),
            ),
            "<<" => Expr::DefOpApp(
                operator_to_def.as_str(),
                Rc::new(ASTBuilder::build_lambda(rest)?),
            ),
            &_ => {
                return Err(Error {
                    err_type: ErrorType::ParsingError,
                    start: operator.as_span().start(),
                    end: operator.as_span().end(),
                    msg: "invalid definition operator".to_string(),
                })
            }
        })
    }

    fn build_def(pair: Pair<Rule>) -> Result<Expr, Error> {
        let mut pairs = pair.clone().into_inner();
        let sym_or_operator = pairs.next().unwrap();
        Ok(match sym_or_operator.as_rule() {
            Rule::sym => ASTBuilder::build_def_sym(
                sym_or_operator,
                pairs.next().unwrap(),
                pairs.next().unwrap(),
            )?,
            Rule::operator_def => ASTBuilder::build_def_operator(
                sym_or_operator,
                pairs.next().unwrap(),
                pairs.next().unwrap(),
            )?,
            _ => {
                return Err(Error {
                    err_type: ErrorType::ParsingError,
                    start: pair.as_span().start(),
                    end: pair.as_span().end(),
                    msg: "invalid token in definition".to_string(),
                })
            }
        })
    }

    fn build_expr(pair: Pair<Rule>) -> Result<Expr, Error> {
        Ok(match pair.as_rule() {
            Rule::def => ASTBuilder::build_def(pair)?,
            Rule::op => Expr::UnresolvedOp(pair.clone()),
            Rule::fun_blk => ASTBuilder::build_fun_blk(pair)?,
            Rule::def_fun => ASTBuilder::build_lambda(pair)?,
            Rule::def_val => ASTBuilder::build_def_val(pair)?,
            Rule::group => Expr::Group(ASTBuilder::build_fields(pair)?),
            Rule::sym_typ => ASTBuilder::build_sym_typ(pair)?,
            Rule::typ => ASTBuilder::build_typ(pair)?,
            Rule::sym => Expr::Sym(pair.as_str()),
            Rule::dec => Expr::Dec(pair.as_str().parse::<f64>()?),
            Rule::int => Expr::Int(pair.as_str().parse::<i64>()?),
            Rule::str => Expr::Str(pair.as_str()),
            _ => {
                return Err(Error {
                    err_type: ErrorType::ParsingError,
                    start: pair.as_span().start(),
                    end: pair.as_span().end(),
                    msg: format!("invalid token when parsing expr: {}", pair.to_string())
                        .to_string(),
                })
            }
        })
    }

    fn first_pass(pairs: Pairs<Rule>) -> Result<Vec<Expr>, Error> {
        Ok(pairs
            .into_iter()
            .map(ASTBuilder::build_expr)
            .collect::<Result<Vec<_>, _>>()?)
    }

    pub fn build(pairs: Pairs<Rule>) -> Result<Vec<Expr>, Error> {
        ASTBuilder::first_pass(pairs)
    }
}
