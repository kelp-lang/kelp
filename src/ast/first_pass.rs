use std::rc::Rc;

use crate::{
    ast::*,
    error::{Error, ErrorType},
    parser::Rule,
};
use pest::iterators::Pair;

fn build_fun_typ(pair: Pair<Rule>) -> Result<Expr, Error> {
    let mut pairs = pair.into_inner();
    Ok(Expr::FunTyp(
        build_fields(pairs.next().unwrap())?,
        Rc::new(build_typ(pairs.next().unwrap())?),
    ))
}

fn build_typ(pair: Pair<Rule>) -> Result<Expr, Error> {
    Ok(match pair.as_rule() {
        Rule::sym => Expr::Typ(pair.as_str()),
        //Rule::typ => ASTBuilder::build_typ(pair)?,
        Rule::fun_typ => build_fun_typ(pair)?,
        _ => {
            return Err(Error::default()
                .with_type(ErrorType::ParsingError)
                .with_position(pair.as_span().start(), pair.as_span().end())
                .with_message(
                    format!("invalid token when parsing type: {}", pair.to_string()).to_string(),
                )
                .build())
        }
    })
}

fn build_sym_typ(pair: Pair<Rule>) -> Result<Expr, Error> {
    let mut pairs = pair.into_inner();
    Ok(Expr::SymTyp(
        pairs.next().unwrap().as_str(),
        Rc::new(build_typ(pairs.next().unwrap())?),
    ))
}

fn build_field(pair: Pair<Rule>) -> Result<Expr, Error> {
    match pair.as_rule() {
        Rule::field => build_expr(pair.into_inner().next().unwrap()),
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

fn build_fields(pair: Pair<Rule>) -> Result<Vec<Expr>, Error> {
    Ok(pair
        .into_inner()
        .map(build_field)
        .collect::<Result<Vec<_>, _>>()?)
}

fn build_lambda(pair: Pair<Rule>) -> Result<Expr, Error> {
    let mut pairs = pair.into_inner();
    let a1 = pairs.next().unwrap();
    let (is_async, fun_typ) = match pairs.next().unwrap().as_rule() {
        Rule::is_async => (true, build_fun_typ(pairs.next().unwrap())?),
        _ => (false, build_fun_typ(a1)?),
    };
    Ok(Expr::Lambda {
        is_async,
        fun_typ: Rc::new(fun_typ),
        body: build_fun_bod(pairs.next().unwrap())?,
    })
}

fn build_fun_bod(pair: Pair<Rule>) -> Result<Vec<Expr>, Error> {
    Ok(pair
        .into_inner()
        .map(build_expr)
        .collect::<Result<Vec<_>, _>>()?)
}

fn build_fun_blk(pair: Pair<Rule>) -> Result<Expr, Error> {
    let mut pairs = pair.into_inner();
    Ok(Expr::FunBlk(
        Rc::new(build_expr(pairs.next().unwrap())?),
        build_fun_bod(pairs.next().unwrap())?,
    ))
}

fn build_def_val(pair: Pair<Rule>) -> Result<Expr, Error> {
    let pair = pair.into_inner().next().unwrap();
    Ok(match pair.as_rule() {
        Rule::def => build_def(pair)?,
        Rule::op => Expr::UnresolvedOp(pair.clone()),
        //Rule::fun_blk => ASTBuilder::build_fun_blk(pair)?,
        //Rule::def_fun => ASTBuilder::build_lambda(pair)?,
        //Rule::def_val => ASTBuilder::build_def_val(pair)?,
        Rule::group => Expr::Group(build_fields(pair)?),
        Rule::sym_typ => build_sym_typ(pair)?,
        Rule::typ => build_typ(pair)?,
        Rule::sym => Expr::Sym(pair.as_str()),
        Rule::dec => Expr::Dec(pair.as_str().parse::<f64>()?),
        Rule::int => Expr::Int(pair.as_str().parse::<i64>()?),
        Rule::str => Expr::Str(pair.as_str()),
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

fn build_def_sym<'a>(
    sym: Pair<'a, Rule>,
    operator: Pair<'a, Rule>,
    rest: Pair<'a, Rule>,
) -> Result<Expr<'a>, Error> {
    Ok(match operator.as_str() {
        "=" => Expr::Def(Rc::new(build_expr(sym)?), Rc::new(build_expr(rest)?)),
        &_ => Expr::DefWOp(
            Rc::new(build_expr(sym)?),
            operator.as_str(),
            Rc::new(build_expr(rest)?),
        ),
    })
}

fn build_def_operator<'a>(
    operator_to_def: Pair<'a, Rule>,
    operator: Pair<'a, Rule>,
    rest: Pair<'a, Rule>,
) -> Result<Expr<'a>, Error> {
    Ok(match operator.as_str() {
        "=" => Expr::DefOp(operator_to_def.as_str(), Rc::new(build_fun_blk(rest)?)),
        "<<" => Expr::DefOpApp(operator_to_def.as_str(), Rc::new(build_lambda(rest)?)),
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

fn build_def(pair: Pair<Rule>) -> Result<Expr, Error> {
    let mut pairs = pair.clone().into_inner();
    let sym_or_operator = pairs.next().unwrap();
    Ok(match sym_or_operator.as_rule() {
        Rule::sym => build_def_sym(
            sym_or_operator,
            pairs.next().unwrap(),
            pairs.next().unwrap(),
        )?,
        Rule::operator_def => build_def_operator(
            sym_or_operator,
            pairs.next().unwrap(),
            pairs.next().unwrap(),
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

pub(super) fn build_expr(pair: Pair<Rule>) -> Result<Expr, Error> {
    Ok(match pair.as_rule() {
        Rule::def => build_def(pair)?,
        Rule::op => Expr::UnresolvedOp(pair.clone()),
        Rule::fun_blk => build_fun_blk(pair)?,
        Rule::def_fun => build_lambda(pair)?,
        Rule::def_val => build_def_val(pair)?,
        Rule::group => Expr::Group(build_fields(pair)?),
        Rule::sym_typ => build_sym_typ(pair)?,
        Rule::typ => build_typ(pair)?,
        Rule::sym => Expr::Sym(pair.as_str()),
        Rule::dec => Expr::Dec(pair.as_str().parse::<f64>()?),
        Rule::int => Expr::Int(pair.as_str().parse::<i64>()?),
        Rule::str => Expr::Str(pair.as_str()),
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
