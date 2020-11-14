mod first_pass;
mod second_pass;

use pest::iterators::Pair;
use pest::Parser;
use std::rc::Rc;

use crate::operator::build_operators;
use crate::{error::Error, operator::OperatorList, parser::Rule};
use crate::parser::KelpParser;
use crate::operator::Operator;

#[macro_export]
macro_rules! corrupt_expr {
    ($e:expr,$a:ident) => {{
        match $e {
            Ok(r) => r,
            Err(e) => {
                $a.had_error = true;
                eprintln!("{}", e);
                crate::ast::Expr::Corrupt
            }
        }
    }};
}

#[macro_export]
macro_rules! corrupt_vec {
    ($e:expr, $a:ident) => {{
        match $e {
            Ok(r) => r,
            Err(e) => {
                $a.had_error = true;
                eprintln!("{}", e);
                vec![crate::ast::Expr::Corrupt]
            }
        }
    }};
}

#[macro_export]
macro_rules! parse_unwrap {
    ($e:expr,$m:literal,$a:ident) => {{
        match $e {
            Some(s) => s,
            None => {
                $a.had_error = true;
                let err = crate::error::Error::default()
                    .with_message($m.to_string())
                    .with_type(crate::error::ErrorType::ParsingError)
                    .build();

                return Err(err);
            }
        }
    }};
}

#[derive(Debug, Clone)]
pub struct ASTBuilder {
    ast: Option<AST>,
    parse_tree: Option<Pair<'static, Rule>>,
    operator_list: Option<OperatorList>,
    had_error: bool,
}

pub type AST = Expr;

#[derive(Debug, Clone)]
pub enum Expr {
    Def(Rc<Expr>, Rc<Expr>), // def {name + type?} = {value}
    DefWOp(Rc<Expr>, Operator, Rc<Expr>), // def {name + type?} [{operator}] {Lambda/Value}
    DefOp(Operator, Rc<Expr>),   // def [{operator}] = {FunBlk}
    DefOpApp(Operator, Rc<Expr>), // def [{operator}] << {Lamba}
    Op(Vec<(Expr, Operator, Expr)>), // not going to touch this
    UnresolvedOp(String),    // neither this
    FunBlk(Rc<Expr>, Vec<Expr>), // {name} ({expressions})
    Lambda {
        fun_typ: Rc<Expr>,
        body: Vec<Expr>,
    },
    Sym(String),
    SymTyp(String, Rc<Expr>),        // {symbol}: {type}
    FunTyp(Vec<Expr>, Rc<Expr>), // [{args}]: {return_type}
    Typ(String),
    Group(Vec<Expr>),
    Int(i64),
    Dec(f64),
    Str(String),
    Root(Vec<Expr>),
    Corrupt,
}

type Sym = String;

impl ASTBuilder {
    pub fn add_parse_tree(&mut self, input: &'static str) -> Result<&mut Self, Error> {
        self.parse_tree = Some(parse_unwrap!(KelpParser::parse(Rule::root, input)?.next(), "missing root, the file is not valid/empty", self));
        Ok(self)
    }

    pub fn first_pass(&mut self) -> &mut Self {
        self.ast = Some(Expr::Root(
            self.clone()
                .parse_tree
                .expect("no parse tree")
                .into_inner()
                .map(|expr| corrupt_expr!(self.build_expr(expr), self))
                .collect(),
        ));

        self
    }

    pub fn build_operators(&mut self) -> &mut Self {
        self.operator_list = Some(match build_operators(
            self.ast
                .clone()
                .expect("first pass wasn't done, this is probably a bug"),
        ) {
            Ok(operator_list) => operator_list,
            Err(e) => {
                self.had_error = true;
                eprintln!("{}", e);
                return self;
            }
        });
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
            had_error: ast.had_error,
        }
    }

    pub fn get_ast(&self) -> AST {
        self.ast.clone().unwrap()
    }
}

impl Default for ASTBuilder {
    fn default() -> Self {
        Self {
            ast: None,
            parse_tree: None,
            operator_list: None,
            had_error: false,
        }
    }
}
