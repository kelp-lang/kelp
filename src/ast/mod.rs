mod first_pass;
use std::rc::Rc;

use crate::{error::Error, parser::Rule};
use pest::iterators::Pair;

#[derive(Debug, Clone)]
pub struct ASTBuilder<'a> {
    ast: Option<AST<'a>>,
    parse_tree: Option<Pair<'a, Rule>>,
}

pub type AST<'a> = Expr<'a>;

#[derive(Debug, Clone)]
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
    Root(Vec<Expr<'a>>),
}

type Operator<'a> = &'a str;
type Sym<'a> = &'a str;

impl<'a> ASTBuilder<'a> {
    pub fn add_parse_tree(&mut self, pair: Pair<'a, Rule>) -> &mut Self {
        self.parse_tree = Some(pair);
        self
    }

    pub fn first_pass(&mut self) -> Result<&mut Self, Error> {
        self.ast = Some(Expr::Root(
            self.clone()
                .parse_tree
                .expect("no parse tree")
                .into_inner()
                .map(first_pass::build_expr)
                .collect::<Result<Vec<_>, _>>()?,
        ));

        Ok(self)
    }

    pub fn second_pass(&mut self) -> Result<&mut Self, Error> {
        panic!();
    }

    pub fn build(&mut self) -> Self {
        let ast = std::mem::take(self);
        ASTBuilder {
            ast: ast.ast,
            parse_tree: ast.parse_tree,
        }
    }

    pub fn get_ast(&self) -> AST {
        self.ast.clone().unwrap()
    }
}

impl<'a> Default for ASTBuilder<'a> {
    fn default() -> Self {
        Self {
            ast: None,
            parse_tree: None,
        }
    }
}
