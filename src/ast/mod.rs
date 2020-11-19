mod ast_builder;
mod span;

pub use crate::ast::ast_builder::ASTBuilder;
pub use crate::ast::span::Span;

use pest::iterators::Pair;
use std::cell::RefCell;
use std::rc::Rc;

use crate::operator::Operator;

#[macro_export]
macro_rules! corrupt_expr {
    ($e:expr,$d:expr) => {{
        match $e {
            Ok(r) => r,
            Err(e) => {
                $d.dispatch(e);
                crate::ast::Expr::corrupted()
            }
        }
    }};
}

#[macro_export]
macro_rules! corrupt_vec {
    ($e:expr, $d:expr) => {{
        match $e {
            Ok(r) => r,
            Err(e) => {
                $d.dispatch(e);
                vec![crate::ast::Expr::corrupted()]
            }
        }
    }};
}

#[macro_export]
macro_rules! parse_unwrap {
    ($e:expr,$m:literal,$s:ident) => {{
        match $e {
            Some(s) => s,
            None => {
                let err = crate::message::Error::default()
                    .with_message($m.to_string())
                    .with_type(crate::message::ErrorType::ParsingError)
                    .with_span($s)
                    .build();

                return Err(err);
            }
        }
    }};
}

//pub type AST = Expr;
pub type ExprRef = Rc<RefCell<Expr>>;

#[derive(Debug, Clone)]
struct _Expr {
    //uuid: Uuid,
    pub inner: ExprKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    // let {symtyp} {operator} {value}
    Def(Expr, Expr, Expr),
    // let [{operator}] = {FunBlk}
    // DefOp(Expr, Expr),
    // let {operator fun name} {operator} {value}
    //OpFun(Expr, Expr, Expr),
    // {value} {operator} {value}
    Op(Expr, Expr, Expr),
    // this resolves to Body full of Ops once operators are parsed
    UnresolvedOp(String),
    // {name} ({expressions})
    FunBlk(Expr, Vec<Expr>),
    // [{params}] ({expressions})
    Lambda(Expr, Vec<Expr>),
    Sym(String),
    // {symbol}: {type}
    SymTyp(String, Expr),
    // [{args}]: {return_type}
    FunTyp(Vec<Expr>, Expr),
    // {sym}
    Typ(String),
    // [{fields}]
    Group(Vec<Expr>),
    Int(i64),
    Dec(f64),
    Str(String),
    Body(Vec<Expr>),
    Operator(Operator),
    // this is the inner value, when branch returned error
    Corrupted,
}

#[derive(Debug, Clone)]
pub struct Expr(Rc<RefCell<_Expr>>);

impl Expr {
    pub fn new(kind: ExprKind, span: Span) -> Self {
        let expr = _Expr { inner: kind, span };

        Expr(Rc::new(RefCell::new(expr)))
    }
    pub fn corrupted() -> Self {
        let expr = _Expr {
            inner: ExprKind::Corrupted,
            span: Span::default(),
        };

        Expr(Rc::new(RefCell::new(expr)))
    }
    pub fn kind(&self) -> ExprKind {
        self.0.borrow().clone().inner
    }
    pub fn set_kind(&mut self, kind: ExprKind) {
        self.0.borrow_mut().inner = kind;
    }
    pub fn span(&self) -> Span {
        self.0.borrow().clone().span
    }

    pub fn is_corrupted(&self) -> bool {
        match (self.0.borrow()).inner {
            ExprKind::Corrupted => true,
            _ => false,
        }
    }
}
