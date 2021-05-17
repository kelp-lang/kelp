mod parse_tree;

use kelp_derive::Spans;
use kelp_span::{Span, Spans};

use std::fmt::Display;
use std::rc::Rc;
use std::{cell::RefCell, fmt::Debug};

pub use parse_tree::ParseTree;

#[macro_export]
macro_rules! corrupt_expr {
    ($e:expr,$d:expr) => {{
        match $e {
            Ok(r) => r,
            Err(e) => {
                $d.dispatch(e, MessageLevel::Error);
                crate::Expr::corrupted()
            }
        }
    }};
}

#[macro_export]
macro_rules! corrupt_typ {
    ($e:expr,$d:expr) => {{
        match $e {
            Ok(r) => r,
            Err(e) => {
                $d.dispatch(e, MessageLevel::Error);
                crate::Typ::Corrupted
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
                $d.dispatch(e, MessageLevel::Error);
                vec![crate::ast::Expr::corrupted()]
            }
        }
    }};
}

#[macro_export]
macro_rules! parse_unwrap {
    ($e:expr,$m:expr,$s:ident) => {{
        match $e {
            Some(s) => s,
            None => {
                let err = Error::default()
                    .with_message($m.to_string())
                    .with_type(ErrorType::ParsingError)
                    .with_span($s)
                    .build();

                return Err(err);
            }
        }
    }};
}

#[derive(Debug, Clone, Spans)]
struct _Expr {
    //uuid: Uuid,
    pub inner: ExprKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    /// {lhs} {operator} {rhs}
    Expression(Expr, Expr, Expr),
    /// {fun_typ} {body}
    Lambda(Typ, Expr),
    /// {symbol}
    Sym(String),
    /// {symbol}: {typ}
    SymTyp(String, Typ),
    /// {typ}
    Typ(Typ),
    /// elements
    Group(Vec<Expr>),
    /// Literal value
    Lit(Literal),
    /// {expresions}
    Body(Vec<Expr>),
    /// {expresions}
    Object(Expr),
    /// {operator}
    Operator(String),
    /// this is the inner value, when branch returned error
    Corrupted,
}

#[derive(Debug, Clone)]
pub enum Literal {
    Int(i32),
    Real(f32),
    Str(String),
    Char(char),
    Bool(bool),
}

#[derive(Debug, Clone)]
pub enum Typ {
    Simple(String),
    Group(Expr),
    Fun(Expr, Expr),
    Unknown,
    Corrupted,
}

impl Display for ExprKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let output = match self {
            ExprKind::Lambda(fun_typ, exprs) => {
                format!("{} {}", fun_typ, exprs)
            }
            ExprKind::Sym(sym) => sym.to_string(),
            ExprKind::SymTyp(sym, typ) => {
                if let Typ::Unknown = typ {
                    format!("{}", sym)
                } else {
                    format!("{}: {}", sym, typ)
                }
            }
            ExprKind::Typ(typ) => format!("{}", typ),
            ExprKind::Group(fields) => format!("{}", print_group(fields)),
            ExprKind::Body(exprs) => format!("{}", print_body(exprs)),
            ExprKind::Operator(operator) => operator.to_string(),
            ExprKind::Corrupted => "!@#$".to_string(),
            ExprKind::Lit(literal) => {
                format!("{}", literal)
            }
            ExprKind::Expression(lhs, op, rhs) => format!("{} {} {}", lhs, op, rhs),
            ExprKind::Object(expr) => format!(
                "{{\n{}\n}}",
                match expr.kind() {
                    ExprKind::Body(ref exprs) => print_object_body(exprs),
                    _ => format!("{}", expr),
                }
            ),
        };
        write!(f, "{}", output)
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind())
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Int(i) => write!(f, "{}", i),
            Literal::Real(r) => write!(f, "{}", r),
            Literal::Str(s) => write!(f, "{}", s),
            Literal::Char(c) => write!(f, "{}", c),
            Literal::Bool(b) => write!(f, "{}", b),
        }
    }
}

impl Display for Typ {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let output = match self {
            Typ::Simple(sym) => sym.to_string(),
            Typ::Group(group) => format!("{}", group),
            Typ::Fun(args, ret) => format!("{} -> {}", args, ret),
            Typ::Corrupted => "!@#$".to_string(),
            Typ::Unknown => "[]".to_string(),
        };
        write!(f, "{}", output)
    }
}

fn print_group<T>(exprs: &Vec<T>) -> String
where
    T: Display,
{
    let mut group = "[".to_string();
    for field in exprs {
        group = format!("{}, {}", group, field);
    }
    group = format!("{}]", group);
    group
}

fn print_body<T>(exprs: &Vec<T>) -> String
where
    T: Display,
{
    let mut body = "(\n".to_string();
    for field in exprs {
        body = format!("{}  {}\n", body, field);
    }
    body = format!("{})", body);
    body
}

fn print_object_body<T>(exprs: &Vec<T>) -> String
where
    T: Display,
{
    let mut object_body = "{\n".to_string();
    for expr in exprs {
        object_body = format!("{}   {}\n", object_body, expr);
    }
    object_body = format!("{}}}", object_body);
    object_body
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
