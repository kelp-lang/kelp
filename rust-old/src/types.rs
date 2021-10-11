use std::{borrow::{Borrow, BorrowMut}, fmt::Display};

use crate::ast::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum _KelpVal {
    Int64(i64),
    Float32(f32),
    Bool(bool),
    Str(String),
    Sym{
        name: String,
        typ: Option<KelpVal>,
    },
    Unknown,
    Expr { 
        lhs: KelpVal,
        op: KelpVal,
        rhs: KelpVal
    },
    Lambda {
        arg: KelpVal,
        body: KelpVal,
        ret: KelpVal,
    },
    Body(Vec<KelpVal>),
    Group(Vec<KelpVal>),
    Enum(Vec<KelpVal>),
    Trait(Vec<KelpVal>),
}

#[derive(Debug, Clone)]
pub struct KelpVal {
    inner: Box<_KelpVal>,
    span: Span,
}

impl KelpVal {
    pub fn new(inner: _KelpVal, span: Span) -> Self {
        Self {
            inner: Box::new(inner),
            span: span,
        }
    }
    pub fn inner(&self) -> &_KelpVal {
        self.inner.borrow()
    }
    pub fn inner_mut(&mut self) -> &mut _KelpVal {
        self.inner.borrow_mut()
    }
    pub fn span(&self) -> Span {
        self.span.clone()
    }
}

impl PartialEq for KelpVal {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}

impl Display for KelpVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.inner() {
            _KelpVal::Int64(i) => write!(f, "Int({})", i),
            _KelpVal::Float32(n) => write!(f, "Float({})", n),
            _KelpVal::Bool(b) => write!(f, "Bool({})", b),
            _KelpVal::Str(s) => write!(f, "String({})", s),
            _KelpVal::Sym { name, typ } => write!(f, "Sym({} ({:#?}))", name, typ),
            _KelpVal::Unknown => write!(f, "Unknown"),
            _KelpVal::Expr { lhs, op, rhs } => write!(f, "Expr({} {} {})", lhs, op, rhs),
            _KelpVal::Lambda { arg, body, ret } => write!(f, "Lambda({} -> {}: {})", arg, ret, body),
            _KelpVal::Body(b) => write!(f, "Body({:?})", b),
            _KelpVal::Group(g) => write!(f, "Group({:?})", g),
            _KelpVal::Enum(e) => write!(f, "Enum({:?})", e),
            _KelpVal::Trait(i) => write!(f, "Trait({:?})", i),
        }
    }
}

macro_rules! group {
    ($span:expr, $seq:expr) => {{
        KelpVal::new(_KelpVal::Group($seq), $span)
    }};
    [$span:expr, $($arg:expr),*] => {{
        KelpVal::new(_KelpVal::Group(vec![$($args),*]), $span)
    }}
}