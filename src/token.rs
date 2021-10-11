use std::fmt::Display;

use crate::{span::Span, typ::{BitWidth, ErrorType, Signed, Type, Typed}};

#[derive(Debug, Clone)]
pub struct Token {
    inner: Box<TokenInner>,
    span: Span,
}

#[derive(Debug, Clone)]
pub enum TokenInner {
    Bool(bool),
    Int(i64),
    Float(f32),
    String(String),
    Sym(String),
    Seq(Vec<Token>),
    Body(Vec<Token>),
    List(Vec<Token>),
    Tuple(Vec<Token>),
    Quote(Token),
    Quasiquote(Token),
    Unquote(Token),
    Eval(Token),
    Empty,
    EOF,
}

impl Token {
    pub fn new(inner: TokenInner, span: Span) -> Self {
        Self {
            inner: Box::new(inner),
            span,
        }
    }

    pub fn inner(&self) -> &TokenInner {
        &self.inner
    }

    pub fn span(&self) -> Span {
        self.span.clone()
    }

    pub fn quote(&self) -> Self {
        Self {
            inner: Box::new(TokenInner::Quote(self.clone())),
            span: self.span.clone(),
        }
    }

    pub fn quasiquote(&self) -> Self {
        Self {
            inner: Box::new(TokenInner::Quasiquote(self.clone())),
            span: self.span.clone(),
        }
    }

    pub fn unquote(&self) -> Self {
        Self {
            inner: Box::new(TokenInner::Unquote(self.clone())),
            span: self.span.clone(),
        }
    }

    pub fn eval(&self) -> Self {
        Self {
            inner: Box::new(TokenInner::Eval(self.clone())),
            span: self.span.clone(),
        }
    }
}

macro_rules! list {
    ($seq:expr, $span:expr) => {{
        Token::new(TokenInner::List($seq), $span)
    }};
    ($span:expr, [$($args:expr),*]) => {{
        let v: Vec<Token> = vec![$($args),*];
        Token::new(TokenInner::List($seq), $span)
    }}
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn reduce_seq(seq: &Vec<Token>) -> String {
            seq.iter()
                .fold(String::new(), |acc, s| format!("{} {}", acc, s))
                .to_string()
        }

        match &*self.inner {
            TokenInner::Bool(b) => write!(f, "{}", b),
            TokenInner::Int(i) => write!(f, "{}", i),
            TokenInner::Float(n) => write!(f, "{}", n),
            TokenInner::String(s) => write!(f, "\"{}\"", s),
            TokenInner::Sym(s) => write!(f, "<<Symbol '{}'>>", s),
            TokenInner::Seq(seq) => write!(f, "({})", reduce_seq(seq)),
            TokenInner::List(seq) => write!(f, "[{}]", reduce_seq(seq)),
            TokenInner::Tuple(seq) => write!(f, "{{{}}}", reduce_seq(seq)),
            TokenInner::Quote(t) => write!(f, "'{}", t),
            TokenInner::Quasiquote(t) => write!(f, "`{}", t),
            TokenInner::Unquote(t) => write!(f, "~{}", t),
            TokenInner::Empty => write!(f, "<<Empty>>"),
            TokenInner::EOF => write!(f, "<<EOF>>"),
            TokenInner::Eval(t) => write!(f, "!{}", t),
            TokenInner::Body(bs) => write!(f, "{}", reduce_seq(bs))
        }
    }
}

impl Typed for Token {
    fn typ(&self) -> Type {
        match self.inner() {
            TokenInner::Bool(_) => Type::Bool,
            TokenInner::Int(_) => Type::Int(BitWidth::SixtyFour, Signed::Signed),
            TokenInner::Float(_) => Type::Float(BitWidth::SixtyFour, Signed::Signed),
            TokenInner::String(_) => Type::String,
            TokenInner::Sym(_) => Type::Symbol,
            //FIXME: This is not how type should be determined
            TokenInner::Seq(e) => Type::Sequence(e.iter().map(|e| e.typ()).collect()),
            TokenInner::Body(b) => b.last().unwrap().typ(),
            TokenInner::List(a) => Type::List(a.iter().map(|a| a.typ()).collect()),
            TokenInner::Tuple(t) => Type::Tuple(
                t.chunks(2)
                    .map(|t| match t[0].inner() {
                        TokenInner::Sym(s) => (s.clone(), t[1].typ()),
                        _ => {
                            let span = t[0].span();
                            error!(span, "Expected symbol, got '{:?}'", t[0]);
                            ("unknown".to_string(), Type::Error(ErrorType::TupleKey))
                        }
                    })
                    .collect(),
            ),
            TokenInner::Quote(q) => Type::Quote(Box::new(q.typ())),
            TokenInner::Quasiquote(q) => Type::Quasiquote(Box::new(q.typ())),
            TokenInner::Unquote(u) => Type::Unquote(Box::new(u.typ())),
            TokenInner::Eval(e) => Type::Eval(Box::new(e.typ())),
            TokenInner::Empty => Type::Empty,
            TokenInner::EOF => Type::Empty,
        }
    }
}
