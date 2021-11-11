use std::{fmt::Display};

use crate::{span::Span, typ::{BitWidth, ErrorType, Signed, Type, Typed}};

#[derive(Debug, Clone)]
pub enum FunctionType {
    Function,
    Macro,
    Operator
}

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
    Keyword(String),
    Seq(Vec<Token>),
    Body(Vec<Token>),
    List(Vec<Token>),
    Tuple(Vec<(Token, Token)>),
    Quote(Token),
    Quasiquote(Token),
    Unquote(Token),
    Eval(Token),
    Func {
        context: String,
        ast: Token,
        params: Vec<String>,
        fun_type: FunctionType
    },
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

    pub fn with_inner(&self, inner: TokenInner) -> Self {
        Self {
            inner: Box::new(inner),
            span: self.span.clone()
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
            TokenInner::Tuple(seq) => write!(f, "{{{}}}", seq.iter().fold(String::new(), |acc, pair| {
                format!("{} :{} {}", acc, pair.0, pair.1)
            })),
            TokenInner::Quote(t) => write!(f, "'{}", t),
            TokenInner::Quasiquote(t) => write!(f, "`{}", t),
            TokenInner::Unquote(t) => write!(f, "~{}", t),
            TokenInner::Empty => write!(f, "<<Empty>>"),
            TokenInner::EOF => write!(f, "<<EOF>>"),
            TokenInner::Eval(t) => write!(f, "!{}", t),
            TokenInner::Body(bs) => write!(f, "{}", reduce_seq(bs)),
            TokenInner::Func { context, ast, params, fun_type } => write!(f, "<<Function ({}) {}>>", params.join(", "), ast),
            TokenInner::Keyword(k) => write!(f, "<<Keyword {}>>", k),
        }
    }
}