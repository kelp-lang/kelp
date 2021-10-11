use std::fmt::Display;

use crate::{span::Span, token::{Token, TokenInner}, typ::{BitWidth, Signed, Type, Typed}};

#[derive(Debug, Clone)]
pub struct Node {
    inner: Box<NodeInner>,
    // NOTE: I actually probably need types lol
    typ: Type,
    span: Span,
}

#[derive(Debug, Clone)]
pub enum NodeInner {
    FunCall {
        function: String,
        params: Vec<Node>
    },
    PartialFunCall {
        function: String,
        params: Vec<Node>,
        missing_params: usize
    },
    Lambda {
        params: Vec<Node>,
        seq: Node,
        context_id: String
    },
    Symbol(String),
    // OperatorCall {
    //     lhs: Node,
    //     op: Node,
    //     rhs: Node,
    // },
    DefSymCall {
        symbol: String,
        value: Node,
    },
    SetCall {
        symbol: String,
        value: Node,
    },
    GetCall {
        symbol: String,
    },
    MacroDef {
        params: Vec<String>,
        seq: Node,
    },
    Int(i64),
    Float(f32),
    String(String),
    Bool(bool),
    Sequence(Vec<Node>),
    Body(Vec<Node>),
    List(Vec<Node>),
    UncheckedTuple(Vec<Node>),
    Tuple(Vec<(Node, Node)>),
    KeyValPair(Node, Node),
    Quote(Node),
    Quasiquote(Node),
    Unquote(Node),
    Eval(Node),
    Empty,
}

impl Node {
    pub fn unknown(span: Span) -> Self {
        Self {
            inner: Box::new(NodeInner::Empty),
            span: span,
            typ: Type::Unknown,
        }
    }

    pub fn with_type(&self, typ: Type) -> Self {
        Self {
            inner: self.inner.clone(),
            typ: typ,
            span: self.span.clone(),
        }
    }

    pub fn with_inner(&self, inner: NodeInner) -> Self {
        Self {
            inner: Box::new(inner.clone()),
            typ: self.typ.clone(),
            span: self.span.clone(),
        }
    }

    pub fn with_span(&self, span: Span) -> Self {
        Self {
            inner: self.inner.clone(),
            typ: self.typ.clone(),
            span: span,
        }
    }
    
    pub fn inner(&self) -> &NodeInner {
        &self.inner
    }

    pub fn span(&self) -> Span {
        self.span.clone()
    }

    pub fn typ(&self) -> &Type {
        &self.typ
    }

    pub fn quote(&self) -> Self {
        Self {
            inner: Box::new(NodeInner::Quote(self.clone())),
            typ: Type::Quote(Box::new(self.typ.clone())),
            span: self.span.clone(),
        }
    }

    pub fn quasiquote(&self) -> Self {
        Self {
            inner: Box::new(NodeInner::Quasiquote(self.clone())),
            typ: Type::Quote(Box::new(self.typ.clone())),
            span: self.span.clone(),
        }
    }
}

impl Typed for Node {
    fn typ(&self) -> Type {
        self.typ.clone()
    }
}

impl From<Token> for Node {
    fn from(token: Token) -> Self {
        let inner = match token.inner() {
            TokenInner::Bool(b) => NodeInner::Bool(b.clone()),
            TokenInner::Int(i) => NodeInner::Int(i.clone()),
            TokenInner::Float(f) => NodeInner::Float(f.clone()),
            TokenInner::String(s) => NodeInner::String(s.clone()),
            TokenInner::Sym(s) => NodeInner::Symbol(s.clone()),
            TokenInner::Seq(s) => NodeInner::Sequence(s.iter().map(|s| s.clone().into()).collect()),
            TokenInner::Body(b) => NodeInner::Body(b.iter().map(|b| b.clone().into()).collect()),
            // TODO: Arrays
            TokenInner::List(s) => NodeInner::List(s.iter().map(|s| s.clone().into()).collect()),
            TokenInner::Tuple(t) => NodeInner::UncheckedTuple(t.iter().map(|t| t.clone().into()).collect()),
            TokenInner::Quote(q) => NodeInner::Quote(q.clone().into()),
            TokenInner::Quasiquote(q) => NodeInner::Quasiquote(q.clone().into()),
            TokenInner::Unquote(u) => NodeInner::Unquote(u.clone().into()),
            TokenInner::Eval(e) => NodeInner::Eval(e.clone().into()),
            TokenInner::Empty => NodeInner::Empty,
            TokenInner::EOF => NodeInner::Empty,
        };

        Self {
            inner: Box::new(inner),
            typ: token.typ(),
            span: token.span(),
        }
    }
}

macro_rules! sequence {
    ($seq:expr) => {{
        NodeInner::Sequence($seq)
    }};
    [$($args:expr),*] => {{
        let v: Vec<Node> = vec![$($args),*];
        NodeInner::Sequence(v)
    }}
}

pub fn get_span(vec: &Vec<Node>) -> Span {
    if vec.len() > 0 {
        Span::new(vec[0].span().start, vec[vec.len() - 1].span().end, vec[0].span().path, vec[0].span().content)
    } else {
        Span::default()
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.inner() {
            NodeInner::FunCall { function, params } => write!(f, "{}({})", function, params.iter().map(|p| {format!("{}", p)}).collect::<Vec<_>>().join(",")),
            NodeInner::PartialFunCall { function, params, missing_params } => write!(f, "~{}({}) /{}", function, params.iter().map(|p| {format!("{}", p)}).collect::<Vec<_>>().join(","), missing_params),
            NodeInner::Lambda { params, seq, .. } => write!(f, "\\{} -> {}", params.iter().map(|p| {format!("{}", p)}).collect::<Vec<_>>().join(","), seq),
            NodeInner::Symbol(s) => write!(f, "'{}'", s),
            NodeInner::DefSymCall { symbol, value } => write!(f, "def {} {}", symbol, value),
            NodeInner::SetCall { symbol, value } => write!(f, "set {} {}", symbol, value),
            NodeInner::GetCall { symbol } => write!(f, "get {}", symbol),
            NodeInner::MacroDef { params, seq } => write!(f, "\\m{} -> {}", params.iter().map(|p| {format!("{}", p)}).collect::<Vec<_>>().join(","), seq),
            NodeInner::Int(i) => write!(f, "{}", i),
            NodeInner::Float(i) => write!(f, "{}", i),
            NodeInner::String(s) => write!(f, "\"{}\"", s),
            NodeInner::Bool(b) => write!(f, "{}", b),
            NodeInner::Sequence(ns) => write!(f, "({})", ns.iter().map(|n| {format!("{}", n)}).collect::<Vec<_>>().join(" ")),
            NodeInner::Body(bs) => write!(f, "{}", bs.iter().map(|b| {format!("{}", b)}).collect::<Vec<_>>().join("\n")),
            NodeInner::List(ls) => write!(f, "[{}]", ls.iter().map(|n| {format!("{}", n)}).collect::<Vec<_>>().join(",")),
            NodeInner::UncheckedTuple(_) => todo!(),
            NodeInner::Tuple(_) => todo!(),
            NodeInner::KeyValPair(_, _) => todo!(),
            NodeInner::Quote(n) => write!(f, "quote {}", n),
            NodeInner::Quasiquote(_) => todo!(),
            NodeInner::Unquote(_) => todo!(),
            NodeInner::Eval(n) => write!(f, "eval {}", n),
            NodeInner::Empty => write!(f, "empty"),
        }
    }
}