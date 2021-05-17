use kelp_derive::Spans;
use kelp_span::{Span, Spans};

use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone, Debug, Spans)]
pub struct Variable {
    pub kind: VariableKind,
    pub span: Span,
    pub is_corrupted: bool,
}

#[derive(Clone, Debug)]
pub enum VariableKind {
    Lambda(Lambda),
    Literal(Literal),
    Group(Vec<Expr>),
    Symbol(String),
    SymbolType(String, Type),
    Object(Object),
    Body(Vec<Expr>),
    Corrupted,
}

#[derive(Clone, Debug, Spans)]
pub struct Lambda {
    pub args: Vec<Variable>,
    pub ret: Type,
    pub body: Vec<Expr>,
    pub span: Span,
    pub is_corrupted: bool,
}

#[derive(Clone, Debug, Spans)]
pub struct Object {
    pub body: Vec<Expr>,
    pub span: Span,
    pub is_corrupted: bool,
}

#[derive(Clone, Debug, Spans)]
pub struct Type {
    pub kind: Rc<RefCell<TypeKind>>,
    pub span: Span,
}

#[derive(Debug)]
pub enum TypeKind {
    Function(Type, Type),
    Object(Vec<Type>),
    Simple(String),
}

#[derive(Clone, Debug)]
pub enum Literal {
    Int(i32),
    Real(f32),
    Str(String),
    Char(char),
    Bool(bool),
}

#[derive(Clone, Debug, Spans)]
pub struct Expr {
    pub lhs: Variable,
    pub operator: Option<Variable>,
    pub rhs: Option<Variable>,
    pub span: Span,
    pub is_corrupted: bool,
}
