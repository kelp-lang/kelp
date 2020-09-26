use std::collections::HashMap;

pub struct Sym {
    pub(crate) sym: String,
    pub(crate) typ: Option<Type>,
}

#[derive(Clone)]
pub enum Type {
    Simp(String),
    Fun { arg: Box<Type>, ret: Box<Type> },
    Comp(Vec<Type>),
    None,
}

pub enum OpFun {
    Add,
    Sub,
    Mul,
    Div,
    Grt,
    Get,
    Smt,
    Set,
    Map,
    Ass,
    App,
}

pub struct Expr {
    left: Elem,
    right: Elem,
}

pub struct Op {
    left: Elem,
    right: Elem,
    op: OpFun,
}

pub enum Elem {
    Sym(Sym),
    //SymType { sym: Sym, typ: Type },
    Float(f32),
    Int(i32),
    String(String),
    Group(Vec<Elem>),
}

pub struct FunArg {
    args: Vec<Elem>,
    cond: Vec<Op>,
}

pub struct FunDec {
    name: Sym,
    arg: Type,
    ret: Type,
    defs: HashMap<Vec<Elem>, FunDef>,
}

pub struct FunDef {
    exprs: Vec<Expr>,
}

#[derive(Default)]
pub struct Program {
    pub(crate) funcs: Vec<FunDec>,
}
