use std::{cell::RefCell, collections::HashMap, rc::Rc};

use kelp_origin::Origin;

pub struct Type {
    pub name: Origin,
    pub kind: Rc<RefCell<TypeKind>>,
    pub memsize: usize,
}

pub enum TypeKind {
    Int,
    Float,
    Boolean,
    String,
    Lambda(Type, Type),
    Void,
    Complex(HashMap<String, Type>),
    Group(Vec<Type>),
    Memsize(usize),
}
