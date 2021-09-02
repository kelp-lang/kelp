use std::{cell::RefCell, collections::HashSet, rc::Rc};

use kelp_message::Error;
use kelp_origin::Origin;
use kelp_stree::{Expr, Variable};

struct OriginContext {
    parent_context_store: Rc<RefCell<OriginContext>>,
    origins: HashSet<Origin>,
    children_context_stores: Vec<Rc<RefCell<OriginContext>>>,
    object_context_stores: Vec<Rc<RefCell<OriginContext>>>,
}

impl OriginContext {
    pub fn new(parent_context_store: Rc<RefCell<OriginContext>>) -> Self {
        Self {
            parent_context_store: parent_context_store,
            origins: HashSet::new(),
            children_context_stores: vec![],
            object_context_stores: vec![],
        }
    }
}

pub struct OriginResolver {
    root_context_store: OriginContext,
}

impl OriginResolver {
    pub fn resolve(&mut self, file_origin: Origin, stree: Variable) -> Result<Self, Error> {
        todo!()
    }

    fn resolve_variable(&mut self, parent_origin: Origin, variable: Variable) -> Origin {
        todo!()
    }
}
