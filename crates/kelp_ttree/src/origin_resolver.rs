use std::{cell::RefCell, collections::HashSet, rc::Rc};

use kelp_message::Error;
use kelp_origin::Origin;
use kelp_stree::{Expr, Variable};

struct OriginContext {
    parent_context: Rc<RefCell<OriginContext>>,
    origins: HashSet<Origin>,
    children_contexts: Vec<Rc<RefCell<OriginContext>>>,
    object_contexts: Vec<Rc<RefCell<OriginContext>>>,
}

impl OriginContext {
    pub fn new(parent_context: Rc<RefCell<OriginContext>>) -> Self {
        Self {
            parent_context: parent_context,
            origins: HashSet::new(),
            children_contexts: vec![],
            object_contexts: vec![],
        }
    }
}

pub struct OriginResolver {
    root_context: OriginContext,
}

impl OriginResolver {
    pub fn resolve(&mut self, file_origin: Origin, stree: Variable) -> Result<Self, Error> {
        todo!()
    }

    fn resolve_variable(&mut self, parent_origin: Origin, variable: Variable) -> Origin {
        todo!()
    }
}
