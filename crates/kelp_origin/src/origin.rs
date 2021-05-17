use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct Origin {
    pub name: String,
    pub kind: OriginKind,
    parent: Option<Rc<RefCell<Origin>>>,
}

impl Origin {
    fn new(name: String, kind: OriginKind, parent: Option<Origin>) -> Self {
        let parent = if let Some(parent) = parent {
            Some(Rc::new(RefCell::new(parent)))
        } else { None };
        Self {
            name,
            kind,
            parent,
        }
    }
}

impl Default for Origin {
    fn default() -> Self {
        Self {
            name: "".to_string(),
            kind: OriginKind::Root,
            parent: None,
        }
    }
}

#[derive(Clone, Debug)]
pub enum OriginKind {
    Directory,
    File,
    Function,
    Object,
    Variable,
    Root,
}
