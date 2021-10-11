use std::{borrow::Borrow, collections::HashMap, sync::Mutex};

use lazy_static::lazy_static;

use crate::{builtins::get_builtin_context,typer::KelpType};

pub type KelpId = usize;

lazy_static! {
    static ref ID_COUNTER: Mutex<KelpId> = Mutex::new(0);
}

pub fn get_new_id() -> KelpId {
    let mut counter = ID_COUNTER.lock().expect("unable to lock id counter");
    *counter += 1;
    *counter - 1
}

#[derive(Debug)]
pub struct ContextStore {
    pub map: HashMap<String, Context>,
    pub current_context: String,
    pub possible_child_context: Option<String>,
}

#[derive(Debug, Clone)]
pub struct Context {
    pub symbol_map: HashMap<String, KelpType>,
    pub parent_context: Option<String>,
    pub children_contexts: Vec<String>,
    pub inserted_contexts: Vec<String>,
}

impl Context {
    fn new(parent_context_id: Option<String>) -> Self {
        Context {
            symbol_map: HashMap::new(),
            parent_context: parent_context_id,
            children_contexts: Vec::new(),
            inserted_contexts: Vec::new(),
        }
    }
}

impl ContextStore {
    pub fn new() -> Self {
        let mut map = HashMap::new();
        map.insert(":builtin".to_string(), get_builtin_context());
        map.insert(":root".to_string(), Context::new(None));
        let mut this = Self {
            map,
            current_context: ":root".to_string(),
            possible_child_context: None,
        };

        this.insert_context(":builtin".to_string());

        this
    }
    fn search_symbol_type(&self, context_id: &str, name: &str) -> Option<KelpType> {
        let context = self.map[context_id].borrow();
        if let Some(t) = context.symbol_map.get(name) {
            Some(t.clone())
        } else {
            let inserted = context
                .inserted_contexts
                .iter()
                .filter_map(|context_id| self.search_symbol_type(context_id, name))
                .collect::<Vec<_>>();

            if inserted.len() == 1 {
                Some(inserted[0].clone())
            } else if inserted.len() > 1 {
                error!(
                    "There are multiple definitions of this symbol included {:#?}",
                    inserted
                );
                None
            } else if let Some(parent_context_id) = &context.parent_context {
                self.search_symbol_type(parent_context_id, name)
            } else {
                None
            }
        }
    }
    pub fn get_symbol_type(&self, name: &str) -> Option<KelpType> {
        self.search_symbol_type(&self.current_context, name)
    }
    
    pub fn _get_type(&self, name: &str) -> Option<KelpType> {
        match self.get_symbol_type(name) {
            Some(KelpType::Type {name, value}) => {
                Some((*(value.clone())).clone())
            },
            Some(nt) => {
                error!("Symbol '{}' is not a type", nt);
                        None
            },
            None => {
                error!("Symbol '{}' does not exist", name);
                    None
            }
        }
    }
    // pub fn add_type(&mut self, values: IndexMap<String, (usize, Option<KelpType>)>) -> KelpId {
    //     let id = get_new_id();
    //     let en = KelpEnum { id, values };
    //     if let Some(t) = self
    //         .map
    //         .get_mut(&self.current_context)
    //         .unwrap()
    //         .type_map
    //         .insert(id, en)
    //     {
    //         error(&format!("Type {} already exists", t));
    //     }
    //     id
    // }

    pub fn add_to_context(&mut self, name: String, value: KelpType) -> Option<KelpType> {
        self.map
            .get_mut(&self.current_context[..])
            .unwrap()
            .symbol_map
            .insert(name, value)
    }

    pub fn insert_context(&mut self, id: String) {
        self.map.get_mut(&self.current_context).unwrap().inserted_contexts.push(id)
    }

    pub fn add_child_context(&mut self) {
        let name = match self.possible_child_context.clone() {
            Some(c) => c,
            None => format!("lambda{}", get_new_id())
        };
        let context_id = self.current_context.clone() + " " + &name;
        let child_context = Context::new(if self.current_context != "" {
            Some(self.current_context.clone())
        } else {
            None
        });
        self.map.insert(context_id.clone(), child_context);
        self.map
            .get_mut(&self.current_context[..])
            .unwrap_or(&mut Context::new(None))
            .children_contexts
            .push(context_id.clone());
        self.enter_context(context_id.as_str());
    }

    pub fn change_possible_child_context(&mut self, cc: Option<String>) {
        self.possible_child_context = cc;
    }

    fn enter_context(&mut self, id: &str) {
        self.current_context = id.to_string();
    }

    pub fn escape_context(&mut self) -> Result<(), ()> {
        if let Some(parent_context_id) = self.map[&self.current_context].parent_context.clone() {
            self.current_context = parent_context_id;
            Ok(())
        } else {
            error!(
                "Cannot escape current context {} as it has no parent",
                self.current_context
            );
            Err(())
        }
    }
}

