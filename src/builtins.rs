use std::collections::HashMap;

use crate::{context::Context, typer::KelpType};

macro_rules! builtin_func {
    ($name:literal, $lhs_name:ident, $lhs_type:ident, $rhs_name:ident, $rhs_type:ident, $ret_type:ident) => {{
        //let mut args: indexmap::IndexMap<String, KelpType> = indexmap::IndexMap::new();

        let args = vec![
            crate::typer::KelpType::Symbol {
                name: stringify!($lhs_name).to_string(),
                typ: std::rc::Rc::new(crate::typer::KelpType::Type {
                    name: stringify!($lhs_type).to_string(),
                    value: std::rc::Rc::new(crate::typer::KelpType::$lhs_type)
                })
            },
            crate::typer::KelpType::Symbol {
                name: stringify!($rhs_name).to_string(),
                typ: std::rc::Rc::new(crate::typer::KelpType::Type {
                    name: stringify!($rhs_type).to_string(),
                    value: std::rc::Rc::new(crate::typer::KelpType::$rhs_type)
                })
            }
        ];

        ($name.to_string(), crate::typer::KelpType::Lambda { 
            arg: std::rc::Rc::new(crate::typer::KelpType::Group{ members: args }),
            ret: std::rc::Rc::new(crate::typer::KelpType::$ret_type)
        })
    }}
}

macro_rules! builtin_type {
    ($name:ident) => {{
        (stringify!($name).to_string(), crate::typer::KelpType::Type {
            name: stringify!($name).to_string(),
            value: std::rc::Rc::new(crate::typer::KelpType::$name),
        })
    }}
}

pub fn get_builtin_context() -> Context {
    //TODO: This is temporary, this should be agar exported
    let symbol_map: HashMap<String, KelpType> = vec![
        builtin_type!(Int),
        builtin_type!(Float),
        builtin_type!(Bool),
        builtin_type!(String),
        builtin_func!("add", lhs, Int, rhs, Int, Int),
        builtin_func!("sub", lhs, Int, rhs, Int, Int),
        builtin_func!("mul", lhs, Int, rhs, Int, Int),
    ].into_iter().collect();
    Context {
        symbol_map,
        parent_context: None,
        children_contexts: vec![],
        inserted_contexts: vec![],
    }
}