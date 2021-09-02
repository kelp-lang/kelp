use indexmap::IndexMap;

use std::{fmt::Display, rc::Rc};

use crate::{
    context::{KelpId},
};

#[derive(Debug, Clone)]
pub enum KelpType {
    Int,
    Float,
    String,
    Bool,
    Enum {
        values: IndexMap<String, (usize, Option<KelpType>)>,
    },
    Lambda {
        arg: Rc<KelpType>,
        ret: Rc<KelpType>,
    },
    Type {
        name: String,
        value: Rc<KelpType>,
    },
    Symbol {
        name: String,
        typ: Rc<KelpType>,
    },
    Group {
        members: Vec<KelpType>,
    },
    NamedGroup {
        members: IndexMap<String, KelpType>,
    },
    SymbolDefinition(String),
    Trait {
        indices: Vec<KelpType>,
    },
    Empty,
    Unknown,
    Special
}

#[derive(Debug, Clone)]
pub struct KelpEnum {
    pub id: KelpId,
    pub values: IndexMap<String, (usize, Option<KelpType>)>,
}

impl PartialEq for KelpType {
    fn eq(&self, other: &Self) -> bool {
        let is_lambda_eq = |(first_arg, first_ret), (second_arg, second_ret)| -> bool {
            first_arg == second_arg && first_ret == second_ret
        };

        let is_group_eq = |first_members: &Vec<_>, second_members: &Vec<_>| -> bool {
            first_members.len() == second_members.len()
                && first_members
                    .iter()
                    .zip(second_members.iter())
                    .all(|(a, b)| a == b)
        };

        let is_named_group_eq =
            |first_members: &IndexMap<_, _>, second_members: &IndexMap<_, _>| -> bool {
                first_members.len() == second_members.len()
                    && first_members
                        .keys()
                        .zip(first_members.values())
                        .zip(second_members.keys().zip(second_members.values()))
                        .all(|((k_a, v_a), (k_b, v_b))| k_a == k_b && v_a == v_b)
            };

        let is_enum_eq = |first_values: &IndexMap<_, _>, second_values: &IndexMap<_, _>| {
            first_values.len() == second_values.len()
                && first_values
                    .keys()
                    .zip(first_values.values())
                    .zip(second_values.keys().zip(second_values.values()))
                    .all(|((k_a, (i_a, v_a)), (k_b, (i_b, v_b)))| {
                        k_a == k_b && i_a == i_b && v_a == v_b
                    })
        };

        match (self, other) {
            (KelpType::Int, KelpType::Int) => true,
            (KelpType::Float, KelpType::Float) => true,
            (KelpType::String, KelpType::String) => true,
            (KelpType::Bool, KelpType::Bool) => true,
            (
                KelpType::Lambda { arg, ret },
                KelpType::Lambda {
                    arg: arg_other,
                    ret: ret_other,
                },
            ) => is_lambda_eq((arg, ret), (arg_other, ret_other)),
            (
                KelpType::Type { name, value },
                KelpType::Type {
                    name: name_other,
                    value: value_other,
                },
            ) => name == name_other && value == value_other,
            (t, KelpType::Type { name: _, value}) => {
                t == &**value
            },
            (KelpType::Type { name: _, value }, t) => {
                &**value == t
            }
            (
                KelpType::Group { members },
                KelpType::Group {
                    members: members_other,
                },
            ) => is_group_eq(members, members_other),
            (
                KelpType::Group { members },
                KelpType::NamedGroup {
                    members: members_other,
                },
            ) => is_group_eq(members, &members_other.values().cloned().collect()),
            (
                KelpType::NamedGroup { members },
                KelpType::Group {
                    members: members_other,
                },
            ) => is_group_eq(&members.values().cloned().collect(), members_other),
            (
                KelpType::NamedGroup { members },
                KelpType::NamedGroup {
                    members: members_other,
                },
            ) => is_named_group_eq(members, members_other),
            (KelpType::SymbolDefinition(_), KelpType::SymbolDefinition(_)) => false,
            (
                KelpType::Enum { values },
                KelpType::Enum {
                    values: values_other,
                },
            ) => is_enum_eq(values, values_other),
            (
                KelpType::Symbol { name, typ },
                KelpType::Symbol {
                    name: name_other,
                    typ: typ_other,
                },
            ) => name == name_other && typ == typ_other,
            (KelpType::Unknown, _) => true,
            (_, KelpType::Unknown) => true,
            (_, _) => false,
        }
    }
}
impl Display for KelpEnum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let format_value = |enum_value: (usize, Option<_>)| {
            if let Some(value) = enum_value.1 {
                format!("{}", value)
            } else {
                "".to_string()
            }
        };
        write!(
            f,
            "{}",
            self.values
                .iter()
                .map(|(key, value)| format!("{} {}{}", value.0, key, format_value(value.clone())))
                .reduce(|acc, next| format!("{}\n{}", acc, next))
                .unwrap_or("".to_string())
        )
    }
}
impl Display for KelpType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let display_named_group_members = |members: &IndexMap<_, _>| {
            members
                .iter()
                .map(|(key, value)| format!("{}: {}", key, value))
                .reduce(|acc, next| format!("{}\n{}", acc, next))
                .unwrap_or("".to_string())
        };

        let display_group_members = |members: &Vec<KelpType>| {
            members
                .iter()
                .map(|member| format!("{}", member))
                .reduce(|acc, next| format!("{}\n{}", acc, next))
                .unwrap_or("".to_string())
        };

        let display_enum_values = |values: &IndexMap<_, _>| {
            values
                .iter()
                .map(|(key, (id, value))| format!("{}: id: {} value: {:#?}", key, id, value))
                .reduce(|acc, next| format!("{}\n{}", acc, next))
                .unwrap_or("".to_string())
        };

        match self {
            KelpType::Int => write!(f, "Int"),
            KelpType::Float => write!(f, "Float"),
            KelpType::String => write!(f, "String"),
            KelpType::Bool => write!(f, "Bool"),
            KelpType::Lambda { arg, ret } => write!(f, "<<lambda args: {} -> {}>>", arg, ret),
            KelpType::Type { name, value } => write!(f, "<<type {}: {}>>", name, value),
            KelpType::Group { members } => {
                write!(f, "<<group [{}]>>", display_group_members(members))
            }
            KelpType::NamedGroup { members } => write!(
                f,
                "<<named_group [{}]>>",
                display_named_group_members(members)
            ),
            KelpType::SymbolDefinition(sym) => write!(f, "<<symbol definition {}>>", sym),
            KelpType::Empty => write!(f, "<<empty>>"),
            KelpType::Symbol { name, typ } => write!(f, "<<symbol {}: {}>>", name, typ),
            KelpType::Enum { values } => write!(f, "<<enum {}>>", display_enum_values(values)),
            KelpType::Unknown => write!(f, "<<unknown>>"),
            KelpType::Special => write!(f, "<<special>>"),
            KelpType::Trait { indices } => write!(f, "<<trait {}>>", display_group_members(indices)),
        }
    }
}

// fn type_defined_sym(context_store: &mut ContextStore, symbol: &str) -> Result<KelpType, KelpErr> {
//     match context_store
//         .get_symbol_type(symbol) {
//             Some(typ) => Ok(typ),
//             None => {
//                 error(&format!("Symbol not found {}", symbol));
//                 Err(KelpErr::ErrString("Symbol not found".to_string()))
//             }
//         }
// }
/*

fn type_to_pure_type(typ: KelpType) -> Result<KelpType, KelpErr> {
    match typ {
        KelpType::Type { name: _, value } => Ok((*(value.clone())).clone()),
        _ => Err(KelpErr::ErrString(format!("{} is not a type", typ))),
    }
}
fn named_group_to_type_only_group(group: KelpType) -> Result<KelpType, KelpErr> {
    match group {
        KelpType::NamedGroup { members } => Ok(KelpType::Group {
            members: members
                .values()
                .cloned()
                .filter_map(|m| type_to_pure_type(m).ok())
                .collect::<Vec<_>>(),
        }),
        _ => Err(KelpErr::ErrString(format!(
            "{} is not a named group",
            group
        ))),
    }
}

fn type_type(context_store: &mut ContextStore, name: &str) -> Result<KelpType, KelpErr> {
    context_store
        .get_type(name)
        .ok_or(KelpErr::ErrString(format!("Type {} not found", name)))
}

fn type_sym(
    context_store: &mut ContextStore,
    symbol: &str,
    typ: Option<KelpVal>,
) -> Result<KelpType, KelpErr> {
    // lazy_static! {
    //     static ref RE: Regex = Regex::new(r###"^[^\s]+:[^\s]+$"###).unwrap();
    // }

    // if let Some(t) = context_store.get_symbol_type(symbol) {
    //     Ok(t)
    // } else if let Some(t) = context_store.get_type(symbol) {
    //     Ok(t)
    // } else if RE.is_match(symbol) {
    //     let type_sym = symbol.split(":").collect::<Vec<&str>>()[1];

    //     match context_store.get_type(type_sym) {
    //         Some(type_type) => Ok(type_type.clone()),
    //         None => Err(KelpErr::ErrString(format!("{} not defined", type_sym))),
    //     }
    // } else {
    //     Err(KelpErr::ErrString(format!("{} is not defined", symbol)))
    //
    //println!("{} typ {:#?} {:#?}", symbol, typ, context_store);
    let typ = match (context_store.get_symbol_type(symbol), typ) {
        (None, None) => {
            error!("The symbol '{}' has no type", symbol);
            return Err(KelpErr::ErrString(format!(
                "The symbol '{}' has no type",
                symbol
            )));
        }
        (None, Some(KelpVal::Sym { name, typ: _ })) => {
            //println!("{:?}", type_type(context_store, &name));
            KelpType::Type {
                name: name.clone(),
                value: Rc::new(type_type(context_store, &name)?),
            }
        }
        (None, Some(t_val)) => type_atom(context_store, &t_val, None)?,
        (Some(t), None) => t,
        (Some(t), Some(t_val)) => {
            error(&format!(
                "The symbol {} has already type {} and it cannot be changed during runtime to {}",
                symbol, t, t_val
            ));
            return Err(KelpErr::ErrString(format!(
                "The symbol {} has already type {} and it cannot be changed during runtime to {}",
                symbol, t, t_val
            )));
        }
    };

    Ok(KelpType::Symbol {
        name: symbol.to_string(),
        typ: Rc::new(typ),
    })
}

fn type_body(context_store: &mut ContextStore, exprs: &Vec<KelpVal>) -> Result<KelpType, KelpErr> {
    let expr_types = exprs
        .iter()
        .filter_map(|expr| match expr {
            KelpVal::Expr { lhs, op, rhs } => {
                match type_expr(context_store, lhs.borrow(), op.borrow(), rhs.borrow()) {
                    Ok(t) => Some(t),
                    Err(e) => {
                        error(&format!("{}", e));
                        None
                    }
                }
            }
            KelpVal::Int(_) => Some(KelpType::Int),
            KelpVal::Float(_) => Some(KelpType::Float),
            KelpVal::Str(_) => Some(KelpType::String),
            KelpVal::Bool(_) => Some(KelpType::Bool),
            KelpVal::Group(g) => match type_group(context_store, g) {
                Ok(t) => Some(t),
                Err(_) => None,
            },
            _ => {
                error(&format!("Body element is not a valid expression {}", expr));
                None
            }
        })
        .collect::<Vec<_>>();

    println!("{:?}", expr_types);

    match expr_types.last() {
        Some(t) => {
            return Ok(t.clone());
        }
        None => {
            error("lol");
            return Ok(KelpType::Empty);
        }
    }
}

fn type_group(
    context_store: &mut ContextStore,
    members: &Vec<KelpVal>,
) -> Result<KelpType, KelpErr> {
    Ok(KelpType::Group {
        members: members
            .iter()
            .filter_map(|member| match type_atom(context_store, member, None) {
                Ok(t) => Some(t),
                Err(e) => {
                    error(&format!("Cannot type within group: {}", e));
                    None
                }
            })
            .collect(),
    })
}

fn type_lambda_arg(context_store: &mut ContextStore, arg: &KelpVal) -> Result<KelpType, KelpErr> {
    match arg {
        KelpVal::Sym { name, typ } => {
            if let Some(typ_val) = (*(typ.clone())).clone() {
                let typ = type_atom(context_store, &typ_val, None)?;
                context_store.add_to_context(name.clone(), typ.clone());
                Ok(KelpType::Symbol {
                    name: name.clone(),
                    typ: Rc::new(typ),
                })
            } else {
                error(&format!("Missing type for symbol '{}'", name));
                Err(KelpErr::ErrString(format!(
                    "Missing type for symbol '{}'",
                    name
                )))
            }
        }
        KelpVal::Lambda { arg, body, ret } => {
            let typ = type_lambda(context_store, arg, body, ret, None)?;

            let _ = type_lambda_arg(context_store, arg)?;

            Ok(typ)
        }
        KelpVal::Group(members) => {
            let named = members
                .iter()
                .filter_map(|m| {
                    let atom = type_atom(context_store, m, None);
                    match atom.clone() {
                        Ok(KelpType::Symbol { name, typ }) => {
                            context_store.add_to_context(name.clone(), (*(typ.clone())).clone());
                            Some((name, (*(typ.clone())).clone()))
                        }
                        Ok(t) => {
                            error(&format!("{:#?} is not a symbol, it's a {}", atom, t));
                            None
                        }
                        Err(e) => {
                            error(&format!("{}", e));
                            None
                        }
                    }
                })
                .collect::<IndexMap<String, KelpType>>();

            Ok(KelpType::NamedGroup { members: named })
        }
        _ => {
            error("Argument cannot be other type then Group, Lambda or a Symbol");
            Err(KelpErr::ErrString("Argument cannot be static".to_string()))
        }
    }
}

fn type_lambda(
    context_store: &mut ContextStore,
    arg: &KelpVal,
    body: &KelpVal,
    ret: &KelpVal,
    symbol_name: Option<String>,
) -> Result<KelpType, KelpErr> {
    let escape = if let Some(name) = symbol_name.clone() {
        context_store.add_child_context(&name);
        true
    } else {
        false
    };

    let arg_type = match type_lambda_arg(context_store, arg) {
        Ok(t) => t,
        Err(e) => {
            error(&format!("Arg Error: {}", e));
            return Err(e);
        }
    };

    let body_type = match type_atom(context_store, body, symbol_name.clone()) {
        Ok(t) => t,
        Err(e) => {
            error(&format!("Body Error: {}", e));
            return Err(e);
        }
    };

    if escape {
        let _ = context_store.escape_context();
    }

    let ret_type = match ret {
        KelpVal::Unknown => body_type,
        KelpVal::Sym { name, typ: _ } => type_type(context_store, &name)?,
        _ => {
            if type_atom(context_store, ret, None)? == body_type {
                body_type
            } else {
                error(&format!(
                    "Declared type {:?} does not match with the real return type {}",
                    type_atom(context_store, ret, None),
                    body_type
                ));
                return Err(KelpErr::ErrString(format!(
                    "Declared type {:?} does not match with the real return type {}",
                    type_atom(context_store, ret, None),
                    body_type
                )));
            }
        }
    };

    Ok(KelpType::Lambda {
        arg: Rc::new(arg_type),
        ret: Rc::new(ret_type),
    })
}

fn type_atom(
    context_store: &mut ContextStore,
    atom: &KelpVal,
    symbol_name: Option<String>,
) -> Result<KelpType, KelpErr> {
    Ok(match atom {
        KelpVal::Int(_) => KelpType::Int,
        KelpVal::Float(_) => KelpType::Float,
        KelpVal::Bool(_) => KelpType::Bool,
        KelpVal::Str(_) => KelpType::String,
        KelpVal::Sym { name, typ } => {
            // if is_type {
            //     match type_type(context_store, name.as_str()) {
            //         Ok(t) => KelpType::Type {
            //             name: name.clone(),
            //             value: Rc::new(t),
            //         },
            //         Err(e) => return Err(e),
            //     }
            // } else {
            match type_sym(context_store, name.as_str(), (*(typ.clone())).clone()) {
                Ok(t) => t,
                Err(e) => {
                    error(&format!("Symbol typing resulted in error: {}", e));
                    return Err(e);
                }
            }
            //}
        }
        KelpVal::Unknown => {
            error(&format!("Value is not known"));
            return Err(KelpErr::ErrString("Value is not known".to_string()));
        }
        //KelpVal::Expr { lhs, op, rhs } => todo!(),
        KelpVal::Lambda { arg, body, ret } => {
            match type_lambda(
                context_store,
                arg.borrow(),
                body.borrow(),
                ret.borrow(),
                symbol_name,
            ) {
                Ok(t) => t,
                Err(e) => {
                    error(&format!("Error: {}", e));
                    return Err(e);
                }
            }
        }
        KelpVal::Body(exprs) => {
            return match type_body(context_store, exprs) {
                Ok(t) => Ok(t),
                Err(e) => {
                    error(&format!("Type Atom Body Error: {}", e));
                    return Err(e);
                }
            }
        }
        KelpVal::Group(members) => {
            return match type_group(context_store, members) {
                Ok(t) => Ok(t),
                Err(e) => {
                    error(&format!("Type Atom Group Error: {}", e));
                    return Err(e);
                }
            }
        }
        _ => {
            error("Value is not an atom");
            return Err(KelpErr::ErrString("Value is not an atom".to_string()));
        }
    })
}

fn type_expr(
    context_store: &mut ContextStore,
    lhs: &KelpVal,
    op: &KelpVal,
    rhs: &KelpVal,
) -> Result<KelpType, KelpErr> {
    let check_expr = |lhs_type: KelpType, op_type: KelpType, rhs_type: KelpType| {
        if let KelpType::Lambda { arg, ret } = op_type.clone() {
            let group = KelpType::Group {
                members: vec![lhs_type.clone(), rhs_type.clone()],
            };
            let converted_arg = named_group_to_type_only_group((*(arg.clone())).clone())?;
            if converted_arg == group {
                Ok((*ret).clone())
            } else {
                Err(KelpErr::ErrString(format!(
                    "lhs: {} op: {} rhs: {} don't have compatible types",
                    lhs_type, op_type, rhs_type
                )))
            }
        } else {
            Err(KelpErr::ErrString("Operator is not a function".to_string()))
        }
    };

    let x = match (lhs, op, rhs) {
        (
            KelpVal::Expr {
                lhs: l_lhs,
                op: l_op,
                rhs: l_rhs,
            },
            KelpVal::Sym { name, typ: _ },
            rhs,
        ) => {
            let (lhs_type, op_type, rhs_type) = (
                type_expr(context_store, l_lhs, l_op, l_rhs)?,
                context_store
                    .get_symbol_type(name)
                    .expect(&format!("Operator {} is not defined", name)),
                type_atom(context_store, rhs, None)?,
            );

            check_expr(lhs_type, op_type, rhs_type)
        }
        (lhs, KelpVal::Sym { name, typ: _ }, rhs) => match (lhs, name.as_str()) {
            (
                KelpVal::Sym {
                    name: lhs_name,
                    typ: lhs_type,
                },
                "=",
            ) => {
                //check for other contexts before registering new symbol
                let rhs_type = match type_atom(context_store, rhs, Some(lhs_name.clone())) {
                    Ok(t) => t,
                    Err(e) => {
                        error(&format!("{}", e));
                        return Err(e);
                    }
                };
                let lhs_type_val = (*(lhs_type.clone())).clone();
                if let Some(KelpVal::Sym { name, typ: _ }) = lhs_type_val {
                    match type_type(context_store, &name) {
                        Ok(lhs_type_type) => {
                            if lhs_type_type != rhs_type {
                                error(&format!(
                                    "Incompatible types {} and {}",
                                    lhs_type_type, rhs_type
                                ));
                                return Err(KelpErr::ErrString(format!(
                                    "Incompatible types lhs: {}, rhs: {}",
                                    lhs_type_type, rhs_type
                                )));
                            }
                        }
                        Err(e) => {
                            error(&format!("{}", e));
                        }
                    }
                } else if let Some(lhs_type) = lhs_type_val {
                    match type_atom(context_store, &lhs_type, None) {
                        Ok(lhs_type) => {
                            if lhs_type != rhs_type {
                                error(&format!("Incompatible types {} and {}", lhs_type, rhs_type));
                                return Err(KelpErr::ErrString(format!(
                                    "Incompatible types lhs: {}, rhs: {}",
                                    lhs_type, rhs_type
                                )));
                            }
                        }
                        Err(e) => return Err(e),
                    }
                }

                match context_store.add_to_context(lhs_name.clone(), rhs_type) {
                    Some(_) => Err(KelpErr::ErrString("Symbol already defined".to_string())),
                    None => Ok(KelpType::SymbolDefinition(lhs_name.clone())),
                }
            }
            (lhs_val, ">") => {
                let (arg_type, ret_type) = match type_atom(context_store, rhs, None)? {
                    KelpType::Lambda { arg, ret } => {
                        ((*(arg.clone())).clone(), (*(ret.clone())).clone())
                    }
                    KelpType::Symbol { name: _, typ } => {
                        if let KelpType::Lambda { arg, ret } = (*(typ.clone())).clone() {
                            ((*(arg.clone())).clone(), (*(ret.clone())).clone())
                        } else {
                            error(&format!("Symbol {} is not a function", typ));
                            return Err(KelpErr::ErrString(format!(
                                "Symbol {} is not a function",
                                typ
                            )));
                        }
                    }
                    _ => {
                        error(&format!("{} is not a function nor a lambda", rhs));
                        return Err(KelpErr::ErrString(format!(
                            "{} is not a function nor a lambda",
                            rhs
                        )));
                    }
                };
                let lhs_type = type_atom(context_store, lhs_val, None)?;
                if lhs_type != arg_type {
                    error(&format!("The provided arguments {} do not match the required arguments by the function {}", lhs_type, arg_type));
                    return Err(KelpErr::ErrString(format!(
                        "The provided arguments {} do not match the required by the function {}",
                        lhs_type, arg_type
                    )));
                }

                Ok(ret_type)
            }
            (lhs, op) => check_expr(
                type_atom(context_store, lhs, None)?,
                context_store
                    .get_symbol_type(op)
                    .expect("Op is not defined in context"),
                type_atom(context_store, rhs, None)?,
            ),
        },
        (_, not_op, _) => Err(KelpErr::ErrString(format!(
            "{} is not an operator symbol!",
            not_op
        ))),
    };
    //println!("{:#?}", x);
    x
}

pub fn type_check(val: KelpVal) -> Result<KelpType, KelpErr> {
    let mut context_store = ContextStore::new();
    let res = match val {
        KelpVal::Body(exprs) => type_body(&mut context_store, &exprs),
        _ => type_atom(&mut context_store, &val, Some("root".to_string())),
    };
    println!("{:#?}", context_store);
    res
}
*/