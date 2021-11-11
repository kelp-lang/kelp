//#![feature(backtrace)]

use std::collections::HashMap;

use env::EnvironmentStore;
use instruction::{Instruction, InstructionInner};
use span::Span;
use token::{Token, TokenInner};

pub use crate::message::{info, warning};
use crate::{instruction::Literal, special_forms::{f_function, f_macro}};

#[macro_use]
pub mod message;

pub mod env;
mod instruction;
pub mod link;
mod reader;
mod span;
mod special_forms;
mod token;
mod typ;

const SPECIAL_FORMS: &[(&str, usize)] = &[("def", 2), ("function", 3), ("macro", 3), ("quote", 1), ("eval", 1), ("get", 1)];

fn read(str: &str, path: String) -> Token {
    reader::read_string(str.to_string(), path)
}

fn print(ast: Instruction) {
    println!("{}", ast);
}

fn funcall(
    a0sym: String,
    tail: Vec<Instruction>,
    span: Span,
    env_id: usize,
    env_store: &mut EnvironmentStore,
) -> Instruction {
    if let Some(a0instruction) = env_store.get_at(env_id, &a0sym) {
        if let InstructionInner::FunctionDefinition { params: _, body: _ } = a0instruction.inner() {
            Instruction::new(
                InstructionInner::FunctionCall {
                    symbol: a0sym,
                    arguments: tail
                        .iter()
                        .map(|arg| eval(arg.clone(), env_id, env_store))
                        .collect(),
                    env_id,
                },
                span,
            )
        } else {
            unreachable!("Symbol should always be a function definition");
        }
    } else {
        error!(span, "Symbol does not exist");
        todo!()
    }
}

fn expand_body(hash_map: &HashMap<String, Instruction>, macro_body: Instruction) -> Instruction {
    match macro_body.inner() {
        InstructionInner::Symbol(a0sym) if hash_map.contains_key(&a0sym) => {
            hash_map.get(&a0sym).unwrap().clone()
        },
        InstructionInner::SequenceStructure(inner_seq) => {
            Instruction::new(InstructionInner::SequenceStructure(inner_seq.iter().map(|it| expand_body(hash_map, it.clone())).collect()), macro_body.span())
        }
        InstructionInner::ListStructure(inner_seq) => {
            Instruction::new(InstructionInner::ListStructure(inner_seq.iter().map(|it| expand_body(hash_map, it.clone())).collect()), macro_body.span())
        }
        _ => macro_body
    }
}

// TODO: Hygienic macros (don't expand simply by replacing code, rather keep the bindings from where the macro was defined)
fn macroexpand(arguments: Vec<Instruction>, macro_params: Vec<String>, macro_body: Instruction) -> Instruction {
    let map = macro_params.iter().zip(arguments.iter()).map(|(key, val)| (key.clone(), val.clone())).collect::<HashMap<_, _>>();

    expand_body(&map, macro_body)
}

fn is_funcall(a0: Instruction, env_id: usize, env_store: &mut EnvironmentStore) -> Option<usize> {
    match a0.inner() {
        InstructionInner::FunctionDefinition { params, .. } => Some(params.len()),
        InstructionInner::MacroDefinition { params, .. } => Some(params.len()),
        InstructionInner::Symbol(a0sym) => {
            if let Some((_, param_count)) =
                SPECIAL_FORMS.iter().find(|predicate| predicate.0 == a0sym)
            {
                Some(*param_count)
            } else {
                match env_store.get_at(env_id, &a0sym) {
                    Some(instruction) => match instruction.inner() {
                        InstructionInner::FunctionDefinition { params, .. } => Some(params.len()),
                        InstructionInner::MacroDefinition {params, ..} => Some(params.len()),
                        _ => None,
                    },
                    _ => None,
                }
            }
        }
        _ => None,
    }
}

fn eat_all_params(
    seq: Vec<Instruction>,
    calling_fun: Option<Instruction>,
    param_count: usize,
    env_id: usize,
    env_store: &mut EnvironmentStore,
) -> (Instruction, usize) {
    let mut buffer = vec![];
    // create a buffer of the sequence and add the calling function to the start
    if let Some(fun) = calling_fun {
        buffer.push(fun);
    }
    // `i` steps through the sequence
    let mut i = 0;
    // while `i` has not reached the end of the sequence
    while i < seq.len() {
        // if the function has reached it parameter count, then return
        if buffer.len() == param_count + 1 {
            break;
        }
        // is this element of the sequence a function call?
        match is_funcall(seq[i].clone(), env_id, env_store) {
            // if yes, call `eat_all_params` with the new parameter count
            Some(new_param_count) => {
                // call `eat_all_params` that eats the inner function's parameters and provide the calling function so it can be prepended to the sequence
                let ret = eat_all_params(
                    seq[i + 1..].to_vec(),
                    Some(seq[i].clone()),
                    new_param_count,
                    env_id,
                    env_store,
                );
                // move to the end of the last parameter eating
                i += ret.1;
                buffer.push(ret.0);
            }
            // if no, just add it to the buffer
            None => {
                buffer.push(seq[i].clone());
            }
        }
        i += 1;
    }

    // If the buffer contains only one element, remove the outer sequence and return it instead
    if buffer.len() == 1 {
        return (buffer[0].clone(), i);
    }

    (
        Instruction::new(
            InstructionInner::SequenceStructure(buffer.clone()),
            Span::new(
                //buffer[0].span().start,
                buffer
                    .first()
                    .map(|b| b.span().start)
                    .unwrap_or(seq[0].span().start),
                buffer.last().unwrap().span().end,
                buffer.last().unwrap().span().path,
                buffer.last().unwrap().span().content,
            ),
        ),
        i,
    )
}

// BUG: This is an infinite recursion, find out why, doesn't work when I call eat_all_params, otherwise it is fine
fn eval_seq(
    span: Span,
    seq: Vec<Instruction>,
    env_id: usize,
    env_store: &mut EnvironmentStore,
) -> Instruction {
    if seq.is_empty() {
        return Instruction::new(InstructionInner::LiteralStructure(Literal::Empty), span);
    }

    let seq_instruction = eat_all_params(seq.clone(), None, seq.len(), env_id, env_store).0;
    if let InstructionInner::SequenceStructure(seq) = seq_instruction.inner() {
        match seq[0].inner() {
            InstructionInner::MacroDefinition{params, body} => {
                eval(macroexpand(seq[1..].to_vec(), params, body), env_id, env_store)
            },
            InstructionInner::Symbol(a0sym) if a0sym == "def" => {
                if seq.len() >= 3 {
                    let (a1, a2) = (seq[1].clone(), seq[2].clone());

                    if let InstructionInner::Symbol(a1sym) = a1.inner() {
                        if let Some(old) = env_store.get_at(env_id, &a1sym) {
                            let span = a1.span();
                            error!(
                                span,
                                "Symbol already defined at `{}:{}`",
                                old.span().line_start(),
                                old.span().start
                            );
                            Instruction::new(
                                InstructionInner::LiteralStructure(Literal::Empty),
                                span,
                            )
                        } else {
                            let a2eval = eval(a2, env_id, env_store);
                            env_store.set_at(env_id, a1sym.clone(), a2eval.clone());
                            Instruction::new(
                                InstructionInner::DefCall {
                                    symbol: a1sym,
                                    instruction: a2eval,
                                },
                                span,
                            )
                        }
                    } else {
                        let span = a1.span();
                        error!(span, "Only symbol can be defined");
                        Instruction::new(InstructionInner::LiteralStructure(Literal::Empty), span)
                    }
                } else {
                    error!(span, "Def expects 2 arguments");
                    Default::default()
                }
            }
            InstructionInner::Symbol(a0sym) if a0sym == "function" => match (seq[0].inner(), seq.len() > 2) {
                (
                    InstructionInner::SequenceStructure(params)
                    | InstructionInner::ListStructure(params),
                    true,
                ) => f_function(params, seq[2].clone(), env_id, env_store),
                (
                    InstructionInner::SequenceStructure(_) | InstructionInner::ListStructure(_),
                    _,
                ) => {
                    error!(span, "Function expects at least 3 arguments");
                    Default::default()
                }
                _ => {
                    let span = seq[1].span();
                    error!(span, "Function parameters must be a sequence");
                    Instruction::new(InstructionInner::LiteralStructure(Literal::Empty), span)
                }
            },
            InstructionInner::Symbol(a0sym) if a0sym == "macro" => match (seq[1].inner(), seq.len() > 2) {
                (InstructionInner::SequenceStructure(params) | InstructionInner::ListStructure(params), true) => f_macro(params, seq[2].clone()),
                (InstructionInner::SequenceStructure(_) | InstructionInner::ListStructure(_), false) => {
                    error!(span, "Macro expects at least 3 arguments");
                    Default::default()
                },
                _ => {
                    let span = seq[1].span();
                    error!(span, "Macro parameters must be a sequence");
                    Default::default()
                }
            },
            InstructionInner::Symbol(a0sym) if a0sym == "quote" => eval(
                Instruction::new(InstructionInner::Quote(seq[1].clone()), span),
                env_id,
                env_store,
            ),
            InstructionInner::Symbol(a0sym) if a0sym == "eval" => {
                // TODO: enable more than one in seq
                eval(
                    Instruction::new(InstructionInner::Eval(seq[1].clone()), seq[1].span()),
                    env_id,
                    env_store,
                )
            }
            InstructionInner::Symbol(a0sym) if matches!(env_store.get_at(env_id, &a0sym).unwrap_or_default().inner(), InstructionInner::FunctionDefinition{..} | InstructionInner::MacroDefinition{..}) => match env_store.get_at(env_id, &a0sym).unwrap_or_default().inner() {
                InstructionInner::FunctionDefinition {..} => {
                    funcall(a0sym, seq[1..].to_vec(), span, env_id, env_store)
                },
                InstructionInner::MacroDefinition {params, body} => {
                    println!("{}", seq[1..].iter().map(|x| x.to_string()).collect::<Vec<_>>().join(", "));
                    let expanded = macroexpand(seq[1..].to_vec(), params, body);
                    println!("expanded {}", expanded);
                    eval(expanded, env_id, env_store)
                }
                _ => unreachable!(),
            },
            InstructionInner::Symbol(a0sym) if a0sym == "get" => {
                if let InstructionInner::Symbol(a1sym) = seq[1].inner() {
                    println!("get: {}", env_store.get_at(env_id, &a1sym).unwrap_or_default());
                } else {
                    println!("get: {}", seq[1]);
                }
                seq[1].clone()
            },
            /*InstructionInner::Symbol(a0sym) if a0sym == "export" => {

            },
            InstructionInner::Symbol(a0sym) if a0sym == "import" => {

            },*/
            _ => Instruction::new(
                InstructionInner::SequenceStructure(
                    seq.iter()
                        .map(|s| eval(s.clone(), env_id, env_store))
                        .collect(),
                ),
                span,
            ),
        }
    } else {
        eval(seq_instruction, env_id, env_store)
    }
}

fn eval(instruction: Instruction, env_id: usize, env_store: &mut EnvironmentStore) -> Instruction {
    match instruction.inner() {
        InstructionInner::SequenceStructure(seq) => {
            eval_seq(instruction.span(), seq, env_id, env_store)
        }
        InstructionInner::Symbol(ref sym) => {
            if env_store.get_at(env_id, sym).is_none() {
                let span = instruction.span();
                error!(span, "Symbol is not defined");
            }
            Instruction::new(InstructionInner::GetCall(sym.clone()), instruction.span())
        }
        InstructionInner::Quote(q) => Instruction::new(
            InstructionInner::Quote(eval(q, env_id, env_store)),
            instruction.span(),
        ),
        InstructionInner::Eval(e) => {
            let e = eval(e, env_id, env_store);
            match e.inner() {
                InstructionInner::Quote(q) => eval(q, env_id, env_store),
                _ => Instruction::new(
                    InstructionInner::Eval(eval(e.clone(), env_id, env_store)),
                    e.span(),
                ),
            }
        }
        InstructionInner::FunctionDefinition { .. } => todo!(),
        InstructionInner::FunctionCall { .. } => todo!(),
        InstructionInner::Closure { .. } => todo!(),
        _ => instruction,
    }
}

fn build(ast: Token) -> Instruction {
    //println!("{}", ast);
    match ast.inner() {
        TokenInner::Seq(ref seq) => Instruction::new(
            InstructionInner::SequenceStructure(seq.iter().map(|t| build(t.clone())).collect()),
            ast.span(),
        ),
        TokenInner::Quote(q) => {
            Instruction::new(InstructionInner::Quote(build(q.clone())), ast.span())
        }
        TokenInner::Sym(s) => Instruction::new(InstructionInner::Symbol(s.clone()), ast.span()),
        TokenInner::Int(i) => Instruction::new(
            InstructionInner::LiteralStructure(Literal::Int(*i)),
            ast.span(),
        ),
        TokenInner::Float(f) => Instruction::new(
            InstructionInner::LiteralStructure(Literal::Float(*f)),
            ast.span(),
        ),
        TokenInner::String(s) => Instruction::new(
            InstructionInner::LiteralStructure(Literal::String(s.clone())),
            ast.span(),
        ),
        TokenInner::List(ref lst) => Instruction::new(
            InstructionInner::ListStructure(lst.iter().map(|t| build(t.clone())).collect()),
            ast.span(),
        ),
        _ => todo!("{:?}", ast),
    }
}

pub fn rep(input: &str, path: String, env_id: usize, env_store: &mut EnvironmentStore) {
    let tokens = read(input, path);
    let ast = build(tokens);
    //print(ast.clone());
    let evaled_ast = eval(ast, env_id, env_store);
    print(evaled_ast);
}
