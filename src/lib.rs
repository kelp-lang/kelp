#![feature(backtrace)]

use env::EnvironmentStore;
use instruction::{Instruction, InstructionInner};
use span::Span;
use token::{Token, TokenInner};

pub use crate::message::{info, warning};
use crate::{instruction::Literal, special_forms::form_fn};

#[macro_use]
pub mod message;

pub mod env;
mod instruction;
pub mod link;
mod reader;
mod span;
mod token;
mod typ;

fn read(str: &str, path: String) -> Token {
    reader::read_string(str.to_string(), path)
}

fn print(ast: Instruction) {
    println!("{}", ast);
}

mod special_forms {
    use crate::{
        env::EnvironmentStore,
        eval,
        instruction::{Instruction, InstructionInner},
    };

    pub fn form_fn(
        params: Vec<Instruction>,
        body: Instruction,
        env_id: usize,
        env_store: &mut EnvironmentStore,
    ) -> Instruction {
        let env_id = env_store.new_env(Some(env_id));
        let params = params
            .iter()
            .filter_map(|p| match p.inner() {
                InstructionInner::Symbol(sym) => {
                    env_store.set_at(
                        env_id,
                        sym.clone(),
                        Instruction::new(InstructionInner::FromFuncall, p.span()),
                    );
                    Some(sym)
                }
                _ => {
                    let span = p.span();
                    error!(span, "Function parameter must be a symbol");
                    None
                }
            })
            .collect();
        let body = eval(body, env_id, env_store);

        Instruction::new(
            InstructionInner::FunctionDefinition {
                params,
                body: body.clone(),
            },
            body.span(),
        )
    }
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

fn eval(instruction: Instruction, env_id: usize, env_store: &mut EnvironmentStore) -> Instruction {
    match instruction.inner() {
        InstructionInner::SequenceStructure(seq) => {
            if seq.len() == 0 {
                return instruction;
            }
            match seq[0].inner() {
                InstructionInner::Symbol(a0sym) if a0sym == "def" => {
                    let (a1, a2) = (seq[1].clone(), seq[2].clone());

                    if let InstructionInner::Symbol(a1sym) = a1.inner() {
                        if let Some(old) = env_store.get_at(env_id, &a1sym) {
                            let span = a1.span();
                            error!(
                                span,
                                "Symbol already defined at `{}`",
                                old.span().line_start()
                            );
                            Instruction::new(
                                InstructionInner::LiteralStructure(Literal::Empty),
                                instruction.span(),
                            )
                        } else {
                            let a2eval = eval(a2, env_id, env_store);
                            env_store.set_at(env_id, a1sym.clone(), a2eval.clone());
                            Instruction::new(
                                InstructionInner::DefCall {
                                    symbol: a1sym,
                                    instruction: a2eval,
                                },
                                instruction.span(),
                            )
                        }
                    } else {
                        let span = a1.span();
                        error!(span, "Only symbol can be defined");
                        Instruction::new(
                            InstructionInner::LiteralStructure(Literal::Empty),
                            instruction.span(),
                        )
                    }
                }
                InstructionInner::Symbol(a0sym) if a0sym == "fn" => {
                    match (seq[1].inner(), seq.len() > 3) {
                        (InstructionInner::SequenceStructure(params), true) => {
                            println!("Forming bestie");
                            println!(
                                "{}",
                                seq.iter()
                                    .map(|s| format!("{}", s))
                                    .reduce(|acc, x| format!("{}\n{}", acc, x))
                                    .unwrap()
                            );
                            form_fn(
                                params,
                                instruction.shorten_seq(Some(2), None),
                                env_id,
                                env_store,
                            )
                        }
                        (InstructionInner::SequenceStructure(params), false) => {
                            if seq.len() < 3 {
                                let span = instruction.span();
                                error!(span, "fn expects at least 3 arguments");
                                Default::default()
                            } else {
                                println!(
                                    "{}",
                                    seq.iter()
                                        .map(|s| format!("{}", s))
                                        .reduce(|acc, x| format!("{}\n{}", acc, x))
                                        .unwrap()
                                );
                                form_fn(params, seq[2].clone(), env_id, env_store)
                            }
                        }
                        _ => {
                            let span = seq[1].span();
                            error!(span, "Function parameters must be a sequence");
                            Instruction::new(
                                InstructionInner::LiteralStructure(Literal::Empty),
                                instruction.span(),
                            )
                        }
                    }
                }
                InstructionInner::Symbol(a0sym) if a0sym == "quote" => eval(
                    Instruction::new(
                        InstructionInner::Quote(seq[1].clone()),
                        instruction.span(),
                    ),
                    env_id,
                    env_store,
                ),
                InstructionInner::Symbol(a0sym) if a0sym == "eval" => {
                    // TODO: enable more than one in seq
                    eval(Instruction::new(
                        InstructionInner::Eval(seq[1].clone()),
                        seq[1].span(),
                    ), env_id, env_store)
                },
                // This only matches if the instruction is a function, otherwise it falls through
                InstructionInner::Symbol(a0sym)
                    if matches!(
                        env_store
                            .get_at(env_id, &a0sym)
                            .unwrap_or(Instruction::default())
                            .inner(),
                        InstructionInner::FunctionDefinition { .. }
                    ) =>
                {
                    funcall(
                        a0sym,
                        seq[1..].to_vec(),
                        instruction.span(),
                        env_id,
                        env_store,
                    )
                }
                _ => Instruction::new(
                    InstructionInner::SequenceStructure(
                        seq.iter()
                            .map(|s| eval(s.clone(), env_id, env_store))
                            .collect(),
                    ),
                    instruction.span(),
                ),
            }
        }
        InstructionInner::Symbol(ref sym) => {
            if let None = env_store.get_at(env_id, sym) {
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
        InstructionInner::FunctionDefinition { params, body } => todo!(),
        InstructionInner::FunctionCall {
            symbol, arguments, ..
        } => todo!(),
        InstructionInner::Closure { binds, body } => todo!(),
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
            InstructionInner::LiteralStructure(Literal::Int(i.clone())),
            ast.span(),
        ),
        TokenInner::Float(f) => Instruction::new(
            InstructionInner::LiteralStructure(Literal::Float(f.clone())),
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
