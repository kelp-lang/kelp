use crate::message::Error;
use std::fs;

//#[allow(dead_code)]
#[macro_use]
mod ast;
mod message;
mod operator;
mod parser;

use ast::{ASTBuilder, Expr};
use message::{MessageDispatcher, MessageLevel, MessageOutput};

fn string_into_static_str(s: String) -> &'static str {
    Box::leak(s.into_boxed_str())
}

fn main() -> Result<(), Error> {
    let msg_dispatcher = MessageDispatcher::new(MessageLevel::Info, MessageOutput::Stderr);
    // let backtrace = Backtrace::force_capture();
    let unparsed_file =
        fs::read_to_string("examples/fizzbuzz.klp")
            .expect("Cannot read file");

    let ast_builder = ASTBuilder::new(string_into_static_str(unparsed_file), msg_dispatcher)?
        .first_pass()
        .build_operators()
        .build();

    //  println!("{:#?}", backtrace.status());

    //      .build();
    let ast = ast_builder.get_ast();

    println!("{:#?}", ast);

    Ok(())
}
