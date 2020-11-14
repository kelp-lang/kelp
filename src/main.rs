#![feature(backtrace)]

use std::fs;

#[allow(dead_code)]
#[macro_use]
mod ast;
mod error;
mod operator;
mod parser;

use ast::{ASTBuilder, AST};
use error::Error;

fn string_into_static_str(s: String) -> &'static str {
    Box::leak(s.into_boxed_str())
}

fn main() -> Result<(), Error> {
   // let backtrace = Backtrace::force_capture();
    let unparsed_file =
        fs::read_to_string("/home/yachimm_thomasegh/Documents/Projects/kelp/examples/fizzbuzz.klp")
            .expect("Cannot read file");

    let ast_builder = ASTBuilder::default()
        .add_parse_tree(string_into_static_str(unparsed_file))?
        .first_pass()
        .build_operators()
        .build();

      //  println!("{:#?}", backtrace.status());

  //      .build();
    let ast: AST = ast_builder.get_ast();

    println!("{:#?}", ast);

    Ok(())
}
