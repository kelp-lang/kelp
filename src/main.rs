use pest::Parser;
use std::fs;

#[allow(dead_code)]
mod ast;
mod error;
mod parser;

use ast::{ASTBuilder, AST};
use error::Error;
use parser::{KelpParser, Rule};

fn main() -> Result<(), Error> {
    let unparsed_file =
        fs::read_to_string("/home/yachimm_thomasegh/Documents/Projects/kelp/examples/fizzbuzz.klp")
            .expect("Cannot read file");

    let root = KelpParser::parse(Rule::root, &unparsed_file)
        .expect("Unsuccessful parse")
        .next()
        .unwrap();
    //println!("{:#?}", root);

    let ast_builder = ASTBuilder::default()
        .add_parse_tree(root.clone())
        .first_pass()?
        .build();
    let ast: AST = ast_builder.get_ast();

    println!("{:#?}", ast);

    Ok(())
}
