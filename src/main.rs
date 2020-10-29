use pest::Parser;
use std::fs;

#[allow(dead_code)]
mod ast;
mod error;
mod parser;

use ast::ASTBuilder;
use parser::{KelpParser, Rule};

fn main() {
    let unparsed_file =
        fs::read_to_string("/home/yachimm_thomasegh/Documents/Projects/kelp/examples/fizzbuzz.klp")
            .expect("Cannot read file");

    let root = KelpParser::parse(Rule::root, &unparsed_file)
        .expect("Unsuccessful parse")
        .next()
        .unwrap()
        .into_inner();
    //println!("{:#?}", root);

    let ast = ASTBuilder::build(root);

    println!("{:#?}", ast);
}
