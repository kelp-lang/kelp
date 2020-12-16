//#![feature(backtrace)]

//#[allow(dead_code)]
#[macro_use]
mod ast;
mod message;
mod parser;
mod span;

use ast::ASTBuilder;
pub use ast::{Expr, ExprKind, Literal, Typ};
use message::MessageDispatcher;
pub use message::{Error, ErrorType};
pub use message::{MessageLevel, MessageOutput};
use parser::{KelpParser, Rule};
use pest::Parser;
pub use span::Span;
//use std::backtrace::Backtrace;

pub struct Compiler {
    input_file: String,
    msg_dispatcher: MessageDispatcher,
}

impl Compiler {
    pub fn new(
        input_file: String,
        msg_level: MessageLevel,
        msg_output: MessageOutput,
    ) -> Result<Self, Error> {
        //let bt = Backtrace::capture();
        let msg_dispatcher = MessageDispatcher::new(msg_level, msg_output);
        let mut parse_tree = KelpParser::parse(Rule::root, input_file.as_str())?;
        let ast_builder = ASTBuilder::new(msg_dispatcher.clone())
            .build_ast(parse_tree.next().unwrap())?
            .build();
        msg_dispatcher.print_stats();
        let ast = ast_builder.get_ast();

        println!("{}", ast);

        //let string = ast.to_string(); //stack overflow

        Ok(Self {
            input_file: input_file,
            msg_dispatcher,
        })
    }
}
