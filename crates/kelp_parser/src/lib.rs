mod parse_tree;
mod parser;

use kelp_message::{Error, MessageDispatcher};
pub use parse_tree::{Expr, ExprKind, Literal, Typ};
pub use parser::*;

use pest::Parser as PestParser;
use kelp_origin::Origin;

pub struct Parser {
    message_dispatcher: MessageDispatcher,
}
impl Parser {
    pub fn new(message_dispatcher: &MessageDispatcher) -> Self {
        Self {
            message_dispatcher: message_dispatcher.clone(),
        }
    }
    pub fn parse(&self, input_file: String, origin: Origin) -> Result<Expr, Error> {
        let pest_tree = KelpParser::parse(Rule::root, input_file.as_str())?
            .next()
            .expect("Root rule is empty");
        let parse_tree = parse_tree::ParseTree::new(&self.message_dispatcher, origin)
            .build_parse_tree(pest_tree)?
            .get_parse_tree();
        Ok(parse_tree)
    }
}
