use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "dictionary.pest"]
pub(crate) struct KelpParser;

impl KelpParser {}
