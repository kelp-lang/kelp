use std::fs;

use kelp::{Compiler, Error, MessageLevel, MessageOutput};

fn main() -> Result<(), Error> {
    // let backtrace = Backtrace::force_capture();
    let unparsed_file = fs::read_to_string(
        "/home/yachimm_thomasegh/Documents/Projects/kelp/kelp/examples/fizzbuzz.klp",
    )
    .expect("Cannot read file");
    //let unparsed_file = "i = 4".to_string();
    let _compiler = Compiler::new(unparsed_file, MessageLevel::Info, MessageOutput::Stdout);

    Ok(())
}
