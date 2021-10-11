use clap::{App, crate_authors, crate_name, crate_version};
use kelp::{Compiler, error_e, info, warning};
use rustyline::Editor;

fn main() {
    let matches = App::new(crate_name!())
        .version(crate_version!())
        .author(crate_authors!())
        .get_matches();

    
    //println!("{}", kelp::link::link("https://omase.tk", "lol"));

    info("Running interactive!");

    let mut rl = Editor::<()>::new();
    if rl.load_history(".kelp_history").is_err() {
        warning("No previous history");
    }
    
    let mut compiler = Compiler::new();

    loop {
        let readline = rl.readline("kelp~> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(&line);
                rl.save_history(".kelp_history").unwrap();
                if line.len() > 0 {
                    compiler.rep(line.as_str(), "interactive".to_string());
                }
            },
            Err(rustyline::error::ReadlineError::Interrupted) => break,
            Err(rustyline::error::ReadlineError::Eof) => break,
            Err(err) => {
                error_e!("{}", err);
            }
        }
    }
}