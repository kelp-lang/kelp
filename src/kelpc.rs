use std::{fs::File, io::Read};

use clap::{crate_authors, crate_name, crate_version, App, Arg};
use kelp::{Compiler, error_e};

fn main() {
    let matches = App::new(crate_name!())
        .version(crate_version!())
        .author(crate_authors!())
        .arg(
            Arg::with_name("file")
                .takes_value(true)
                .required(true)
                .value_name("FILE"),
        )
        .get_matches();

    let filepath = matches.value_of("file").unwrap();

    match File::open(filepath) {
      Ok(mut file) => {
        let mut filecontent: String = String::new();
        file.read_to_string(&mut filecontent);

        let mut compiler = Compiler::new();

        compiler.rep(&filecontent, filepath.to_string());
      },
      Err(err) => {
        error_e!("{}", err);
      }
    }
}
