//#![feature(backtrace)]

use std::fs::File;
use std::io::Read;

use clap::{clap_app, crate_authors, crate_description, crate_version, App, Arg, ArgMatches};
use colored::Colorize;

use crate::ast::AST;
use crate::message::{MessageLevel, MESSAGE_DISPATCHER};

//use kelp_app::{App, AppBuilder};

#[macro_use]
mod message;
#[macro_use]
mod types;
mod ast;
mod builtins;
mod context;
mod reader;
mod typer;
#[macro_use]
mod llvm;

fn print_banner() {
    let mut banner = File::open("assets/kelp-banner.txt").unwrap();
    let mut banner_string = String::new();
    let _ = banner.read_to_string(&mut banner_string);
    print!("{}\n\n", banner_string.bright_green());
}

fn set_verbosity(matches: &ArgMatches) {
    let msg_level = if matches.is_present("silent") {
        MessageLevel::Silent
    } else {
        match matches.occurrences_of("verbosity") {
            0 => {
                println!("Printing only errors");
                MessageLevel::Error
            }
            1 => {
                println!("Printing errors and warnings");
                MessageLevel::Warning
            }
            2 => {
                println!("Printing all information");
                MessageLevel::Info
            }
            _ => {
                println!("Woah, hold your horses");
                MessageLevel::Info
            }
        }
    };

    MESSAGE_DISPATCHER.lock().unwrap().change_message_level(msg_level);
}

fn compile(matches: ArgMatches) {
    let input = matches.value_of("input").unwrap();

    let mut input_file = match File::open(input) {
        Ok(file) => file,
        Err(_) => {
            error!("No such file: {}", input);
            return;
        }
    };

    let mut input_string = String::new();
    let _ = input_file.read_to_string(&mut input_string);

    let token_tree = reader::read_str(input_string);
    let ast = AST::construct(token_tree);

    MESSAGE_DISPATCHER.lock().unwrap().print_stats();

    llvm::do_llvm();
}

fn main() {
    print_banner();


    let matches = App::new("kelpc")
        .version(crate_version!())
        .author(crate_authors!())
        .about(crate_description!())
        .arg(
            Arg::new("input")
                .value_name("FILE")
                .about("Pick file to compile")
                .takes_value(true)
                .required(true)
                .index(1),
        )
        .arg(
            Arg::new("verbosity")
                .short('v')
                .multiple_occurrences(true)
                //.takes_value(true)
                .about("Sets the level of verbosity"),
        )
        .arg(
            Arg::new("silent")
                .short('s')
                .long("silent")
                .conflicts_with("verbosity")
                .about("Run without any messages"),
        )
        .arg(
            Arg::new("output")
                .short('o')
                .long("output")
                .value_name("OUTPUT_FILE")
                .takes_value(true)
                .default_value("a.out")
                .about("Set the output file"),
        )
        .arg(Arg::new("llvm-test").short('l').long("llvm-test"))
        .get_matches();


    if matches.is_present("llvm-test") {
        llvm::do_llvm();
        return;
    }
    set_verbosity(&matches);
    compile(matches);
}
