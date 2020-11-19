mod error;

pub use error::{Error, ErrorType};
use std::fmt::Display;

#[derive(Debug, Clone)]
pub enum MessageLevel {
    Silent,
    Error,
    Warning,
    Info,
}

#[derive(Debug, Clone)]
pub enum MessageOutput {
    File(String),
    Stderr,
    Stdout,
}

#[derive(Debug, Clone)]
pub struct MessageDispatcher {
    had_error: bool,
    level: MessageLevel,
    output: MessageOutput,
}

impl MessageDispatcher {
    pub fn new(msg_level: MessageLevel, msg_out: MessageOutput) -> Self {
        Self {
            level: msg_level,
            output: msg_out,
            had_error: false,
        }
    }
    pub fn dispatch<T: Display>(&mut self, message: T) {
        self.had_error = true;
        match &self.output {
            MessageOutput::Stderr => eprintln! {"{}", message},
            MessageOutput::Stdout => println! {"{}", message},
            MessageOutput::File(path) => todo!(),
        }
    }
}
