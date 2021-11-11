use std::{fmt::Display, sync::Mutex};

use colored::Colorize;
use lazy_static::lazy_static;

use crate::span::Span;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd)]
pub enum MessageLevel {
    Silent = 0,
    Error = 1,
    Warning = 2,
    Info = 3,
}

pub enum MessageOutput {
    StdOut,
    StdErr,
}

pub struct MessageDispatcher {
    error_count: usize,
    warning_count: usize,
    message_level: MessageLevel,
    message_output: MessageOutput,
    debug: bool,
}

impl MessageDispatcher {
    fn display(&self, message: &str) {
        match self.message_output {
            MessageOutput::StdOut => println!("{}", message),
            MessageOutput::StdErr => eprintln!("{}", message),
        }
    }
    fn send(&mut self, message: &str, message_level: MessageLevel) {
        match message_level {
            MessageLevel::Error => self.error_count += 1,
            MessageLevel::Warning => self.warning_count += 1,
            _ => {}
        }
        if self.message_level >= message_level {
            self.display(message);
        }
    }

    pub fn change_message_level(&mut self, message_level: MessageLevel) {
        self.message_level = message_level;
    }

    pub fn print_stats(&self) {
        self.display(&format!(
            "{} {}",
            "Errors emitted:  ".red(),
            self.error_count
        ));
        self.display(&format!(
            "{} {}",
            "Warnings emitted:".bright_yellow(),
            self.warning_count
        ));
    }
}

lazy_static! {
    pub static ref MESSAGE_DISPATCHER: Mutex<MessageDispatcher> = Mutex::new(MessageDispatcher {
        error_count: 0,
        warning_count: 0,
        message_level: MessageLevel::Info,
        message_output: MessageOutput::StdOut,
        debug: false
    });
}

pub fn error_message(message: &str) {
    let mut md = MESSAGE_DISPATCHER
        .lock()
        .expect(&format!("Cannot send error message: {}", message));
    if md.debug {
        //println!("{}", std::backtrace::Backtrace::capture());
    }
    md.send(message, MessageLevel::Error);
}

pub fn error_with_span(error: Error) {
    let mut md = MESSAGE_DISPATCHER
        .lock()
        .expect(&format!("Cannot send error message: {}", error.message));
    if md.debug {
        //println!("{}", std::backtrace::Backtrace::capture());
    }

    md.send(&format!("{}", error), MessageLevel::Error)
}

macro_rules! error {
    ($span:ident, $($arg:tt)*) => {
        crate::message::error_with_span(crate::message::Error {
            span: $span.clone(),
            message: format!($($arg)*)
        });
    };
    ($($arg:tt)*) => {
        crate::message::error_message(&format!($($arg)*));
    };
}

///HACK: This is a hack of the `error!` macro to use it outside of the crate environment.
///This could be maybe done by using a second wrapper macro, but this works and I won't change it, lol
#[macro_export]
macro_rules! error_e {
    ($span:ident, $($arg:tt)*) => {
        kelp::message::error_with_span(crate::message::Error {
            span: $span.clone(),
            message: format!($($arg)*)
        });
    };
    ($($arg:tt)*) => {
        kelp::message::error_message(&format!($($arg)*));
    };
}

pub fn warning(message: &str) {
    MESSAGE_DISPATCHER
        .lock()
        .expect(&format!("Cannot send warning message: {}", message))
        .send(message, MessageLevel::Warning);
}

pub fn info(message: &str) {
    MESSAGE_DISPATCHER
        .lock()
        .expect(&format!("Cannot send info message: {}", message))
        .send(message, MessageLevel::Info);
}

pub struct Error {
    pub message: String,
    pub span: Span,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {

        let mut span = self.span.clone();
        let header = format!(
            "\n{}{}",
            "Error".bold().red(),
            format!(" {}:{}:{}\n", crate::link::link(&span.path, &span.path.split("/").last().unwrap()), span.ln() + 1, span.col())
        );
        let content = {
            //println!("start: {} end: {}", span.start, span.end);
            //println!("span: '{}'", &span.content[span.start..span.end]);
            let line_start = &self.span.content[span.line_start()..span.start];
            let error = span.content[span.start..span.end].underline().red();
            //println!("span end: {} span line end: {}", self.span.end, span.line_end());
            let line_end = &self.span.content[span.end..span.line_end()];

            format!("|\n|  {}{}{}\n", line_start, error, line_end)
        };

        let result = format!("{}{}", header, content);
        write!(f, "{}\n{}\n\n", result, self.message)
    }
}
