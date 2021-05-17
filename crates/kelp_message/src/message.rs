pub use crate::error::{Error, ErrorType};
use colored::Colorize;
use std::{cell::RefCell, fmt::Display, rc::Rc};

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
    error_count: Rc<RefCell<usize>>,
    warn_count: Rc<RefCell<usize>>,
    level: Rc<RefCell<MessageLevel>>,
    output: Rc<RefCell<MessageOutput>>,
}

impl MessageDispatcher {
    pub fn new(msg_level: MessageLevel, msg_out: MessageOutput) -> Self {
        Self {
            level: Rc::new(RefCell::new(msg_level)),
            output: Rc::new(RefCell::new(msg_out)),
            error_count: Rc::new(RefCell::new(0)),
            warn_count: Rc::new(RefCell::new(0)),
        }
    }
    pub fn dispatch<T: Display>(&mut self, message: T, message_level: MessageLevel) {
        match message_level {
            MessageLevel::Error => *self.error_count.borrow_mut() += 1,
            MessageLevel::Silent => {}
            MessageLevel::Warning => {}
            MessageLevel::Info => {}
        }
        self.print(message.to_string());
    }
    pub fn print_stats(&self) {
        let errors = "errors".red();
        let warnings = "warnings".yellow();
        let err_count = *self.error_count.borrow();
        let warn_count = *self.warn_count.borrow();
        self.print("\n\n--------------------------".to_string());
        if err_count > 0 {
            self.print(format!("{}: {}", errors, err_count))
        };
        if warn_count > 0 {
            self.print(format!("{}: {}", warnings, warn_count))
        }
    }

    fn print(&self, message: String) {
        match &self.output.borrow().clone() {
            MessageOutput::Stderr => eprintln! {"{}\n", message},
            MessageOutput::Stdout => println! {"{}\n", message},
            MessageOutput::File(_path) => todo!(),
        }
    }

    pub fn set_message_level(&mut self, message_level: MessageLevel) -> &mut Self {
        self.level.replace(message_level);
        self
    }

    pub fn set_message_output(&mut self, message_output: MessageOutput) -> &mut Self {
        self.output.replace(message_output);
        self
    }
}

impl Default for MessageDispatcher {
    fn default() -> Self {
        Self {
            level: Rc::new(RefCell::new(MessageLevel::Error)),
            output: Rc::new(RefCell::new(MessageOutput::Stdout)),
            error_count: Rc::new(RefCell::new(0)),
            warn_count: Rc::new(RefCell::new(0)),
        }
    }
}
