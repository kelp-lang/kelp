use kelp_message::MessageDispatcher;
use kelp_parser::*;
use kelp_stree::*;

pub struct App {
    pub message_dispatcher: MessageDispatcher,
    pub parser: Option<Parser>,
}

impl App {
    pub fn run(mut self) {
        println!("App running");
    }
}

impl Default for App {
    fn default() -> Self {
        Self {
            message_dispatcher: MessageDispatcher::default(),
            parser: None,
        }
    }
}
