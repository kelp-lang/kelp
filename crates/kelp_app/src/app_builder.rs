use crate::app::App;

use kelp_message::{MessageLevel, MessageOutput};
use kelp_parser::Parser;

pub struct AppBuilder {
    pub app: App,
}

impl Default for AppBuilder {
    fn default() -> Self {
        let mut app_builder = AppBuilder {
            app: App::default(),
        };

        app_builder
    }
}

impl AppBuilder {
    pub fn add_message_level(&mut self, message_level: MessageLevel) -> &mut Self {
        self.app.message_dispatcher.set_message_level(message_level);
        self
    }

    pub fn add_message_output(&mut self, message_output: MessageOutput) -> &mut Self {
        self.app
            .message_dispatcher
            .set_message_output(message_output);
        self
    }

    pub fn add_parser(&mut self) -> &mut Self {
        self.app.parser = Some(Parser::new(&self.app.message_dispatcher));
        self
    }
}
