use clap::{crate_authors, crate_name, crate_version, App};
use kelp::{env::EnvironmentStore, error_e, info, warning};
use rustyline::{
    config::Configurer,
    highlight::{Highlighter, MatchingBracketHighlighter},
    validate::{MatchingBracketValidator, ValidationResult, Validator},
    Config, Editor, OutputStreamType, hint::Hinter
};
use rustyline_derive::{Completer, Helper, Hinter};

#[derive(Completer, Hinter, Helper)]
struct AtomHelper {
    highlighter: MatchingBracketHighlighter,
    validator: MatchingBracketValidator,
    colored_prompt: String,
}

impl Highlighter for AtomHelper {
    fn highlight<'l>(&self, line: &'l str, pos: usize) -> std::borrow::Cow<'l, str> {
        let _ = pos;
        std::borrow::Cow::Borrowed(line)
    }

    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &'s self,
        prompt: &'p str,
        default: bool,
    ) -> std::borrow::Cow<'b, str> {
        let _ = default;
        std::borrow::Cow::Borrowed(prompt)
    }

    fn highlight_hint<'h>(&self, hint: &'h str) -> std::borrow::Cow<'h, str> {
        std::borrow::Cow::Borrowed(hint)
    }

    fn highlight_candidate<'c>(
        &self,
        candidate: &'c str,
        completion: rustyline::CompletionType,
    ) -> std::borrow::Cow<'c, str> {
        let _ = completion;
        std::borrow::Cow::Borrowed(candidate)
    }

    fn highlight_char(&self, line: &str, pos: usize) -> bool {
        let _ = (line, pos);
        false
    }
}

impl Validator for AtomHelper {
    fn validate(
        &self,
        ctx: &mut rustyline::validate::ValidationContext,
    ) -> rustyline::Result<rustyline::validate::ValidationResult> {
        let paren_balance = ctx.input().chars().fold(0, |parity, c| match c {
            '(' => parity + 1,
            ')' => parity - 1,
            _ => parity,
        });

        if paren_balance == 0 {
            Ok(ValidationResult::Valid(None))
        } else if paren_balance > 0 {
            Ok(ValidationResult::Incomplete)
        } else {
            Ok(ValidationResult::Invalid(None))
        }
    }

    fn validate_while_typing(&self) -> bool {
        false
    }
}

fn main() {
    let matches = App::new(crate_name!())
        .version(crate_version!())
        .author(crate_authors!())
        .get_matches();

    //println!("{}", kelp::link::link("https://omase.tk", "lol"));

    info("Running interactive!");

    let mut rl = Editor::new();
    let h = AtomHelper {
        highlighter: MatchingBracketHighlighter::new(),
        colored_prompt: "".to_string(),
        validator: MatchingBracketValidator::new(),
    };
    rl.set_helper(Some(h));

    if rl.load_history(".kelp_history").is_err() {
        warning("No previous history");
    }

    //let mut compiler = Compiler::new();
    let mut env_store = EnvironmentStore::new();
    let env_id = env_store.new_env(None);

    let mut buffer: String = String::new();
    loop {
        let readline = rl.readline("kelp~> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(&line);
                rl.save_history(".kelp_history").unwrap();
                if line.len() > 0 {
                    kelp::rep(
                        line.as_str(),
                        "interactive".to_string(),
                        env_id,
                        &mut env_store,
                    );
                    /*buffer += &line;
                    if buffer.chars().fold(0, |parity, c| match c {
                        '(' => parity + 1,
                        ')' => parity - 1,
                        _ => parity,
                    }) == 0 {
                        kelp::rep(buffer.as_str(), "interactive".to_string(), env_id, &mut env_store);
                        buffer = String::new();
                    } else {
                        print!(">>>  ");
                        continue;
                    }*/
                }
            }
            Err(rustyline::error::ReadlineError::Interrupted) => break,
            Err(rustyline::error::ReadlineError::Eof) => break,
            Err(err) => {
                error_e!("{}", err);
            }
        }
    }
}
