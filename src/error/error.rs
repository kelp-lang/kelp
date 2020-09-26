use std::fmt::Display;

#[derive(Debug)]
pub struct Error {
    pub(crate) ln: usize,
    pub(crate) err_type: ErrorType,
    pub(crate) mess: String,
}

#[derive(Debug)]
pub enum ErrorType {
    ParsingError,
    UnreachableCode,
    NotImplemented,
}

#[derive(Debug)]
pub enum Warning {}

impl Display for ErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let err_string = match self {
            ErrorType::ParsingError => "parsing error",
            ErrorType::UnreachableCode => {
                "unreachable code, contact developer if you see this message"
            }
            ErrorType::NotImplemented => "not implemented, you should probably fix that",
            _ => "error",
        };

        write!(f, "{}", err_string)
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} at {}: {}", self.err_type, self.ln, self.mess)
    }
}
