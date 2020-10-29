use std::fmt::Display;

#[derive(Debug)]
pub struct Error {
    pub(crate) start: usize,
    pub(crate) end: usize,
    pub(crate) err_type: ErrorType,
    pub(crate) msg: String,
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum ErrorType {
    ParsingError,
    UnreachableCode,
    NotImplemented,
    FloatParsingError,
    IntParsingError,
}

#[derive(Debug)]
pub enum Warning {}

#[allow(unreachable_patterns)]
impl Display for ErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let err_string = match self {
            ErrorType::ParsingError => "parsing error",
            ErrorType::UnreachableCode => {
                "unreachable code, contact developer if you see this message"
            }
            ErrorType::NotImplemented => "not implemented, you should probably fix that",
            ErrorType::FloatParsingError => "failed to parse float",
            ErrorType::IntParsingError => "failed to parse int",
            _ => "error",
        };

        write!(f, "{}", err_string)
    }
}

impl Display for Error {
    fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!("add span support");
        //write!(f, "{} at {}: {}", self.err_type, self.start, self.msg)
    }
}

impl From<std::num::ParseFloatError> for Error {
    fn from(_: std::num::ParseFloatError) -> Self {
        Error {
            start: 0,
            end: 0,
            err_type: ErrorType::FloatParsingError,
            msg: "".to_string(),
        }
    }
}

impl From<std::num::ParseIntError> for Error {
    fn from(_: std::num::ParseIntError) -> Self {
        Error {
            start: 0,
            end: 0,
            err_type: ErrorType::IntParsingError,
            msg: "".to_string(),
        }
    }
}
