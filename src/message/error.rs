use crate::ast::Span;

use colored::Colorize;
use std::fmt::Display;

#[derive(Debug)]
pub struct Error {
    span: Option<Span>,
    err_type: ErrorType,
    msg: String,
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum ErrorType {
    ParsingError,
    PestError,
    UnreachableCode,
    NotImplemented,
    FloatParsingError,
    IntParsingError,
    OperatorDefinitionError,
    UnspecifiedError,
    UnsupportedError,
}

#[derive(Debug)]
pub enum Warning {}

impl Error {
    pub fn with_type(&mut self, err_type: ErrorType) -> &mut Self {
        self.err_type = err_type;
        self
    }

    pub fn with_span(&mut self, span: Span) -> &mut Self {
        self.span = Some(span);
        self
    }

    pub fn with_message(&mut self, msg: String) -> &mut Self {
        self.msg = msg;
        self
    }

    pub fn build(&mut self) -> Self {
        let error = std::mem::take(self);
        error
    }
}

impl Default for Error {
    fn default() -> Self {
        Self {
            span: None::<Span>,
            err_type: ErrorType::UnspecifiedError,
            msg: "".to_string(),
        }
    }
}

#[allow(unreachable_patterns)]
impl Display for ErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let err_string = match self {
            ErrorType::ParsingError => "parsing error",
            ErrorType::UnreachableCode => "unreachable code error",
            ErrorType::NotImplemented => "not implemented error",
            ErrorType::FloatParsingError => "failed to parse strig to float",
            ErrorType::IntParsingError => "failed to parse string to int",
            ErrorType::OperatorDefinitionError => "invalid operator definition",
            ErrorType::UnspecifiedError => "unspecified error",
            ErrorType::UnsupportedError => "feature currently unsupported",
            ErrorType::PestError => "error from internal parser",
        };

        write!(f, "{}", err_string)
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut output = format!("{}", self.err_type);
        if let Some(start) = &self.span {
            output += format!("at {}", start.start_ln()).as_str();
        }
        if self.msg.len() > 0 {
            output += format!(": {}", self.msg).as_str();
        }
        write!(f, "{}", output.red())
    }
}

impl From<std::num::ParseFloatError> for Error {
    fn from(_: std::num::ParseFloatError) -> Self {
        Error::default()
            .with_type(ErrorType::FloatParsingError)
            .build()
    }
}

impl From<std::num::ParseIntError> for Error {
    fn from(_: std::num::ParseIntError) -> Self {
        Error::default()
            .with_type(ErrorType::IntParsingError)
            .build()
    }
}

impl<T> From<pest::error::Error<T>> for Error {
    fn from(_: pest::error::Error<T>) -> Self {
        Error::default().with_type(ErrorType::PestError).build()
    }
}
