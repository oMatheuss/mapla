use std::borrow::Cow;
use std::fmt::{Debug, Display};

use crate::position::Position;

#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
    message: Cow<'static, str>,
}

impl std::error::Error for Error {}

#[derive(Debug)]
pub enum ErrorKind {
    LexicalError,
    SyntaxError,
    CliError,
    IoError,
}

pub type Result<T> = std::result::Result<T, Error>;

impl<T> From<Error> for Result<T> {
    fn from(value: Error) -> Self {
        Self::Err(value)
    }
}

impl Error {
    pub fn new(kind: ErrorKind, message: impl Into<Cow<'static, str>>) -> Self {
        Self {
            kind,
            message: message.into(),
        }
    }

    pub fn lexical<T>(message: &str, position: Position) -> Result<T> {
        Self::new(ErrorKind::LexicalError, format!("{position} -> {message}")).into()
    }

    pub fn syntatic<T>(message: &str, position: Position) -> Result<T> {
        Self::new(ErrorKind::SyntaxError, format!("{position} -> {message}")).into()
    }

    pub fn cli<T>(message: impl Into<Cow<'static, str>>) -> Result<T> {
        Self::new(ErrorKind::CliError, message).into()
    }
}

impl From<std::io::Error> for Error {
    fn from(err: std::io::Error) -> Self {
        Self::new(ErrorKind::IoError, format!("{err}"))
    }
}

pub trait PositionResult<T> {
    fn with_position(self, position: Position) -> Result<T>;
}

impl<T, E> PositionResult<T> for std::result::Result<T, E>
where
    E: Display,
{
    fn with_position(self, position: Position) -> Result<T> {
        match self {
            Ok(ok) => Ok(ok),
            Err(err) => Error::lexical(&err.to_string(), position),
        }
    }
}

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::LexicalError => write!(f, "Lexical Error"),
            ErrorKind::SyntaxError => write!(f, "Syntax Error"),
            ErrorKind::CliError => write!(f, "Error"),
            ErrorKind::IoError => write!(f, "Io Error"),
        }
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.kind, self.message)
    }
}
