use std::fmt::{Debug, Display};

use crate::position::Position;

#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
    message: String,
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

impl Error {
    pub fn lexical<T>(message: &str, position: Position) -> Result<T> {
        Err(Error {
            kind: ErrorKind::LexicalError,
            message: format!("{position} -> {message}"),
        })
    }

    pub fn syntatic<T>(message: &str, position: Position) -> Result<T> {
        Err(Error {
            kind: ErrorKind::SyntaxError,
            message: format!("{position} -> {message}"),
        })
    }

    pub fn cli<T>(message: &str) -> Result<T> {
        Err(Error {
            kind: ErrorKind::CliError,
            message: message.to_string(),
        })
    }
}

impl From<std::io::Error> for Error {
    fn from(value: std::io::Error) -> Self {
        Self {
            kind: ErrorKind::IoError,
            message: value.to_string(),
        }
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
            Err(err) => Err(Error {
                kind: ErrorKind::LexicalError,
                message: format!("{position} -> {err}"),
            }),
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
