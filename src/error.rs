use std::fmt::{Debug, Display};

use crate::position::Position;

#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
    message: String,
    position: Position,
}

#[derive(Debug)]
pub enum ErrorKind {
    LexicalError,
    SyntaxError,
}

pub type Result<T> = std::result::Result<T, Error>;

impl Error {
    pub fn lexical<T>(message: &str, position: Position) -> Result<T> {
        Err(Error {
            kind: ErrorKind::LexicalError,
            message: String::from(message),
            position,
        })
    }

    pub fn syntatic<T>(message: &str, position: Position) -> Result<T> {
        Err(Error {
            kind: ErrorKind::SyntaxError,
            message: String::from(message),
            position,
        })
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
                message: err.to_string(),
                position,
            }),
        }
    }
}

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::LexicalError => write!(f, "Lexical Error"),
            ErrorKind::SyntaxError => write!(f, "Syntax Error"),
        }
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} -> {}: {}", self.position, self.kind, self.message)
    }
}
