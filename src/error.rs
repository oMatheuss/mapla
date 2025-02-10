#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
    message: &'static str,
}

#[derive(Debug)]
pub enum ErrorKind {
    LexicalError,
    SyntaxError,
}

pub type Result<T> = std::result::Result<T, Error>;

impl Error {
    pub fn lexical<T>() -> Result<T> {
        Err(Error {
            kind: ErrorKind::LexicalError,
            message: "",
        })
    }

    pub fn syntatic<T>(message: &'static str) -> Result<T> {
        Err(Error {
            kind: ErrorKind::SyntaxError,
            message,
        })
    }
}

impl From<std::num::ParseIntError> for Error {
    fn from(value: std::num::ParseIntError) -> Self {
        let message = match value.kind() {
            std::num::IntErrorKind::Empty => todo!(),
            std::num::IntErrorKind::InvalidDigit => "invalid digit",
            std::num::IntErrorKind::PosOverflow => "number too large",
            std::num::IntErrorKind::NegOverflow => "negative number too large",
            std::num::IntErrorKind::Zero => todo!(),
            _ => "invalid number",
        };

        Error {
            kind: ErrorKind::LexicalError,
            message,
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
        write!(f, "{}: {}", self.kind, self.message)
    }
}
