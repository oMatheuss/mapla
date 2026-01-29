use std::str::FromStr;

use crate::error::Error;

#[derive(Debug, Clone, Copy)]
pub enum CompilerTarget {
    Linux,
    Windows,
}

impl Default for CompilerTarget {
    fn default() -> Self {
        #[cfg(any(unix, not(windows)))]
        return Self::Linux;

        #[cfg(windows)]
        return Self::Windows;
    }
}

impl FromStr for CompilerTarget {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "linux" => Ok(CompilerTarget::Linux),
            "windows" | "win" => Ok(CompilerTarget::Windows),
            _ => Error::cli("unknown compiler target"),
        }
    }
}
