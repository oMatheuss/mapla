use std::convert::Infallible;
use std::str::FromStr;

#[derive(Debug, Clone, Copy)]
pub enum CompilerTarget {
    Linux,
    Windows,
    Unknown,
}

impl FromStr for CompilerTarget {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "linux" => Ok(CompilerTarget::Linux),
            "windows" | "win" => Ok(CompilerTarget::Windows),
            _ => Ok(CompilerTarget::Unknown),
        }
    }
}
