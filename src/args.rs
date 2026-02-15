use std::path::PathBuf;

use crate::error::{Error, Result};
use crate::target::CompilerTarget;

#[derive(Debug, Default)]
pub struct CompilerConfig {
    pub dir: PathBuf,
    pub input: PathBuf,
    pub output: Option<PathBuf>,
    pub target: CompilerTarget,
}

pub fn parse_args() -> Result<CompilerConfig> {
    let mut args = std::env::args();

    let mut input = None;
    let mut config = CompilerConfig::default();

    config.dir = std::env::current_dir()?;
    args.next().expect("first argument should be program path");

    while let Some(arg) = args.next() {
        match arg.as_str() {
            "-o" | "--output" => {
                config.output = match args.next() {
                    Some(output) => Some(output.into()),
                    None => Error::cli("output path must be informed after -o flag")?,
                }
            }
            "-t" | "--target" => {
                config.target = match args.next() {
                    Some(target) => target.parse()?,
                    None => Error::cli("target must be informed after -t flag")?,
                }
            }
            _ => input = Some(arg),
        }
    }

    config.input = match input {
        Some(input) => input.into(),
        None => Error::cli("no input file provided")?,
    };

    Ok(config)
}

impl CompilerConfig {
    pub fn output_path(&self) -> PathBuf {
        self.output
            .as_ref()
            .and_then(|o| Some(self.dir.join(o)))
            .unwrap_or_else(|| self.dir.join(&self.input).with_extension("asm"))
    }
}
