use std::path::PathBuf;

use crate::error::{Error, Result};
use crate::target::CompilerTarget;

pub struct ParsedArgs {
    pub dir: PathBuf,
    pub input: String,
    pub output: Option<String>,
    pub target: CompilerTarget,
}

pub fn parse_args() -> Result<ParsedArgs> {
    let mut args = std::env::args();
    let dir = std::env::current_dir()?;

    let mut input = None;
    let mut output = None;
    let mut target = CompilerTarget::Linux;

    args.next().expect("first argument should be program path");

    while let Some(arg) = args.next() {
        match arg.as_str() {
            "-o" => {
                let Some(sout) = args.next() else {
                    return Error::cli("output path must be informed after -o flag");
                };

                output = Some(sout);
            }
            "-t" => {
                let Some(starget) = args.next() else {
                    return Error::cli("target must be informed after -t flag");
                };

                target = starget.parse()?;
            }
            _ => input = Some(arg),
        }
    }

    let Some(input) = input else {
        return Error::cli("no input file provided");
    };

    Ok(ParsedArgs {
        dir,
        input,
        output,
        target,
    })
}

impl ParsedArgs {
    pub fn output_path(&self) -> PathBuf {
        self.output
            .as_ref()
            .and_then(|o| Some(self.dir.join(o)))
            .unwrap_or_else(|| self.dir.join(&self.input).with_extension("asm"))
    }
}
