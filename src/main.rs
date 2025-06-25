use std::{env, path::PathBuf};
use std::process::{ExitCode};

use compiler::Compiler;
use error::{Error, Result};
use lexer::Lexer;
use parser::Parser;
use target::CompilerTarget;

mod asm;
mod ast;
mod compiler;
mod error;
mod intrinsic;
mod lexer;
mod parser;
mod position;
mod target;
mod token;

fn run() -> Result<()> {
    let cur_dir = env::current_dir()?;
    let mut args = env::args();

    let mut file = None;
    let mut out_file = None;
    let mut target = CompilerTarget::Linux;

    args.next().expect("first argument should be program path");

    while let Some(arg) = args.next() {
        match arg.as_str() {
            "-o" => {
                let Some(sout) = args.next() else {
                    return Error::cli("output path must be informed after -o flag");
                };

                out_file = Some(PathBuf::from(sout));
            }
            "-t" => {
                let Some(starget) = args.next() else {
                    return Error::cli("target must be informed after -t flag");
                };

                target = starget.parse()?;
            }
            _ => file = Some(arg),
        }
    }

    let Some(file) = &file else {
        return Error::cli("no input file provided");
    };

    let in_file = cur_dir.join(file);
    let out_file = out_file.unwrap_or(in_file.with_extension("asm"));

    let code = std::fs::read_to_string(in_file)?;

    let tokens = Lexer::new(&code)
        .collect::<Result<Vec<_>>>()
        .map_err(|err| err.with_source(file))?;

    let ast = Parser::new(&tokens)
        .parse()
        .map_err(|err| err.with_source(file))?;

    let assembly = Compiler::compile(ast, target);

    std::fs::write(out_file, assembly)?;

    Ok(())
}

fn main() -> ExitCode {
    if run().inspect_err(|err| eprintln!("{err}")).is_ok() {
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    }
}
