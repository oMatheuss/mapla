use std::process::ExitCode;
use std::{env, path::PathBuf};

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
mod utils;

struct ParsedArgs {
    input: String,
    output: Option<String>,
    target: CompilerTarget,
}

fn parse_args() -> Result<ParsedArgs> {
    let mut args = env::args();

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
        input,
        output,
        target,
    })
}

fn run() -> Result<()> {
    let args = parse_args()?;
    let cur_dir = env::current_dir()?;

    let in_file = cur_dir.join(&args.input);
    let out_file = args
        .output
        .map(PathBuf::from)
        .unwrap_or(in_file.with_extension("asm"));

    let source = &args.input;
    let code = std::fs::read_to_string(in_file)?;

    let tokens = Lexer::new(&code)
        .collect::<Result<Vec<_>>>()
        .map_err(|err| err.with_source(source))?;

    let ast = Parser::new(&tokens)
        .parse()
        .map_err(|err| err.with_source(source))?;

    let assembly = Compiler::new(args.target)
        .prolog()
        .compile(ast)
        .epilog()
        .assembly();

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
