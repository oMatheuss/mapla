use std::process::ExitCode;

use compiler::Compiler;
use error::Result;
use lexer::Lexer;
use parser::Parser;
use source::SourceManager;

mod args;
mod asm;
mod ast;
mod compiler;
mod error;
mod lexer;
mod parser;
mod position;
mod source;
mod target;
mod token;
mod utils;

fn run() -> Result<()> {
    let args = args::parse_args()?;

    let sm = SourceManager::new(&args.input, args.dir.clone());
    let mut main = sm.main()?;

    let tokens = Lexer::new(&mut main)
        .into_iter()
        .collect::<Result<Vec<_>>>()?;
    let ast = Parser::new(&tokens).parse()?;
    let assembly = Compiler::new(args.target).compile(ast);

    std::fs::write(args.output_path(), assembly)?;

    Ok(())
}

fn main() -> ExitCode {
    if run().inspect_err(|err| eprintln!("{err}")).is_ok() {
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    }
}
