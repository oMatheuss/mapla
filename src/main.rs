use compiler::Compiler;
use error::Result;
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
    let ast = Parser::new(sm).parse()?;

    let assembly = Compiler::new(args.target).compile(ast);

    std::fs::write(args.output_path(), assembly)?;

    Ok(())
}

fn main() -> std::process::ExitCode {
    if let Err(err) = run() {
        eprintln!("{err}");
        return std::process::ExitCode::FAILURE;
    }

    return std::process::ExitCode::SUCCESS;
}
