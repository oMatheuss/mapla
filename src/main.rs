use std::env;

use compiler::Compiler;
use lexer::Lexer;
use parser::Parser;

mod asm;
mod ast;
mod compiler;
mod error;
mod intrinsic;
mod lexer;
mod parser;
mod position;
mod token;

fn main() -> std::io::Result<()> {
    let cur_dir = env::current_dir()?;
    let args = env::args().collect::<Vec<_>>();

    let Some(file) = args.get(1) else {
        return Err(std::io::Error::other("Error: no input file provided"));
    };
    
    let in_file = cur_dir.join(file);
    let out_file = in_file.with_extension("asm");

    let code = std::fs::read_to_string(in_file)?;

    match Lexer::parse(&code) {
        Ok(tokens) => match Parser::new(&tokens).parse() {
            Ok(ast) => {
                std::fs::write(out_file, Compiler::compile(ast))?;
            }
            Err(err) => eprintln!("{file}:{err}"),
        },
        Err(err) => eprintln!("{file}:{err}"),
    };

    Ok(())
}
