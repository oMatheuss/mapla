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
    let file = args.get(1).unwrap();

    let path = cur_dir.join(file);
    let code = std::fs::read_to_string(path)?;

    let out = cur_dir.join("out.asm");

    match Lexer::parse(&code) {
        Ok(tokens) => match Parser::new(&tokens).parse() {
            Ok(ast) => {
                std::fs::write(out, Compiler::compile(ast))?;
            }
            Err(err) => eprintln!("{file}:{err}"),
        },
        Err(err) => eprintln!("{file}:{err}"),
    };

    Ok(())
}
