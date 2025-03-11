use compiler::Compiler;
use error::Result;
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

fn main() -> Result<()> {
    let code = match std::fs::read_to_string("examples/fibonacci.txt") {
        Ok(input) => input,
        Err(_) => todo!(),
    };

    let tokens = Lexer::from_source(&code).collect()?;
    let ast = Parser::new(&tokens).parse()?;

    let asm = Compiler::compile(ast);

    println!("{asm}");

    Ok(())
}
