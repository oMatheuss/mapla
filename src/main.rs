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

fn main() {
    let file = "examples/fibonacci.txt";
    
    let code = match std::fs::read_to_string(file) {
        Ok(input) => input,
        Err(_) => todo!(),
    };

    match Lexer::parse(&code) {
        Ok(tokens) => match Parser::new(&tokens).parse() {
            Ok(ast) => println!("{asm}", asm = Compiler::compile(ast)),
            Err(err) => eprintln!("{file}:{err}"),
        },
        Err(err) => eprintln!("{file}:{err}"),
    };
}
