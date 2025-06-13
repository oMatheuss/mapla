use std::{env, path::PathBuf};

use compiler::Compiler;
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
mod token;
mod target;

fn main() -> std::io::Result<()> {
    let cur_dir = env::current_dir()?;
    let mut args = env::args();
    
    let mut file = None;
    let mut out_file = None;
    let mut target = CompilerTarget::Unknown;

    while let Some(arg) = args.next() {
        match arg.as_str() {
            "-o" => {
                let Some(sout) = args.next() else {
                    return Err(std::io::Error::other("Error: output path must be informed after -o flag"));
                };

                out_file = Some(PathBuf::from(sout));
            },
            "-t" => {
                let Some(starget) = args.next() else {
                    return Err(std::io::Error::other("Error: target must be informed after -t flag"));
                };

                target = starget.parse().unwrap();
            },
            _ => file = Some(arg),
        }
    }

    let Some(file) = &file else {
        return Err(std::io::Error::other("Error: no input file provided"));
    };

    let in_file = cur_dir.join(file);
    let out_file = out_file.unwrap_or(in_file.with_extension("asm"));

    let code = std::fs::read_to_string(in_file)?;

    match Lexer::lex(&code) {
        Ok(tokens) => match Parser::new(&tokens).parse() {
            Ok(ast) => {
                let assembly = Compiler::compile(ast, target);
                std::fs::write(out_file, assembly)?;
            }
            Err(err) => eprintln!("{file}:{err}"),
        },
        Err(err) => eprintln!("{file}:{err}"),
    };

    Ok(())
}
