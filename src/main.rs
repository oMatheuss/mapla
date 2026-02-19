use crate::{
    binder::Binder, codegen::CodeGen, error::Result, parser::Parser, source::SourceManager,
};

mod args;
mod ast;
mod binder;
mod codegen;
mod error;
mod ir;
mod lexer;
mod parser;
mod position;
mod source;
mod symbols;
mod target;
mod token;
mod types;
mod utils;

fn run() -> Result<()> {
    let args = args::parse_args()?;

    let mut sources = SourceManager::new(&args.input, &args.dir);
    let main = sources.main()?;
    let main = Parser::parse(main)?;
    let mut binder = Binder::new();

    let main_ns = main.namespace.clone();
    let mut files = main.imports.clone();
    let mut codes = Vec::new();

    while let Some(file) = files.pop() {
        let code = sources.source(&file)?;
        if !code.visited {
            let code = Parser::parse(code)?;
            files.append(&mut code.imports.clone());
            binder.bind_globals(&code)?;
            codes.push(code);
        }
    }

    while let Some(code) = codes.pop() {
        binder.bind_ast(code)?;
    }

    binder.bind_globals(&main)?;
    binder.bind_ast(main)?;

    let mut codegen = CodeGen::new(args.target, binder.globals);
    codegen.gen_intro();

    for func in binder.functions {
        let is_entry = func.namespace == main_ns && func.name == "main";
        codegen.gen_func(func, is_entry);
    }

    codegen.gen_data(binder.data);

    std::fs::write(args.output_path(), codegen.to_string())?;

    Ok(())
}

fn main() -> std::process::ExitCode {
    if let Err(err) = run() {
        eprintln!("{err}");
        return std::process::ExitCode::FAILURE;
    }

    return std::process::ExitCode::SUCCESS;
}
