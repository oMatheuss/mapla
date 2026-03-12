use crate::{
    args::CompilerConfig,
    binder::Binder,
    codegen::CodeGen,
    error::{Error, Result},
    parser::Parser,
    source::SourceManager,
    target::CompilerTarget,
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
    let asm = compile(&args)?;

    let asm_file = args.output_file("asm");
    let obj_file = args.output_file("obj");
    let exe_file = match args.target {
        CompilerTarget::Linux => args.output_file(""),
        CompilerTarget::Windows => args.output_file("exe"),
    };

    std::fs::write(&asm_file, asm)?;
    if args.emit_asm && !args.emit_obj {
        return Ok(());
    }
    
    std::process::Command::new("nasm")
        .arg(match args.target {
            CompilerTarget::Linux => "-felf64",
            CompilerTarget::Windows => "-fwin64",
        })
        .arg(&asm_file)
        .arg("-o")
        .arg(&obj_file)
        .status()?;

    if !args.emit_asm {
        std::fs::remove_file(&asm_file)?;
    }
    if args.emit_obj {
        return Ok(());
    }

    if !args.target.is_current_platform() {
        std::fs::remove_file(&obj_file)?;
        Error::cli("cross-compilation is not supported yet")?;
    }

    std::process::Command::new("cc")
        .arg(&obj_file)
        .arg("-o")
        .arg(&exe_file)
        .status()?;

    std::fs::remove_file(&obj_file)?;

    if args.run {
        std::process::Command::new(&exe_file).status()?;
        std::fs::remove_file(&exe_file)?;
    }

    Ok(())
}

fn compile(args: &CompilerConfig) -> Result<String> {
    let mut sources = SourceManager::new(&args.input, &args.dir);
    let main = sources.main()?;
    let main = Parser::parse(main)?;
    let mut binder = Binder::new();

    let main_ns = main.namespace.clone();
    let mut files = main.imports.clone();
    let mut codes = vec![main];

    while let Some(file) = files.pop() {
        let code = sources.source(&file)?;
        if !code.visited {
            let code = Parser::parse(code)?;
            files.append(&mut code.imports.clone());
            codes.push(code);
        }
    }

    for code in codes.iter() {
        binder.bind_globals(&code)?;
    }

    while let Some(code) = codes.pop() {
        binder.bind_ast(code)?;
    }

    let mut codegen = CodeGen::new(args.target, binder.globals);
    codegen.gen_intro();

    for func in binder.functions {
        let is_entry = func.namespace == main_ns && func.name == "main";
        codegen.gen_func(func, is_entry);
    }

    codegen.gen_data(binder.data);

    Ok(codegen.to_string())
}

fn main() -> std::process::ExitCode {
    if let Err(err) = run() {
        eprintln!("{err}");
        return std::process::ExitCode::FAILURE;
    }

    return std::process::ExitCode::SUCCESS;
}
