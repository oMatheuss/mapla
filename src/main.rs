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

const CODE: &str = r#"
use io;

func fib(num: int): int do
    if num < 2 then
        return num;
    end

    return fib(num - 1) + fib(num - 2);
end

func main() do
    int r = fib(10);
    printInt(r);
end
"#;

fn main() -> Result<()> {
    let lex = Lexer::from_source(CODE);
    let tokens = lex.collect()?;

    let parser = Parser::new(&tokens);
    let ast = parser.parse()?;

    let asm = Compiler::compile(ast);

    println!("{asm}");

    Ok(())
}
