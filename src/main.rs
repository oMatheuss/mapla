use compiler::Compiler;
use error::Result;
use lexer::Lexer;
use parser::Parser;

mod asm;
mod ast;
mod compiler;
mod error;
mod lexer;
mod parser;
mod position;
mod token;

const CODE: &str = r#"
func soma(limit: int): int do
    int x = 0
    for i to limit then
        x += 1 * i + (i * 2 + i * 3)
    end

    return x
end

func main() do
    int r = soma(10)
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
