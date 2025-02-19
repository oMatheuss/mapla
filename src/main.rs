use error::Result;
use lexer::Lexer;
use parser::Parser;

mod ast;
mod error;
mod lexer;
mod parser;
mod position;
mod token;

const CODE: &str = r#"
func euler(limit: int): real do
    real soma = .0
    for i to limit then
        int fatorial = 1
        int j = 1
        while (j += 1) < i then fatorial *= j end
        soma += 1 / fatorial
    end

    return soma
end

real result = euler(10)
"#;

fn main() -> Result<()> {
    let lex = Lexer::from_source(CODE);
    let tokens = lex.collect()?;

    let parser = Parser::new(&tokens);
    let ast = parser.parse();

    match ast {
        Ok(ast) => println!("{ast:?}"),
        Err(err) => eprintln!("{err}"),
    };

    Ok(())
}
