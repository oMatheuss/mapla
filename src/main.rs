use error::Result;
use lexer::Lexer;
use parser::Parser;

mod ast;
mod error;
mod lexer;
mod parser;
mod token;

const CODE: &str = r#"
real euler = 0

for i to 10 then
    int fatorial = 1
    int j = 1
    while (j += 1) < i then fatorial *= j end
    euler += 1 / fatorial
end
"#;

fn main() -> Result<()> {
    let lex = Lexer::from_source(CODE);
    let tokens = lex.collect_all()?;

    let parser = Parser::new(&tokens);
    let ast = parser.parse();

    match ast {
        Ok(ast) => println!("{ast:?}"),
        Err(err) => eprintln!("{err}"),
    };

    Ok(())
}
