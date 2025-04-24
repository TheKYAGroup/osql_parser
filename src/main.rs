use osql_parser::{lexer::Lexer, parser::Parser};

fn main() {
    let input = include_str!("test.sql");

    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);

    let out = parser.parse_program().unwrap();

    println!("{}", out)
}
