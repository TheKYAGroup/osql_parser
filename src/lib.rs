#![feature(error_generic_member_access)]

use comemo::memoize;
use ecow::EcoString;

use crate::ast::Program;

pub mod ast;
pub mod lexer;
pub mod oir;
pub mod parser;
pub mod token;

#[memoize]
pub fn parse(input: EcoString) -> Result<Program, parser::ParserError> {
    let lexer = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(lexer);
    parser.parse_program().map_err(|err| err.inner)
}
