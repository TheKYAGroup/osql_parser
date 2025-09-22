#![feature(error_generic_member_access)]
#![feature(never_type)]

use std::ops::Index;

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
    parser.parse_program().map_err(|err| err.inner.clone())
}

#[memoize]
pub fn parse_with_backtrace(
    input: EcoString,
) -> Result<Program, Box<parser::ParserErrorWithBacktrace>> {
    let lexer = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(lexer);
    parser.parse_program()
}

#[derive(Debug, Clone, PartialEq, Default, Hash, Copy)]
pub struct Loc {
    pub line: usize,
    pub col: usize,
    pub idx: usize,
}

impl<T> Index<Loc> for [T] {
    type Output = T;
    fn index(&self, idx: Loc) -> &Self::Output {
        &self[idx.idx]
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Span {
    pub start: Loc,
    pub end: Loc,
}
