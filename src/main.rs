use std::{fs::File, io::Read, path::Path, process::exit};

use osql_parser::{
    lexer::Lexer,
    parser::{Parser, ParserError, ParserErrorWithBacktrace},
};

fn main() {
    let mut args = std::env::args();

    _ = args.next();

    let input_file_name = args.next().unwrap();

    let mut input_file = File::open(Path::new(&input_file_name)).unwrap();

    let mut input = String::new();

    input_file.read_to_string(&mut input).unwrap();

    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);

    let out = match parser.parse_program() {
        Ok(val) => val,
        Err(ParserErrorWithBacktrace {
            inner: ParserError::PeekFailed { expected, got },
            backtrace,
            ..
        }) => {
            eprintln!("Backtrace: {backtrace}");
            eprintln!("Failed to parse expected: {expected}");
            eprintln!("Got: {got:?}");
            match got {
                None => {}
                Some(tok) => {
                    eprint!("{}", &input[0..tok.start.idx]);
                    eprint!("\x1b[0;31m{}\x1b[0;37m", &input[tok.start.idx..tok.end.idx]);
                    eprintln!("{}", &input[tok.end.idx..]);
                }
            }
            exit(1)
        }
        Err(ParserErrorWithBacktrace {
            inner: ParserError::NoPrefixParseFn(tok),
            backtrace,
            ..
        }) => {
            eprintln!("Backtrace: {backtrace}");
            eprintln!("No prefix parse function for: {}", tok.kind);
            eprint!("{}", &input[0..tok.start.idx]);
            eprint!("{{|{}|}}", &input[tok.start.idx..tok.end.idx]);
            eprintln!("{}", &input[tok.end.idx..]);
            exit(1)
        }
        Err(err) => {
            println!("Backtrace: {}", err.backtrace);
            panic!("Unhandeled error: {err}")
        }
    };

    println!("{out}")
}
