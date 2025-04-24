use std::{hash::Hash, mem::discriminant};

use derive_more::Display;
use logos::{Lexer, Logos};

#[derive(Debug, Clone, Eq, Display)]
pub enum Token {
    /// (
    LParen,
    /// )
    RParen,

    /// .
    Period,
    /// ,
    Comma,

    /// FROM
    From,

    /// WHERE
    Where,

    /// INNER
    Inner,

    /// JOIN
    Join,

    /// SELECT
    Select,

    /// ON
    On,

    /// IN
    In,

    /// =
    Eq,

    /// *
    Asterisk,

    /// ;
    Semicolon,

    Ident(String),

    String(String),

    Comment(String),

    Unkown(char),
}

impl Hash for Token {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let disc = discriminant(self);
        Hash::hash(&disc, state);
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        PartialEq::eq(&discriminant(self), &discriminant(other))
    }
}

impl Token {
    pub fn ident(s: &str) -> Self {
        Token::Ident(s.to_string())
    }

    pub fn string(s: &str) -> Self {
        Token::String(s.to_string())
    }
}

pub fn ident_map(ident: String) -> Token {
    match ident.to_lowercase().as_str() {
        "select" => Token::Select,
        "from" => Token::From,
        "where" => Token::Where,
        "inner" => Token::Inner,
        "join" => Token::Join,
        "on" => Token::On,
        "in" => Token::In,
        _ => Token::Ident(ident),
    }
}
