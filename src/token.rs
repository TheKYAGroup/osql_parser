use std::{fmt::Display, hash::Hash, mem::discriminant, ops::Index, slice::SliceIndex};

use derive_more::Display;

#[derive(Debug, Clone, Eq, Display)]
pub enum TokenKind {
    /// (
    #[display("(")]
    LParen,
    /// )
    #[display(")")]
    RParen,

    /// <
    #[display("<")]
    LT,
    /// >
    #[display(">")]
    GT,

    /// <=
    #[display("<=")]
    LTEq,
    /// >=
    #[display(">=")]
    GTEq,

    /// .
    #[display(".")]
    Period,
    /// ,
    #[display(",")]
    Comma,

    /// FROM
    #[display("FROM")]
    From,

    /// WHERE
    #[display("WHERE")]
    Where,

    /// INNER
    #[display("INNER")]
    Inner,

    /// LEFT
    #[display("LEFT")]
    Left,

    /// JOIN
    #[display("JOIN")]
    Join,

    /// SELECT
    #[display("SELECT")]
    Select,

    /// ON
    #[display("ON")]
    On,

    /// IN
    #[display("IN")]
    In,

    /// CASE
    #[display("CASE")]
    Case,

    /// WHEN
    #[display("WHEN")]
    When,

    /// THEN
    #[display("THEN")]
    Then,

    /// ELSE
    #[display("ELSE")]
    Else,

    /// END
    #[display("END")]
    End,

    /// AS
    #[display("AS")]
    As,

    /// AND
    #[display("AND")]
    And,

    /// OR
    #[display("OR")]
    Or,

    /// =
    #[display("=")]
    Eq,

    /// *
    #[display("*")]
    Asterisk,

    /// -
    #[display("-")]
    Sub,

    /// ;
    #[display(";")]
    Semicolon,

    /// /
    #[display("/")]
    Slash,

    Ident(String),

    String(String),

    #[display("/*{_0}*/")]
    Comment(String),

    Unkown(char),

    Whitespace(String),

    Integer(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub start: Loc,
    pub end: Loc,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}

pub trait GetKind {
    fn get_kind<'a>(&'a self) -> Option<&'a TokenKind>;
}

impl GetKind for Option<Token> {
    fn get_kind<'a>(&'a self) -> Option<&'a TokenKind> {
        self.as_ref().map(|tok| &tok.kind)
    }
}

impl<'b> GetKind for Option<&'b Token> {
    fn get_kind<'a>(&'a self) -> Option<&'a TokenKind> {
        self.as_ref().map(|tok| &tok.kind)
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
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

impl Hash for TokenKind {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let disc = discriminant(self);
        Hash::hash(&disc, state);
    }
}

impl PartialEq for TokenKind {
    fn eq(&self, other: &Self) -> bool {
        PartialEq::eq(&discriminant(self), &discriminant(other))
    }
}

impl TokenKind {
    pub fn ident(s: &str) -> Self {
        TokenKind::Ident(s.to_string())
    }

    pub fn string(s: &str) -> Self {
        TokenKind::String(s.to_string())
    }
}

pub fn ident_map(ident: String) -> TokenKind {
    match ident.to_lowercase().as_str() {
        "select" => TokenKind::Select,
        "from" => TokenKind::From,
        "where" => TokenKind::Where,
        "inner" => TokenKind::Inner,
        "join" => TokenKind::Join,
        "on" => TokenKind::On,
        "in" => TokenKind::In,
        "case" => TokenKind::Case,
        "when" => TokenKind::When,
        "then" => TokenKind::Then,
        "else" => TokenKind::Else,
        "end" => TokenKind::End,
        "as" => TokenKind::As,
        "left" => TokenKind::Left,
        "and" => TokenKind::And,
        "or" => TokenKind::Or,
        _ => TokenKind::Ident(ident),
    }
}
