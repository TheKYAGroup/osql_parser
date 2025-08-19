use std::{fmt::Display, hash::Hash, mem::discriminant, ops::Index};

use derive_more::Display;
use ecow::EcoString;
use osql_parser_derive::Ident;

use crate::{Loc, Span};

#[derive(Debug, Clone, Eq, Display, Ident)]
pub enum TokenKind {
    /// (
    #[display("(")]
    LParen,
    /// )
    #[display(")")]
    RParen,

    /// {
    #[display("{{")]
    LBracket,
    /// }
    #[display("}}")]
    RBracket,

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
    #[ident]
    From,

    /// WHERE
    #[display("WHERE")]
    #[ident]
    Where,

    /// INNER
    #[display("INNER")]
    #[ident]
    Inner,

    /// LEFT
    #[display("LEFT")]
    #[ident]
    Left,

    /// JOIN
    #[display("JOIN")]
    #[ident]
    Join,

    /// SELECT
    #[display("SELECT")]
    #[ident]
    Select,

    /// ON
    #[display("ON")]
    #[ident]
    On,

    /// IN
    #[display("IN")]
    #[ident]
    In,

    /// CASE
    #[display("CASE")]
    #[ident]
    Case,

    /// WHEN
    #[display("WHEN")]
    #[ident]
    When,

    /// THEN
    #[display("THEN")]
    #[ident]
    Then,

    /// ELSE
    #[display("ELSE")]
    #[ident]
    Else,

    /// END
    #[display("END")]
    #[ident]
    End,

    /// AS
    #[display("AS")]
    #[ident]
    As,

    /// AND
    #[display("AND")]
    #[ident]
    And,

    /// OR
    #[display("OR")]
    #[ident]
    Or,

    /// GROUP
    #[display("GROUP")]
    #[ident]
    Group,

    /// BY
    #[display("BY")]
    #[ident]
    By,

    /// IS
    #[display("IS")]
    #[ident]
    Is,

    /// NULL
    #[display("NULL")]
    #[ident]
    Null,

    /// NOT
    #[display("NOT")]
    #[ident]
    Not,

    /// USING
    #[display("USING")]
    #[ident]
    Using,

    /// LIKE
    #[display("LIKE")]
    #[ident]
    Like,

    /// UNION
    #[display("UNION")]
    #[ident]
    Union,

    /// ALL
    #[display("ALL")]
    #[ident]
    All,

    /// DATE
    #[display("DATE")]
    #[ident]
    Date,

    /// BETWEEN
    #[display("BETWEEN")]
    #[ident]
    Between,

    /// OUTER
    #[display("OUTER")]
    #[ident]
    Outer,

    /// DISTINCT
    #[ident]
    #[display("DISTINCT")]
    Distinct,

    /// FULL
    #[display("FULL")]
    #[ident]
    Full,

    /// FETCH
    #[display("FETCH")]
    #[ident]
    Fetch,

    /// FIRST
    #[display("FIRST")]
    #[ident]
    First,

    /// NEXT
    #[display("NEXT")]
    #[ident]
    Next,

    /// ROWS
    #[display("ROWS")]
    #[ident]
    Rows,

    /// ROW
    #[display("ROW")]
    #[ident]
    Row,

    /// ONLY
    #[display("ONLY")]
    #[ident]
    Only,

    /// =
    #[display("=")]
    Eq,

    /// <>
    #[display("<>")]
    UnEq,

    /// !=
    #[display("!=")]
    NotEq,

    /// *
    #[display("*")]
    Asterisk,

    /// +
    #[display("+")]
    Plus,

    /// -
    #[display("-")]
    Sub,

    /// ;
    #[display(";")]
    Semicolon,

    /// /
    #[display("/")]
    Slash,

    /// @
    #[display("@")]
    At,

    /// ||
    #[display("||")]
    JoinStrings,

    Ident(EcoString),

    String(EcoString),

    #[display("/*{_0}*/")]
    Comment(EcoString),

    Unkown(char),

    Whitespace(EcoString),

    Integer(EcoString),
}

#[derive(derive_more::Debug, Clone, PartialEq, Hash)]
pub struct Token {
    pub kind: TokenKind,
    #[debug(skip)]
    pub start: Loc,
    #[debug(skip)]
    pub end: Loc,
}

impl Token {
    pub fn get_span(&self) -> Span {
        Span {
            start: self.start,
            end: self.end,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}

pub trait GetKind {
    fn get_kind(&self) -> Option<&TokenKind>;
}

impl GetKind for Option<Token> {
    fn get_kind(&self) -> Option<&TokenKind> {
        self.as_ref().map(|tok| &tok.kind)
    }
}

impl GetKind for Option<&Token> {
    fn get_kind(&self) -> Option<&TokenKind> {
        self.as_ref().map(|tok| &tok.kind)
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
        TokenKind::Ident(s.into())
    }

    pub fn string(s: &str) -> Self {
        TokenKind::String(s.into())
    }
}

pub fn ident_map(ident: EcoString) -> TokenKind {
    TokenKind::ident_map(ident)
}
