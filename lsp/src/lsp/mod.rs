pub mod diagnostics;
pub mod initialize;
pub mod text_document;

use ecow::EcoString;
use serde::{Deserialize, Serialize};

#[derive(Debug, Deserialize, Serialize)]
pub struct Request {
    #[serde(rename = "jsonrpc")]
    pub rpc: String,
    pub id: i32,
    pub method: String,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct Response {
    #[serde(rename = "jsonrpc")]
    pub rpc: String,
    pub id: Option<i32>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct Notification {
    #[serde(rename = "jsonrpc")]
    pub rpc: String,
    pub method: String,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct Range {
    pub start: Position,
    pub end: Position,
}

impl From<osql_parser::Span> for Range {
    fn from(value: osql_parser::Span) -> Self {
        Self {
            start: value.start.into(),
            end: value.end.into(),
        }
    }
}

#[derive(Debug, Deserialize, Serialize)]
pub struct Position {
    pub line: usize,
    pub character: usize,
}

impl From<osql_parser::Loc> for Position {
    fn from(value: osql_parser::Loc) -> Self {
        Self {
            line: value.line,
            character: value.col,
        }
    }
}

#[derive(Debug, Deserialize, Serialize)]
pub struct Location {
    pub uri: EcoString,
    pub range: Range,
}
