use ecow::EcoString;
use serde::{Deserialize, Serialize};

use crate::lsp::{Location, Notification, Position, Request, Response};

#[derive(Debug, Serialize, Deserialize)]
pub struct TextDocumentItem {
    pub uri: EcoString,
    #[serde(rename = "languageId")]
    pub language_id: String,
    pub version: i32,
    pub text: EcoString,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct TextDocumentIdentifier {
    pub uri: EcoString,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct VersionTextDocumentIdent {
    #[serde(flatten)]
    pub id: TextDocumentIdentifier,
    pub version: i32,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct DidOpenNotification {
    #[serde(flatten)]
    pub notification: Notification,

    pub params: DidOpenParams,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct DidOpenParams {
    #[serde(rename = "textDocument")]
    pub text_document: TextDocumentItem,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct DidChangeNotification {
    #[serde(flatten)]
    pub notification: Notification,

    pub params: DidChangeParams,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ContentChangeEvent {
    pub text: EcoString,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct DidChangeParams {
    #[serde(rename = "textDocument")]
    pub text_document: VersionTextDocumentIdent,

    #[serde(rename = "contentChanges")]
    pub content_changes: Vec<ContentChangeEvent>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct PositionRequest {
    #[serde(flatten)]
    pub request: Request,

    pub params: PositionParams,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct PositionParams {
    #[serde(rename = "textDocument")]
    pub text_document: TextDocumentIdentifier,

    pub position: Position,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct PositionResponse {
    #[serde(flatten)]
    pub response: Response,

    pub result: Option<Location>,
}
