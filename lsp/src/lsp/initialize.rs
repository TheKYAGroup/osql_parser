use serde::{Deserialize, Serialize};

use crate::lsp::Response;

#[derive(Debug, Serialize, Deserialize)]
pub struct InitializeRequest {
    #[serde(flatten)]
    pub request: super::Request,
    pub params: Params,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Params {
    #[serde(rename = "clientInfo")]
    pub client_info: Option<ClientInfo>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ClientInfo {
    pub name: String,
    pub version: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct InitializeResponse {
    #[serde(flatten)]
    pub response: super::Response,

    pub result: InitializeResult,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct InitializeResult {
    capabilities: ServerCapabilities,
    #[serde(rename = "serverInfo")]
    server_info: ServerInfo,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ServerCapabilities {
    #[serde(rename = "textDocumentSync")]
    text_document_sync: i32,
    #[serde(rename = "definitionProvider")]
    definition_provider: bool,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ServerInfo {
    name: String,
    version: String,
}

pub fn new_initialize_response(id: i32) -> InitializeResponse {
    return InitializeResponse {
        response: Response {
            rpc: "2.0".to_string(),
            id: Some(id),
        },
        result: InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: 1,
                definition_provider: true,
            },
            server_info: ServerInfo {
                name: "osqllsp".to_string(),
                version: "0.0.1".to_string(),
            },
        },
    };
}
