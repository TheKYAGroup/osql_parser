use ecow::EcoString;
use serde::{Deserialize, Serialize};

use crate::lsp::Location;

#[derive(Debug, Deserialize, Serialize)]
pub struct PublishDiagnosticsParams {
    pub uri: EcoString,

    pub diagnostics: Vec<Diagnostic>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct Diagnostic {
    pub range: super::Range,

    pub source: Option<EcoString>,

    pub message: EcoString,

    #[serde(rename = "relatedInformation")]
    pub related_information: Vec<DiagnosticRelatedInformation>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct DiagnosticRelatedInformation {
    /**
     * The location of this related diagnostic information.
     */
    location: Location,

    /**
     * The message of this related diagnostic information.
     */
    message: EcoString,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct PublishDiagnosticsNotification {
    #[serde(flatten)]
    pub notification: super::Notification,

    pub params: PublishDiagnosticsParams,
}

pub fn new_diagnostics_notification(
    uri: EcoString,
    diagnostics: Vec<Diagnostic>,
) -> PublishDiagnosticsNotification {
    return PublishDiagnosticsNotification {
        notification: super::Notification {
            rpc: "2.0".to_string(),
            method: "textDocument/publishDiagnostics".to_string(),
        },
        params: PublishDiagnosticsParams { uri, diagnostics },
    };
}
