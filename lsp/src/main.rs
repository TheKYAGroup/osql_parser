use std::{
    collections::HashMap,
    io::{BufRead, Read, Write, stdout},
    num::ParseIntError,
    str::Utf8Error,
    string::FromUtf8Error,
};

use derive_more::From;
use ecow::EcoString;
use flexi_logger::{FileSpec, LogSpecification};
use log::{error, info};
use osql_parser::oir::OirCompiler;
use serde::{Deserialize, Serialize, de::Visitor};

use crate::{
    lsp::diagnostics::{Diagnostic, DiagnosticRelatedInformation, new_diagnostics_notification},
    rpc::{decode_message, encode_message},
};

mod lsp;
mod rpc;

fn main() {
    flexi_logger::Logger::with(LogSpecification::debug())
        .log_to_file(FileSpec::try_from("/home/ethan/Code/Work/osql_parser/lsp/log.txt").unwrap())
        .start()
        .unwrap();

    info!("I started");

    let input = std::io::stdin();

    let output = std::io::stdout();

    let mut buf_read = std::io::BufReader::new(input);

    let mut state = State::new();

    while (!state.should_exit) {
        let message = match get_message(&mut buf_read) {
            Ok(Some(val)) => val,
            Ok(None) => continue,
            Err(err) => {
                error!("Failed to get message: {:?}", err);
                continue;
            }
        };

        let (method, contents) = match decode_message(&message) {
            Ok(val) => val,
            Err(err) => {
                error!("Failed to decode message: {err:?}",);
                continue;
            }
        };

        handle_message(method, contents, &mut state);
    }
}

#[derive(Debug)]
enum GetMessageError {
    LengthFromUtf8(Utf8Error),
    LengParse(ParseIntError),
    ReadError(std::io::Error),
    NoContentLength,
}

impl From<std::io::Error> for GetMessageError {
    fn from(value: std::io::Error) -> Self {
        Self::ReadError(value)
    }
}

fn get_message<T: Read>(
    buf_reader: &mut std::io::BufReader<T>,
) -> Result<Option<Vec<u8>>, GetMessageError> {
    let mut buf = Vec::with_capacity(20);

    loop {
        buf_reader.read_until(b'\r', &mut buf)?;
        if buf.len() == 0 || buf[buf.len() - 1] == b'\0' {
            continue;
        }
        buf.extend([0, 0, 0]);
        let buf_len = buf.len();
        buf_reader.read_exact(&mut buf[buf_len - 3..])?;

        if &buf[buf.len() - 4..] == b"\r\n\r\n" {
            break;
        }
    }

    let header = &buf[0..buf.len() - 4];

    info!("Processing header: {}", str::from_utf8(header).unwrap());

    let content_length_str = b"Content-Length: ";

    let (pos, _) = header
        .chunks(content_length_str.len())
        .enumerate()
        .find(|(_, val)| *val == content_length_str)
        .ok_or(GetMessageError::NoContentLength)?;

    let pos = pos + content_length_str.len();

    let content_length_bytes = &header[pos..];
    let content_length: usize = str::from_utf8(&content_length_bytes)
        .map(|val| {
            info!("Content_length: {val}");
            val
        })
        .map_err(GetMessageError::LengthFromUtf8)?
        .parse()
        .map_err(GetMessageError::LengParse)?;

    let mut content = vec![0; content_length];

    buf_reader.read_exact(&mut content[0..])?;

    buf.extend(content);

    return Ok(Some(buf));
}

fn handle_message(method: EcoString, contents: &[u8], state: &mut State) {
    info!("Recieved msg with method: {method}");

    match method.as_str() {
        "initialize" => {
            let request: lsp::initialize::InitializeRequest = match serde_json::from_slice(contents)
            {
                Err(err) => {
                    error!("Couldn't parse: {err:?}");
                    return;
                }
                Ok(val) => val,
            };

            info!(
                "Connected to: {} {}",
                request.params.client_info.as_ref().unwrap().name,
                request.params.client_info.as_ref().unwrap().version,
            );

            let msg = lsp::initialize::new_initialize_response(request.request.id);
            let reply = rpc::encode_message(&msg);
            let mut writer = stdout();
            if let Err(err) = write!(&mut writer, "{reply}") {
                error!("Failed to write to stdout: {err:?}");
            };

            if let Err(err) = writer.flush() {
                error!("Failed to flush stdio: {err:?}");
            };

            info!("Sent the reply")
        }
        "textDocument/didOpen" => {
            let request: lsp::text_document::DidOpenNotification =
                match serde_json::from_slice(contents) {
                    Err(err) => {
                        error!("Couldn't parse: {err:?}");
                        return;
                    }
                    Ok(val) => val,
                };

            info!(
                "Opened: {}, {}",
                request.params.text_document.uri, request.params.text_document.text
            );

            state.open_document(
                request.params.text_document.uri,
                request.params.text_document.text,
            );
        }
        "textDocument/didChange" => {
            let request: lsp::text_document::DidChangeNotification =
                match serde_json::from_slice(contents) {
                    Err(err) => {
                        error!("Couldn't parse: {err:?}");
                        return;
                    }
                    Ok(val) => val,
                };

            for change in request.params.content_changes {
                state.update_document(request.params.text_document.id.uri.clone(), change.text);
            }

            let uri = request.params.text_document.id.uri.clone();

            let program = osql_parser::parse_with_backtrace(
                state
                    .documents
                    .get(&uri)
                    .expect("Failed to get updated document")
                    .clone(),
            );

            let mut info: Vec<Diagnostic> = Vec::new();
            match program {
                Ok(prog) => {
                    let oir = OirCompiler::compile_program(&prog);

                    for oir in oir {
                        match oir {
                            Err(err) => {
                                info.push(Diagnostic {
                                    range: err.span.into(),
                                    source: Some(uri.clone()),
                                    message: format!("{}", err.inner).into(),
                                    related_information: Default::default(),
                                });
                            }
                            Ok(_) => {}
                        }
                    }
                }
                Err(err) => {
                    info.push(Diagnostic {
                        range: err.span.into(),
                        source: Some(uri.clone()),
                        message: err.inner.to_string().into(),
                        related_information: Default::default(),
                    });
                }
            }

            let resp = new_diagnostics_notification(uri, info);

            let reply = encode_message(&resp);

            let mut writer = stdout();
            if let Err(err) = write!(&mut writer, "{reply}") {
                error!("Failed to write to stdout: {err:?}");
            };

            if let Err(err) = writer.flush() {
                error!("Failed to flush stdio: {err:?}");
            };

            info!("Sent the reply")
        }
        "shutdown" => {
            state.should_exit = true;
        }
        _ => {}
    }
}

struct State {
    documents: HashMap<EcoString, EcoString>,
    should_exit: bool,
}

impl State {
    fn new() -> Self {
        Self {
            documents: Default::default(),
            should_exit: false,
        }
    }

    fn open_document(&mut self, uri: EcoString, text: EcoString) {
        self.documents.insert(uri, text);
    }

    fn update_document(&mut self, uri: EcoString, text: EcoString) {
        self.documents.insert(uri, text);
    }
}
