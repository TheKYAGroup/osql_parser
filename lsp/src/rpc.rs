use std::{num::ParseIntError, str::Utf8Error};

use ecow::EcoString;
use serde::{Deserialize, Serialize};

pub fn encode_message<T: Serialize>(msg: &T) -> String {
    let content = serde_json::to_string(msg).expect("Failed to serialize message");

    format!("Content-Length: {}\r\n\r\n{}", content.len(), content)
}

#[derive(Debug)]
pub enum DecodeErrorKind {
    HeaderNotFound,
    ContentLengthToString(Utf8Error),
    ParseContentLength(ParseIntError),
    MessageTooShort,
    JsonDeserialize(serde_json::Error),
}

#[derive(Debug, Deserialize)]
struct BaseMessage {
    method: EcoString,
}

pub fn decode_message(msg: &[u8]) -> Result<(EcoString, &[u8]), DecodeErrorKind> {
    let (header, content) = msg
        .split_on_slice(b"\r\n\r\n")
        .ok_or(DecodeErrorKind::HeaderNotFound)?;

    let content_length_bytes = &header["Content-Length: ".len()..];

    let content_length: usize = str::from_utf8(content_length_bytes)
        .map_err(DecodeErrorKind::ContentLengthToString)?
        .parse()
        .map_err(DecodeErrorKind::ParseContentLength)?;

    if content_length > content.len() {
        return Err(DecodeErrorKind::MessageTooShort);
    }

    let content = &content[..content_length];

    let base_message: BaseMessage =
        serde_json::from_slice(content).map_err(DecodeErrorKind::JsonDeserialize)?;

    Ok((base_message.method, content))
}

trait SplitOnSlice<'a, 'b> {
    fn split_on_slice(&'a self, pred: &'b Self) -> Option<(&'a Self, &'a Self)>;
}

impl<'a, 'b, T: 'a + 'b> SplitOnSlice<'a, 'b> for [T]
where
    &'a [T]: PartialEq<&'b [T]>,
    'b: 'a,
{
    fn split_on_slice(&'a self, pred: &'b Self) -> Option<(&'a Self, &'a Self)> {
        let Some((pos, _)) = self
            .windows(pred.len())
            .enumerate()
            .find(|(_, window)| *window == pred)
        else {
            return None;
        };

        Some((&self[..pos], &self[pos + pred.len()..]))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde::Serialize;

    #[derive(Debug, Serialize)]
    struct EncodingExample {
        testing: bool,
    }

    #[test]
    fn test_encoding() {
        let expected = "Content-Length: 16\r\n\r\n{\"testing\":true}";

        let actual = encode_message(&EncodingExample { testing: true });

        assert_eq!(expected, actual)
    }

    #[test]
    fn test_deconde() {
        let incoming_message = b"Content-Length: 15\r\n\r\n{\"method\":\"hi\"}";

        let (method, content) = decode_message(incoming_message).unwrap();

        assert_eq!(content.len(), 15);
        assert_eq!(method, "hi");
    }
}
