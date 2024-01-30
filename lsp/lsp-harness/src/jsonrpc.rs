//! A minimal implementation of JSON rpc for talking to language servers.
//!
//! This is not a production-grade implementation.

use anyhow::{bail, Context, Result};
use log::debug;
use lsp_server::ResponseError;
use lsp_types::{
    notification::{
        DidChangeTextDocument, DidOpenTextDocument, Exit, Initialized,
        Notification as LspNotification,
    },
    request::{GotoDefinition, Initialize, Request as LspRequest, Shutdown},
    ClientCapabilities, DidChangeTextDocumentParams, DidOpenTextDocumentParams,
    GotoDefinitionParams, GotoDefinitionResponse, InitializeParams, InitializedParams, Position,
    TextDocumentContentChangeEvent, TextDocumentIdentifier, TextDocumentPositionParams, Url,
    VersionedTextDocumentIdentifier, WorkDoneProgressParams,
};
use std::{
    io::{BufRead, BufReader, Read, Write},
    process::Stdio,
};

use serde::{Deserialize, Serialize};

pub struct Server {
    /// For sending messages to the language server.
    write: Box<dyn Write>,
    /// For reading messages from the language server.
    read: Box<dyn BufRead>,
    /// A source of unique ids for requests.
    id: u32,
    /// A buffer for notifications that have been received from the lsp but not
    /// yet delivered to the client.
    pending_notifications: Vec<Notification>,
}

/// A dynamically typed message from the LSP server.
#[derive(Debug)]
pub enum ServerMessage {
    Notification(Notification),
    Response(Response),
}

/// A statically typed request for sending to the LS.
#[derive(Serialize)]
struct SendRequest<T: LspRequest> {
    /// The string "2.0"
    jsonrpc: &'static str,
    /// The value of T::METHOD
    method: &'static str,
    params: T::Params,
    id: u32,
}

/// A statically type notification for sending to the LS.
#[derive(Serialize)]
struct SendNotification<T: LspNotification> {
    /// The string "2.0"
    jsonrpc: &'static str,
    /// The value of T::METHOD
    method: &'static str,
    params: T::Params,
}

/// An untyped notification from the LS.
#[derive(Serialize, Deserialize, Debug)]
pub struct Notification {
    /// The string "2.0", hopefully. (We aren't strict about checking it.)
    jsonrpc: String,
    pub method: String,
    /// The notification parameters. The structure of this should be determined
    /// by `method`, but it hasn't been checked yet.
    pub params: serde_json::Value,
}

/// An untyped request response from the LS.
#[derive(Serialize, Deserialize, Debug)]
pub struct Response {
    /// The string "2.0", hopefully. (We aren't strict about checking it.)
    jsonrpc: String,
    id: u32,
    /// The result. The structure of this should be determined by whatever
    /// request method this is a response to. But it hasn't been checked yet.
    #[serde(default)]
    result: serde_json::Value,
    /// Populated if the request generated an error.
    #[serde(default)]
    error: Option<ResponseError>,
}

impl Server {
    /// Launch a language server by running the given command.
    ///
    /// The command's stdin and stdout will be overridden to "piped" (because
    /// that's what LSes do).
    pub fn new(mut cmd: std::process::Command) -> Result<Server> {
        let lsp = cmd.stdin(Stdio::piped()).stdout(Stdio::piped()).spawn()?;

        let mut lsp = Server {
            write: Box::new(lsp.stdin.unwrap()),
            read: Box::new(BufReader::new(lsp.stdout.unwrap())),
            pending_notifications: Vec::new(),
            id: 0,
        };

        lsp.initialize()?;

        Ok(lsp)
    }

    /// Make the language server aware of a file.
    pub fn send_file(&mut self, uri: Url, contents: &str) -> Result<()> {
        self.send_notification::<DidOpenTextDocument>(DidOpenTextDocumentParams {
            text_document: lsp_types::TextDocumentItem {
                uri,
                language_id: "nickel".to_owned(),
                version: 1,
                text: contents.to_owned(),
            },
        })
    }

    /// Replace the contents of a file with new contents.
    pub fn replace_file(&mut self, uri: Url, version: i32, contents: &str) -> Result<()> {
        self.send_notification::<DidChangeTextDocument>(DidChangeTextDocumentParams {
            content_changes: vec![TextDocumentContentChangeEvent {
                range: None,
                range_length: None,
                text: contents.to_owned(),
            }],
            text_document: VersionedTextDocumentIdentifier { uri, version },
        })
    }

    /// Send a GotoDefinition request to the language server.
    pub fn goto_def(&mut self, uri: Url, pos: Position) -> Result<Option<GotoDefinitionResponse>> {
        self.send_request::<GotoDefinition>(GotoDefinitionParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri },
                position: pos,
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        })
    }

    /// Shut down the language server gracefully.
    pub fn shutdown(&mut self) -> Result<()> {
        self.send_request::<Shutdown>(())?;
        self.send_notification::<Exit>(())
    }

    fn initialize(&mut self) -> Result<()> {
        // `root_path` is deprecated, but we need ot initialize the struct
        // somehow. There is no `Default` implementation for `InitilizeParams`
        // in versions of `lsp-types` compatible with `codespan-lsp`
        #[allow(deprecated)]
        self.send_request::<Initialize>(InitializeParams {
            process_id: None,
            root_path: None,
            root_uri: None,
            initialization_options: None,
            capabilities: ClientCapabilities::default(),
            trace: None,
            workspace_folders: None,
            client_info: None,
            locale: None,
            work_done_progress_params: WorkDoneProgressParams::default(),
        })?;
        self.send_notification::<Initialized>(InitializedParams {})
    }

    /// Send a request to the language server and wait for the response.
    pub fn send_request<T: LspRequest>(&mut self, params: T::Params) -> Result<T::Result> {
        self.id += 1;
        let req = SendRequest::<T> {
            jsonrpc: "2.0",
            method: T::METHOD,
            params,
            id: self.id,
        };
        self.send(&req)?;
        let resp = self.recv_response()?;
        if resp.id != self.id {
            // In general, LSP responses can come out of order. But because we always
            // wait for a response after sending a request, there's only one outstanding
            // response.
            bail!("expected id {}, got {}", self.id, resp.id);
        }
        if let Some(err) = resp.error {
            bail!(err.message);
        }
        Ok(serde_json::value::from_value(resp.result)?)
    }

    /// Send a notification to the language server.
    pub fn send_notification<T: LspNotification>(&mut self, params: T::Params) -> Result<()> {
        let req = SendNotification::<T> {
            jsonrpc: "2.0",
            method: T::METHOD,
            params,
        };
        self.send(&req)
    }

    /// Return all notifications sent by the server.
    pub fn pending_notifications(&mut self) -> Vec<Notification> {
        std::mem::take(&mut self.pending_notifications)
    }

    /// Send something serializable to the language server.
    ///
    /// You probably want [`ServerState::send_request`] or
    /// [`ServerState::send_notification`] instead, because they make sure to
    /// send well-formed JSON RPC.
    fn send<S: Serialize>(&mut self, msg: &S) -> Result<()> {
        let msg = serde_json::to_string(msg)?;
        debug!("sending {msg}");

        self.write
            .write_all(format!("Content-Length: {}\r\n\r\n", msg.len()).as_bytes())?;
        self.write.write_all(msg.as_bytes())?;
        self.write.flush()?;

        Ok(())
    }

    /// Receive messages from the language server until we get a request's response.
    ///
    /// Any notifications we encounter will get stashed.
    fn recv_response(&mut self) -> Result<Response> {
        loop {
            match self.recv()? {
                ServerMessage::Notification(note) => self.pending_notifications.push(note),
                ServerMessage::Response(resp) => {
                    return Ok(resp);
                }
            }
        }
    }

    /// Receive a single JSON RPC message from the server.
    pub(crate) fn recv(&mut self) -> Result<ServerMessage> {
        let mut buf = String::new();
        let mut content_length: Option<usize> = None;

        loop {
            buf.clear();
            if self.read.read_line(&mut buf)? == 0 {
                break;
            }
            if buf == "\r\n" {
                // An empty line terminates the header.
                break;
            }

            if let Some(("Content-Length", value)) = buf.trim().split_once(": ") {
                content_length = Some(value.parse()?);
            }
        }

        let content_length = content_length.context("no Content-Length header")?;
        let mut content = vec![0; content_length];
        self.read.read_exact(&mut content)?;
        let text = String::from_utf8(content).context("invalid utf8 in message")?;
        debug!("server response: {text}");
        if let Ok(note) = serde_json::from_str(&text) {
            Ok(ServerMessage::Notification(note))
        } else {
            Ok(ServerMessage::Response(serde_json::from_str(&text)?))
        }
    }
}
