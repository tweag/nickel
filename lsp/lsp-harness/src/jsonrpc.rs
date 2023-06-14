//! A minimal implementation of JSON rpc for talking to language servers.
//!
//! This is not a production-grade implementation.

use anyhow::{bail, Context, Result};
use log::debug;
use lsp_types::{
    notification::{DidOpenTextDocument, Exit, Initialized, Notification as LspNotification},
    request::{GotoDefinition, Initialize, Request as LspRequest, Shutdown},
    DidOpenTextDocumentParams, GotoDefinitionParams, GotoDefinitionResponse, InitializeParams,
    InitializedParams, Position, TextDocumentIdentifier, TextDocumentPositionParams, Url,
};
use std::{
    cell::RefCell,
    collections::{HashMap, VecDeque},
    io::{BufRead, BufReader, Read, Write},
    path::Path,
    process::Stdio,
};

use serde::{Deserialize, Serialize};

struct ServerState {
    tx: Box<dyn Write>,
    rx: Box<dyn BufRead>,
    /// A source of unique ids for requests.
    id: u32,
    /// A buffer for notifications that have been received from the lsp but not
    /// yet delivered to the client.
    pending_notifications: VecDeque<Notification>,
    /// Request reponses that have been received from the lsp but not yet delivered
    /// to the client.
    ///
    /// The main mechanism for retrieving a response is to call `result` on a
    /// [`ResponseHandle`], which will read messages from the server until it
    /// gets a response with the correct id. Since responses can arrive out of
    /// order, any other responses we encounter while looking for a specific
    /// one get stashed here. (And any notifications we encounter get stashed in
    /// `pending_notifications`.) If you don't call `result` on your `ResponseHandle`,
    /// the response will just live here forever.
    pending_responses: HashMap<u32, Response>,
}

pub struct Server {
    state: RefCell<ServerState>,
}

/// A dynamically typed message from the LSP server.
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
    method: String,
    /// The notification parameters. The structure of this should be determined
    /// by `method`, but it hasn't been checked yet.
    params: serde_json::Value,
}

/// An untyped request response from the LS.
#[derive(Serialize, Deserialize, Debug)]
pub struct Response {
    /// The string "2.0", hopefully. (We aren't strict about checking it.)
    jsonrpc: String,
    id: u32,
    /// The result. The structure of this should be determined by whatever
    /// request method this is a response to. But it hasn't been checked yet.
    result: serde_json::Value,
}

/// A statically typed handle for waiting for the response to a request.
pub struct ResponseHandle<'a, T: LspRequest> {
    id: u32,
    server: &'a Server,
    marker: std::marker::PhantomData<T>,
}

impl<'a, T: LspRequest> ResponseHandle<'a, T> {
    /// Wait for the response to a request, and validate the response according
    /// to the LSP spec.
    pub fn result(self) -> Result<T::Result> {
        let resp = self.server.state.borrow_mut().recv_until(self.id)?;
        Ok(serde_json::value::from_value(resp.result)?)
    }
}

impl Server {
    /// Launch a language server by running the given command.
    ///
    /// The command's stdin and stdout will be override to "piped" (because
    /// that's what LSes do).
    pub fn new(mut cmd: std::process::Command) -> Result<Server> {
        let lsp = cmd.stdin(Stdio::piped()).stdout(Stdio::piped()).spawn()?;

        let lsp = Server {
            state: RefCell::new(ServerState {
                tx: Box::new(lsp.stdin.unwrap()),
                rx: Box::new(BufReader::new(lsp.stdout.unwrap())),
                pending_notifications: VecDeque::new(),
                pending_responses: HashMap::new(),
                id: 0,
            }),
        };

        lsp.initialize()?;

        Ok(lsp)
    }

    /// Make the language server aware of a file.
    pub fn send_file(&self, uri: Url, contents: &str) -> Result<()> {
        self.send_notification::<DidOpenTextDocument>(DidOpenTextDocumentParams {
            text_document: lsp_types::TextDocumentItem {
                uri,
                language_id: "nickel".to_owned(),
                version: 1,
                text: contents.to_owned(),
            },
        })
    }

    /// Send a GotoDefinition request to the language server.
    pub fn goto_def(&self, uri: Url, pos: Position) -> Result<Option<GotoDefinitionResponse>> {
        let res = self
            .send_request::<GotoDefinition>(GotoDefinitionParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: pos,
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            })?
            .result()?;
        Ok(res)
    }

    /// Shut down the language server gracefully.
    pub fn shutdown(&self) -> Result<()> {
        self.send_request::<Shutdown>(())?.result()?;
        self.send_notification::<Exit>(())
    }

    fn initialize(&self) -> Result<()> {
        self.send_request::<Initialize>(InitializeParams::default())?
            .result()?;
        self.send_notification::<Initialized>(InitializedParams {})?;
        Ok(())
    }

    /// Send a request to the language server, returning a handle that can be used
    /// to wait for the response.
    pub fn send_request<T: LspRequest>(&self, params: T::Params) -> Result<ResponseHandle<'_, T>> {
        let id = self.state.borrow_mut().send_request::<T>(params)?;
        Ok(ResponseHandle {
            id,
            server: self,
            marker: std::marker::PhantomData,
        })
    }

    /// Send a notification to the language server.
    pub fn send_notification<T: LspNotification>(&self, params: T::Params) -> Result<()> {
        self.state.borrow_mut().send_notification::<T>(params)
    }

    /// Wait for a message from the language server (or return one that we've already
    /// got saved up).
    pub fn next_msg(&self) -> Result<ServerMessage> {
        self.state.borrow_mut().next_msg()
    }
}

impl ServerState {
    /// Send a request to the language server, returning its id.
    fn send_request<T: LspRequest>(&mut self, params: T::Params) -> Result<u32> {
        self.id += 1;
        let req = SendRequest::<T> {
            jsonrpc: "2.0",
            method: T::METHOD,
            params,
            id: self.id,
        };
        self.send(&req)?;
        Ok(self.id)
    }

    /// Send a notification to the language server.
    fn send_notification<T: LspNotification>(&mut self, params: T::Params) -> Result<()> {
        let req = SendNotification::<T> {
            jsonrpc: "2.0",
            method: T::METHOD,
            params,
        };
        self.send(&req)
    }

    /// Send something serializable to the language server.
    ///
    /// You probably want [`ServerState::send_request`] or
    /// [`ServerState::send_notification`] instead, because they make sure to
    /// send well-formed JSON RPC.
    fn send<S: Serialize>(&mut self, msg: &S) -> Result<()> {
        let msg = serde_json::to_string(msg)?;
        debug!("sending {msg}");

        self.tx
            .write_all(format!("Content-Length: {}\r\n\r\n", msg.len()).as_bytes())?;
        self.tx.write_all(msg.as_bytes())?;
        self.tx.flush()?;

        Ok(())
    }

    /// Receive messages from the language server until we see the requested id.
    ///
    /// All messages until the one we're after will get stashed.
    fn recv_until(&mut self, id: u32) -> Result<Response> {
        // Maybe we already received it.
        if let Some(resp) = self.pending_responses.remove(&id) {
            return Ok(resp);
        }

        loop {
            match self.recv()? {
                ServerMessage::Notification(note) => self.pending_notifications.push_back(note),
                ServerMessage::Response(resp) => {
                    if resp.id == id {
                        return Ok(resp);
                    } else {
                        let id = resp.id;
                        if let Some(old) = self.pending_responses.insert(id, resp) {
                            bail!(
                                "duplicate response for id {id}: {old:?} and {:?}",
                                self.pending_responses[&id]
                            );
                        }
                    }
                }
            }
        }
    }

    /// Receive a single JSON RPC message from the server.
    fn recv(&mut self) -> Result<ServerMessage> {
        let mut buf = String::new();
        let mut content_length: Option<usize> = None;

        loop {
            buf.clear();
            if self.rx.read_line(&mut buf)? == 0 {
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
        self.rx.read_exact(&mut content)?;
        let text = String::from_utf8(content).context("invalid utf8 in message")?;
        debug!("server response: {text}");
        if let Ok(note) = serde_json::from_str(&text) {
            Ok(ServerMessage::Notification(note))
        } else {
            Ok(ServerMessage::Response(serde_json::from_str(&text)?))
        }
    }

    fn next_msg(&mut self) -> Result<ServerMessage> {
        if let Some(note) = self.pending_notifications.pop_front() {
            Ok(ServerMessage::Notification(note))
        } else if let Some(resp_id) = self.pending_responses.keys().next().copied() {
            Ok(ServerMessage::Response(
                self.pending_responses.remove(&resp_id).unwrap(),
            ))
        } else {
            self.recv()
        }
    }
}
