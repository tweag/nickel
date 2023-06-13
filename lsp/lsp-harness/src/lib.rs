use anyhow::{bail, Context, Result};
use log::debug;
use lsp_types::{
    notification::{DidOpenTextDocument, Exit, Initialized},
    request::{GotoDefinition, Initialize, Shutdown},
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
    pending_notifications: VecDeque<JsonNotificationDyn>,
    /// Request reponses that have been received from the lsp but not yet delivered
    /// to the client.
    pending_responses: HashMap<u32, JsonResponseDyn>,
}

pub struct Server {
    state: RefCell<ServerState>,
}

pub enum ServerMessage {
    Notification(JsonNotificationDyn),
    Response(JsonResponseDyn),
}

#[derive(Serialize, Deserialize)]
struct JsonRequest<T: lsp_types::request::Request> {
    jsonrpc: &'static str,
    method: &'static str,
    params: T::Params,
    id: u32,
}

#[derive(Serialize, Deserialize)]
struct JsonNotification<T: lsp_types::notification::Notification> {
    jsonrpc: &'static str,
    method: &'static str,
    params: T::Params,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct JsonNotificationDyn {
    jsonrpc: String,
    method: String,
    params: serde_json::Value,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct JsonResponseDyn {
    jsonrpc: String,
    id: u32,
    result: serde_json::Value,
}

pub struct ResponseHandle<'a, T: lsp_types::request::Request> {
    id: u32,
    server: &'a Server,
    marker: std::marker::PhantomData<T>,
}

impl<'a, T: lsp_types::request::Request> ResponseHandle<'a, T> {
    pub fn result(self) -> Result<T::Result> {
        let resp = self.server.state.borrow_mut().recv_until(self.id)?;
        Ok(serde_json::value::from_value(resp.result)?)
    }
}

impl Server {
    pub fn new(binary: impl AsRef<Path>) -> Result<Server> {
        let lsp = std::process::Command::new(binary.as_ref())
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()?;

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

    pub fn send_request<T: lsp_types::request::Request>(
        &self,
        params: T::Params,
    ) -> Result<ResponseHandle<'_, T>> {
        let id = self.state.borrow_mut().send_request::<T>(params)?;
        Ok(ResponseHandle {
            id,
            server: self,
            marker: std::marker::PhantomData,
        })
    }

    pub fn send_notification<T: lsp_types::notification::Notification>(
        &self,
        params: T::Params,
    ) -> Result<()> {
        self.state.borrow_mut().send_notification::<T>(params)
    }

    pub fn next_msg(&self) -> Result<ServerMessage> {
        self.state.borrow_mut().next_msg()
    }
}

impl ServerState {
    pub fn send_request<T: lsp_types::request::Request>(
        &mut self,
        params: T::Params,
    ) -> Result<u32> {
        self.id += 1;
        let req = JsonRequest::<T> {
            jsonrpc: "2.0",
            method: T::METHOD,
            params,
            id: self.id,
        };
        self.send(&req)?;
        Ok(self.id)
    }

    pub fn send_notification<T: lsp_types::notification::Notification>(
        &mut self,
        params: T::Params,
    ) -> Result<()> {
        let req = JsonNotification::<T> {
            jsonrpc: "2.0",
            method: T::METHOD,
            params,
        };
        self.send(&req)
    }

    fn send<S: Serialize>(&mut self, msg: &S) -> Result<()> {
        let msg = serde_json::to_string(msg)?;
        debug!("sending {msg}");

        self.tx
            .write_all(format!("Content-Length: {}\r\n\r\n", msg.len()).as_bytes())?;
        self.tx.write_all(msg.as_bytes())?;
        self.tx.flush()?;

        Ok(())
    }

    fn recv_until(&mut self, id: u32) -> Result<JsonResponseDyn> {
        // Maybe we already received it.
        if let Some(resp) = self.pending_responses.remove(&id) {
            return Ok(resp);
        }

        loop {
            let resp = self.recv()?;
            if let Ok(not) = serde_json::from_str(&resp) {
                self.pending_notifications.push_back(not);
            } else {
                let resp = serde_json::from_str::<JsonResponseDyn>(&resp)?;
                let resp_id = resp.id;
                if resp.id == id {
                    return Ok(resp);
                } else if let Some(old) = self.pending_responses.insert(resp.id, resp) {
                    bail!(
                        "duplicate response for id {id}: {old:?} and {:?}",
                        self.pending_responses[&resp_id]
                    );
                }
            }
        }
    }

    fn recv(&mut self) -> Result<String> {
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
        Ok(text)
    }

    fn next_msg(&mut self) -> Result<ServerMessage> {
        if let Some(note) = self.pending_notifications.pop_front() {
            Ok(ServerMessage::Notification(note))
        } else if let Some(resp_id) = self.pending_responses.keys().next().copied() {
            Ok(ServerMessage::Response(
                self.pending_responses.remove(&resp_id).unwrap(),
            ))
        } else {
            let msg = self.recv()?;
            if let Ok(note) = serde_json::from_str(&msg) {
                Ok(ServerMessage::Notification(note))
            } else {
                Ok(ServerMessage::Response(serde_json::from_str(&msg)?))
            }
        }
    }
}
