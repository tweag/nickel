use std::{collections::HashMap, ops, os::unix::prelude::FileExt};

use anyhow::Result;
use codespan::{ByteIndex, FileId, Files};
use log::{debug, trace, warn};
use lsp_server::{Connection, ErrorCode, Message, Notification, RequestId, Response};
use lsp_types::{
    notification::{self, DidChangeTextDocument, DidOpenTextDocument},
    notification::{Notification as _, *},
    request::{Request as RequestTrait, *},
    DeclarationCapability, DidChangeTextDocumentParams, DidOpenTextDocumentParams,
    DidSaveTextDocumentParams, GotoDefinitionParams, Hover, HoverContents, HoverOptions,
    HoverParams, HoverProviderCapability, MarkedString, OneOf, Position, Range, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions, WorkDoneProgress,
    WorkDoneProgressOptions,
};
use serde::Deserialize;

use nickel::typecheck::linearization::Completed;
use nickel::{
    cache::{Cache, ImportResolver},
    environment::Environment,
    eval::Thunk,
    identifier::Ident,
    position::{self, TermPos},
};

use crate::{
    diagnostic::LocationCompat,
    requests::{goto, hover},
};

pub struct Server {
    pub connection: Connection,
    pub cache: Cache,
    pub lin_cache: HashMap<FileId, Completed>,
    pub global_env: Environment<Ident, Thunk>,
}

impl Server {
    pub fn capabilities() -> ServerCapabilities {
        ServerCapabilities {
            text_document_sync: Some(TextDocumentSyncCapability::Options(
                TextDocumentSyncOptions {
                    open_close: Some(true),
                    change: Some(TextDocumentSyncKind::Full),
                    ..TextDocumentSyncOptions::default()
                },
            )),
            hover_provider: Some(HoverProviderCapability::Options(HoverOptions {
                work_done_progress_options: WorkDoneProgressOptions {
                    work_done_progress: Some(false),
                },
            })),
            definition_provider: Some(OneOf::Left(true)),
            ..ServerCapabilities::default()
        }
    }

    pub fn new(connection: Connection) -> Server {
        let mut cache = Cache::new();
        cache.prepare_stdlib().unwrap();
        let lin_cache = HashMap::new();
        let global_env = cache.mk_global_env().unwrap();
        Server {
            connection,
            cache,
            lin_cache,
            global_env,
        }
    }

    pub(crate) fn reply(&mut self, response: Response) {
        trace!("Sending response: {:#?}", response);
        self.connection
            .sender
            .send(Message::Response(response))
            .unwrap();
    }
    pub(crate) fn notify(&mut self, notification: Notification) {
        trace!("Sending notification: {:#?}", notification);
        self.connection
            .sender
            .send(Message::Notification(notification))
            .unwrap();
    }

    fn err<E>(&mut self, id: RequestId, err: E)
    where
        E: std::fmt::Display,
    {
        warn!("{}", err);
        self.reply(Response::new_err(
            id,
            ErrorCode::UnknownErrorCode as i32,
            err.to_string(),
        ));
    }

    pub fn run(&mut self) -> Result<()> {
        trace!("Running...");
        while let Ok(msg) = self.connection.receiver.recv() {
            trace!("Message: {:#?}", msg);
            match msg {
                Message::Request(req) => {
                    let id = req.id.clone();
                    match self.connection.handle_shutdown(&req) {
                        Ok(true) => break,
                        Ok(false) => self.handle_request(req),
                        Err(err) => {
                            // This only fails if a shutdown was
                            // requested in the first place, so it
                            // should definitely break out of the
                            // loop.
                            self.err(id, err);
                            break;
                        }
                    }
                }
                Message::Notification(notification) => {
                    let _ = self.handle_notification(notification);
                }
                Message::Response(_) => (),
            }
        }

        Ok(())
    }

    fn handle_notification(&mut self, notification: Notification) {
        match notification.method.as_str() {
            DidOpenTextDocument::METHOD => {
                trace!("handle open notification");
                crate::files::handle_open(
                    self,
                    serde_json::from_value::<DidOpenTextDocumentParams>(notification.params)
                        .unwrap(),
                )
                .unwrap();
            }
            DidChangeTextDocument::METHOD => {
                trace!("handle save notification");
                crate::files::handle_save(
                    self,
                    serde_json::from_value::<DidChangeTextDocumentParams>(notification.params)
                        .unwrap(),
                )
                .unwrap();
            }
            _ => {}
        }
    }

    fn handle_request(&mut self, req: lsp_server::Request) {
        match req.method.as_str() {
            HoverRequest::METHOD => {
                let params: HoverParams = serde_json::from_value(req.params).unwrap();
                hover::handle(params, req.id, self)
            }

            GotoDefinition::METHOD => {
                debug!("handle goto defnition");
                let params: GotoDefinitionParams = serde_json::from_value(req.params).unwrap();
                goto::handle_to_definition(params, req.id, self)
            }

            _ => {}
        }
    }
}

pub fn positon_to_byte_index(
    position: Position,
    file_id: FileId,
    files: &Files<String>,
) -> ByteIndex {
    let mut index = 0;
    let mut lines = position.line;
    for (idx, byte) in files.source(file_id).bytes().enumerate() {
        if byte == '\n' as u8 {
            lines -= 1
        }
        if lines == 0 {
            index = idx as u32;
            break;
        }
    }
    index += position.character;
    return ByteIndex(index);
}
