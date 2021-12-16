use std::collections::HashMap;

use anyhow::Result;
use codespan::FileId;
use log::{debug, trace, warn};
use lsp_server::{
    Connection, ErrorCode, Message, Notification, RequestId, Response, ResponseError,
};
use lsp_types::{
    notification::Notification as _,
    notification::{DidChangeTextDocument, DidOpenTextDocument},
    request::{Request as RequestTrait, *},
    CompletionOptions, CompletionParams, DidChangeTextDocumentParams, DidOpenTextDocumentParams,
    DocumentSymbolParams, GotoDefinitionParams, HoverOptions, HoverParams, HoverProviderCapability,
    OneOf, ReferenceParams, ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind,
    TextDocumentSyncOptions, WorkDoneProgressOptions,
};

use nickel::cache::Cache;
use nickel::typecheck::{linearization::Completed, Environment};

use crate::{
    requests::{completion, goto, hover, symbols},
    trace::Trace,
};

pub struct Server {
    pub connection: Connection,
    pub cache: Cache,
    pub lin_cache: HashMap<FileId, Completed>,
    pub global_env: Environment,
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
            references_provider: Some(OneOf::Left(true)),
            completion_provider: Some(CompletionOptions {
                trigger_characters: Some(vec![]),
                ..Default::default()
            }),
            document_symbol_provider: Some(OneOf::Left(true)),
            ..ServerCapabilities::default()
        }
    }

    pub fn new(connection: Connection) -> Server {
        let mut cache = Cache::new();
        cache.load_stdlib().unwrap();
        let global_env = cache.mk_global_types().unwrap();
        let lin_cache = HashMap::new();
        Server {
            connection,
            cache,
            lin_cache,
            global_env,
        }
    }

    pub(crate) fn reply(&mut self, response: Response) {
        trace!("Sending response: {:#?}", response);

        if response.error.is_some() {
            Trace::error_reply(response.id.clone());
        } else {
            Trace::reply(response.id.clone());
        }

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
                        Ok(false) => self.handle_request(req)?,
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

    fn handle_notification(&mut self, notification: Notification) -> Result<()> {
        match notification.method.as_str() {
            DidOpenTextDocument::METHOD => {
                trace!("handle open notification");
                crate::files::handle_open(
                    self,
                    serde_json::from_value::<DidOpenTextDocumentParams>(notification.params)?,
                )
            }
            DidChangeTextDocument::METHOD => {
                trace!("handle save notification");
                crate::files::handle_save(
                    self,
                    serde_json::from_value::<DidChangeTextDocumentParams>(notification.params)?,
                )
            }
            _ => Ok(()),
        }
    }

    fn handle_request(&mut self, req: lsp_server::Request) -> Result<()> {
        Trace::receive(req.id.clone(), req.method.clone());

        let res = match req.method.as_str() {
            HoverRequest::METHOD => {
                let params: HoverParams = serde_json::from_value(req.params).unwrap();
                hover::handle(params, req.id.clone(), self)
            }

            GotoDefinition::METHOD => {
                debug!("handle goto defnition");
                let params: GotoDefinitionParams = serde_json::from_value(req.params).unwrap();
                goto::handle_to_definition(params, req.id.clone(), self)
            }

            References::METHOD => {
                debug!("handle goto defnition");
                let params: ReferenceParams = serde_json::from_value(req.params).unwrap();
                goto::handle_to_usages(params, req.id.clone(), self)
            }

            Completion::METHOD => {
                debug!("handle completion");
                let params: CompletionParams = serde_json::from_value(req.params).unwrap();
                completion::handle_completion(params, req.id.clone(), self)
            }

            DocumentSymbolRequest::METHOD => {
                debug!("handle completion");
                let params: DocumentSymbolParams = serde_json::from_value(req.params).unwrap();
                symbols::handle_document_symbols(params, req.id.clone(), self)
            }

            _ => Ok(()),
        };

        if let Err(error) = res {
            self.reply(Response {
                id: req.id,
                result: None,
                error: Some(error),
            });
        }
        Ok(())
    }

    pub fn lin_cache_get(&self, file_id: &FileId) -> Result<&Completed, ResponseError> {
        self.lin_cache.get(file_id).ok_or_else(|| ResponseError {
            data: None,
            message: "File has not yet been parsed or cached.".to_owned(),
            code: ErrorCode::ParseError as i32,
        })
    }
}
