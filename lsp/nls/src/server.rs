use anyhow::Result;
use codespan::FileId;
use crossbeam::select;
use log::{debug, trace, warn};
use lsp_server::{Connection, ErrorCode, Message, Notification, RequestId, Response};
use lsp_types::{
    notification::Notification as _,
    notification::{DidChangeTextDocument, DidOpenTextDocument},
    request::{Request as RequestTrait, *},
    CodeActionParams, CompletionOptions, CompletionParams, DidChangeTextDocumentParams,
    DidOpenTextDocumentParams, DocumentFormattingParams, DocumentSymbolParams,
    ExecuteCommandParams, GotoDefinitionParams, HoverOptions, HoverParams, HoverProviderCapability,
    OneOf, PublishDiagnosticsParams, ReferenceParams, RenameParams, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions, Url,
    WorkDoneProgressOptions,
};

use crate::{
    actions,
    background::BackgroundJobs,
    command,
    requests::{completion, formatting, goto, hover, rename, symbols},
    trace::Trace,
    world::World,
};

pub const COMPLETIONS_TRIGGERS: &[&str] = &[".", "\"", "/"];

#[derive(Copy, Clone, PartialEq, Eq)]
enum Shutdown {
    Shutdown,
    Continue,
}

pub struct Server {
    pub connection: Connection,
    pub world: World,
    pub background_jobs: BackgroundJobs,
}

impl Server {
    pub fn capabilities() -> ServerCapabilities {
        ServerCapabilities {
            text_document_sync: Some(TextDocumentSyncCapability::Options(
                TextDocumentSyncOptions {
                    open_close: Some(true),
                    change: Some(TextDocumentSyncKind::FULL),
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
                trigger_characters: Some(
                    COMPLETIONS_TRIGGERS.iter().map(|s| s.to_string()).collect(),
                ),
                ..Default::default()
            }),
            document_symbol_provider: Some(OneOf::Left(true)),
            document_formatting_provider: Some(OneOf::Left(true)),
            code_action_provider: Some(lsp_types::CodeActionProviderCapability::Simple(true)),
            execute_command_provider: Some(lsp_types::ExecuteCommandOptions {
                commands: vec!["eval".to_owned()],
                ..Default::default()
            }),
            rename_provider: Some(OneOf::Left(true)),
            ..ServerCapabilities::default()
        }
    }

    pub fn new(connection: Connection) -> Server {
        Server {
            connection,
            world: World::default(),
            background_jobs: BackgroundJobs::new(),
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
        loop {
            select! {
                recv(self.connection.receiver) -> msg => {
                    // Failure here means the connection was closed, so exit quietly.
                    let Ok(msg) = msg else {
                        break;
                    };
                    if self.handle_message(msg)? == Shutdown::Shutdown {
                        break;
                    }
                }
                recv(self.background_jobs.receiver()) -> msg => {
                    // Failure here means our background thread panicked, and that's a bug.
                    let crate::background::Diagnostics { path, diagnostics } = msg.unwrap();
                    let uri = Url::from_file_path(path).unwrap();
                    let diagnostics = diagnostics.into_iter().map(From::from).collect();
                    self.publish_diagnostics(uri, diagnostics);
                }
            }
        }
        while let Ok(msg) = self.connection.receiver.recv() {
            if self.handle_message(msg)? == Shutdown::Shutdown {
                break;
            }
        }

        Ok(())
    }

    fn handle_message(&mut self, msg: Message) -> Result<Shutdown> {
        trace!("Message: {:#?}", msg);
        match msg {
            Message::Request(req) => {
                let id = req.id.clone();
                match self.connection.handle_shutdown(&req) {
                    Ok(true) => Ok(Shutdown::Shutdown),
                    Ok(false) => {
                        self.handle_request(req)?;
                        Ok(Shutdown::Continue)
                    }
                    Err(err) => {
                        // This only fails if a shutdown was
                        // requested in the first place, so it
                        // should definitely break out of the
                        // loop.
                        self.err(id, err);
                        Ok(Shutdown::Shutdown)
                    }
                }
            }
            Message::Notification(notification) => {
                let _ = self.handle_notification(notification);
                Ok(Shutdown::Continue)
            }
            Message::Response(_) => Ok(Shutdown::Continue),
        }
    }

    fn handle_notification(&mut self, notification: Notification) -> Result<()> {
        match notification.method.as_str() {
            DidOpenTextDocument::METHOD => {
                trace!("handle open notification");
                let params =
                    serde_json::from_value::<DidOpenTextDocumentParams>(notification.params)?;
                let uri = params.text_document.uri.clone();
                let contents = params.text_document.text.clone();
                crate::files::handle_open(self, params)?;
                self.background_jobs
                    .update_file(uri.clone(), contents, &self.world);
                self.background_jobs.eval_file(uri);
                Ok(())
            }
            DidChangeTextDocument::METHOD => {
                trace!("handle save notification");
                let params =
                    serde_json::from_value::<DidChangeTextDocumentParams>(notification.params)?;
                let uri = params.text_document.uri.clone();
                let contents = params.content_changes[0].text.clone();
                crate::files::handle_save(self, params)?;
                self.background_jobs
                    .update_file(uri.clone(), contents, &self.world);
                self.background_jobs.eval_file(uri);
                Ok(())
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
                debug!("handle goto definition");
                let params: GotoDefinitionParams = serde_json::from_value(req.params).unwrap();
                goto::handle_to_definition(params, req.id.clone(), self)
            }

            References::METHOD => {
                debug!("handle goto definition");
                let params: ReferenceParams = serde_json::from_value(req.params).unwrap();
                goto::handle_references(params, req.id.clone(), self)
            }

            Completion::METHOD => {
                debug!("handle completion");
                let params: CompletionParams = serde_json::from_value(req.params).unwrap();
                completion::handle_completion(params, req.id.clone(), self)
            }

            DocumentSymbolRequest::METHOD => {
                debug!("handle document symbols");
                let params: DocumentSymbolParams = serde_json::from_value(req.params).unwrap();
                symbols::handle_document_symbols(params, req.id.clone(), self)
            }

            Formatting::METHOD => {
                debug!("handle formatting");
                let params: DocumentFormattingParams = serde_json::from_value(req.params).unwrap();
                formatting::handle_format_document(params, req.id.clone(), self)
            }

            CodeActionRequest::METHOD => {
                debug!("code action");
                let params: CodeActionParams = serde_json::from_value(req.params).unwrap();
                actions::handle_code_action(params, req.id.clone(), self)
            }

            ExecuteCommand::METHOD => {
                debug!("command");
                let params: ExecuteCommandParams = serde_json::from_value(req.params).unwrap();
                command::handle_command(params, req.id.clone(), self)
            }

            Rename::METHOD => {
                debug!("rename");
                let params: RenameParams = serde_json::from_value(req.params).unwrap();
                rename::handle_rename(params, req.id.clone(), self)
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

    pub fn issue_diagnostics(
        &mut self,
        file_id: FileId,
        diagnostics: impl IntoIterator<Item = impl Into<lsp_types::Diagnostic>>,
    ) {
        let Some(uri) = self.world.file_uris.get(&file_id).cloned() else {
            warn!("tried to issue diagnostics for unknown file id {file_id:?}");
            return;
        };
        let diagnostics = diagnostics.into_iter().map(Into::into).collect();
        self.publish_diagnostics(uri, diagnostics)
    }

    pub fn publish_diagnostics(&mut self, uri: Url, diagnostics: Vec<lsp_types::Diagnostic>) {
        // Issue diagnostics even if they're empty (empty diagnostics are how the editor knows
        // that any previous errors were resolved).
        self.notify(lsp_server::Notification::new(
            "textDocument/publishDiagnostics".into(),
            PublishDiagnosticsParams {
                uri,
                diagnostics,
                version: None,
            },
        ));
    }
}
