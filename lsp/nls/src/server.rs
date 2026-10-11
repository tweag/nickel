use std::collections::HashMap;

use anyhow::Result;
use crossbeam::select;
use log::{debug, trace, warn};
use lsp_server::{
    Connection, ErrorCode, Message, Notification, RequestId, Response, ResponseError,
};
use lsp_types::{
    CodeActionParams, CompletionOptions, CompletionParams, DiagnosticOptions,
    DocumentDiagnosticParams, DocumentDiagnosticReport, DocumentDiagnosticReportResult,
    DocumentFormattingParams, DocumentSymbolParams, ExecuteCommandParams,
    FullDocumentDiagnosticReport, GotoDefinitionParams, HoverOptions, HoverParams,
    HoverProviderCapability, OneOf, PublishDiagnosticsParams, ReferenceParams,
    RelatedFullDocumentDiagnosticReport, RenameParams, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions, Url,
    WorkDoneProgressOptions,
    request::{Request as RequestTrait, *},
};
use nickel_lang_core::files::FileId;

use crate::{
    actions,
    background::{self, BackgroundJobs},
    command,
    config::LspConfig,
    requests::{completion, formatting, goto, hover, rename, symbols},
    task_queue::{DocumentSync, Task, TaskQueue},
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
    /// This is used for "pull diagnostics", where the editor can request
    /// diagnostics for a specific file and then get diagnostics as a response.
    ///
    /// We don't actually re-compute diagnostics on such a request; we just
    /// remember the last-pushed diagnostics and return them.
    pub last_diagnostics: HashMap<Url, Vec<lsp_types::Diagnostic>>,
    pub world: World,
    pub background_jobs: BackgroundJobs,
    pub task_queue: TaskQueue,
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
            diagnostic_provider: Some(lsp_types::DiagnosticServerCapabilities::Options(
                DiagnosticOptions {
                    inter_file_dependencies: true,
                    ..Default::default()
                },
            )),
            ..ServerCapabilities::default()
        }
    }

    pub fn new(connection: Connection, config: LspConfig) -> Server {
        Server {
            connection,
            last_diagnostics: HashMap::new(),
            background_jobs: BackgroundJobs::new(config.eval_config.clone()),
            world: World::new(config),
            task_queue: TaskQueue::new(),
        }
    }

    pub(crate) fn reply(&mut self, response: Response) {
        trace!("Sending response: {response:#?}");

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
        trace!("Sending notification: {notification:#?}");
        self.connection
            .sender
            .send(Message::Notification(notification))
            .unwrap();
    }

    fn err<E>(&mut self, id: RequestId, err: E)
    where
        E: std::fmt::Display,
    {
        warn!("{err}");
        self.reply(Response::new_err(
            id,
            ErrorCode::UnknownErrorCode as i32,
            err.to_string(),
        ));
    }

    pub fn run(&mut self) -> Result<()> {
        trace!("Running...");
        loop {
            let never = crossbeam::channel::never();
            let bg = self.background_jobs.receiver().unwrap_or(&never);

            if let Ok(msg) = self.connection.receiver.try_recv() {
                let result = self.task_queue.queue_message(msg);
                if let Err(err) = result {
                    warn!("Failed to add a message to the queue: {}", err);
                }
            } else if let Ok(diagnostics) = bg.try_recv() {
                self.publish_background_diagnostics(diagnostics);
            } else if let Some(task) = self.task_queue.next_task() {
                if self.handle_task(task)? == Shutdown::Shutdown {
                    break;
                }
            } else {
                select! {
                    recv(self.connection.receiver) -> msg => {
                        // Failure here means the connection was closed, so exit quietly.
                        let Ok(msg) = msg else {
                            break;
                        };

                        let result = self.task_queue.queue_message(msg);
                        if let Err(err) = result {
                            warn!("Failed to add a message to the queue: {}", err);
                        };
                    }
                    recv(bg) -> msg => {
                        // Failure here means our background thread panicked, and that's a bug.
                        self.publish_background_diagnostics(msg.unwrap());
                    }
                }
            }
        }

        Ok(())
    }

    fn publish_background_diagnostics(&mut self, diagnostics: crate::background::Diagnostics) {
        let background::Diagnostics { path, diagnostics } = diagnostics;
        let uri = Url::from_file_path(path).unwrap();
        let diagnostics = diagnostics.into_iter().map(From::from).collect();
        self.publish_diagnostics(uri, diagnostics);
    }

    fn handle_task(&mut self, task: Task) -> Result<Shutdown> {
        match task {
            Task::HandleRequest(req) => {
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
            Task::HandleDocumentSync(req) => {
                if let Err(err) = self.handle_document_sync(req) {
                    warn!("Document synchronization failed: {}", err);
                };
                Ok(Shutdown::Continue)
            }

            Task::Diagnostics(req) => {
                if let Err(err) = crate::files::run_diagnostics_on_file(self, req) {
                    warn!("Unable to publish diagnostics for a file: {}", err);
                }
                Ok(Shutdown::Continue)
            }
            Task::CancelRequest(id) => {
                self.cancel_request(id);
                Ok(Shutdown::Continue)
            }
        }
    }

    fn handle_document_sync(&mut self, task: DocumentSync) -> Result<()> {
        match task {
            DocumentSync::Open(params) => {
                trace!("handle open notification");
                let uri = params.text_document.uri.clone();
                let invalid = crate::files::handle_open(self, params)?;
                self.task_queue.add_diagnostics_task(uri);
                for uri in invalid {
                    self.task_queue.add_diagnostics_task(uri);
                }
                Ok(())
            }
            DocumentSync::Close(params) => {
                trace!("handle close notification");
                let uri = params.text_document.uri.clone();
                let (new_file, invalid) = crate::files::handle_close(self, params)?;
                if new_file.is_some() {
                    self.task_queue.add_diagnostics_task(uri);
                }
                for uri in invalid {
                    self.task_queue.add_diagnostics_task(uri);
                }
                Ok(())
            }
            DocumentSync::Change(params) => {
                trace!("handle save notification");
                let uri = params.text_document.uri.clone();
                let invalid = crate::files::handle_save(self, params)?;
                self.task_queue.add_diagnostics_task(uri);
                for uri in invalid {
                    self.task_queue.add_diagnostics_task(uri);
                }
                Ok(())
            }
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
                goto::handle_goto_definition(params, req.id.clone(), self)
            }

            References::METHOD => {
                debug!("handle goto references");
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

            DocumentDiagnosticRequest::METHOD => {
                debug!("diagnostic request");
                let params: DocumentDiagnosticParams = serde_json::from_value(req.params).unwrap();
                self.resend_diagnostics(req.id.clone(), params.text_document.uri)
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

    /// Sends a response to the client acknowledging that a request has been cancelled.
    ///
    /// It's assumed that the cancellation was requested by the client and not initiated by the
    /// server, and the response is sent accordingly.
    fn cancel_request(&mut self, id: RequestId) {
        debug!("Cancelling request {}", id);
        self.reply(Response {
            id,
            result: None,
            error: Some(ResponseError {
                // If we need to support server cancelled requests at some point this error code is
                // the only thing that really needs to change. `ErrorCode::ServerCanceled` would be
                // used instead.
                code: ErrorCode::RequestCanceled as i32,
                message: "Request cancelled".into(),
                data: None,
            }),
        });
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
        debug!("Issuing diagnostics for {} with ID {:?}", uri, file_id);
        let diagnostics = diagnostics.into_iter().map(Into::into).collect();
        self.publish_diagnostics(uri, diagnostics)
    }

    fn resend_diagnostics(&mut self, id: RequestId, uri: Url) -> Result<(), ResponseError> {
        let empty = Vec::new();
        let diags = self.last_diagnostics.get(&uri).unwrap_or(&empty);
        let response = Response::new_ok(
            id,
            DocumentDiagnosticReportResult::Report(DocumentDiagnosticReport::Full(
                RelatedFullDocumentDiagnosticReport {
                    related_documents: None,
                    full_document_diagnostic_report: FullDocumentDiagnosticReport {
                        result_id: None,
                        items: diags.clone(),
                    },
                },
            )),
        );
        self.reply(response);
        Ok(())
    }

    pub fn publish_diagnostics(&mut self, uri: Url, diagnostics: Vec<lsp_types::Diagnostic>) {
        self.last_diagnostics
            .insert(uri.clone(), diagnostics.clone());
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
