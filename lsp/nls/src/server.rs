use std::{
    collections::{HashMap, HashSet},
    ffi::OsString,
};

use anyhow::Result;
use codespan::FileId;
use codespan_reporting::diagnostic::Diagnostic;
use log::{debug, trace, warn};
use lsp_server::{
    Connection, ErrorCode, Message, Notification, RequestId, Response, ResponseError,
};
use lsp_types::{
    notification::Notification as _,
    notification::{DidChangeTextDocument, DidOpenTextDocument},
    request::{Request as RequestTrait, *},
    CodeActionParams, CompletionOptions, CompletionParams, DidChangeTextDocumentParams,
    DidOpenTextDocumentParams, DocumentFormattingParams, DocumentSymbolParams,
    ExecuteCommandParams, GotoDefinitionParams, HoverOptions, HoverParams, HoverProviderCapability,
    OneOf, PublishDiagnosticsParams, ReferenceParams, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions, Url,
    WorkDoneProgressOptions,
};

use nickel_lang_core::{
    cache::{Cache, ErrorTolerance},
    position::{RawPos, RawSpan, TermPos},
    stdlib::StdlibModule,
    term::{record::FieldMetadata, RichTerm, Term, UnaryOp},
};
use nickel_lang_core::{stdlib, typecheck::Context};

use crate::{
    actions,
    analysis::{Analysis, AnalysisRegistry},
    cache::CacheExt,
    command,
    diagnostic::DiagnosticCompat,
    field_walker::{Def, FieldResolver},
    identifier::LocIdent,
    requests::{completion, formatting, goto, hover, symbols},
    trace::Trace,
};

pub const COMPLETIONS_TRIGGERS: &[&str] = &[".", "\"", "/"];

pub struct Server {
    pub connection: Connection,
    pub cache: Cache,
    /// In order to return diagnostics, we store the URL of each file we know about.
    pub file_uris: HashMap<FileId, Url>,
    pub analysis: AnalysisRegistry,
    pub initial_ctxt: Context,
    pub initial_term_env: crate::usage::Environment,

    /// A map associating imported files with failed imports. This allows us to
    /// invalidate the cached version of a file when one of its imports becomes available.
    ///
    /// The keys in this map are the filenames (just the basename; no directory) of the
    /// files that failed to import, and the values in this map are the file ids that tried
    /// to import it.
    pub failed_imports: HashMap<OsString, HashSet<FileId>>,
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
            ..ServerCapabilities::default()
        }
    }

    pub fn new(connection: Connection) -> Server {
        let mut cache = Cache::new(ErrorTolerance::Tolerant);

        if let Ok(nickel_path) = std::env::var("NICKEL_IMPORT_PATH") {
            cache.add_import_paths(nickel_path.split(':'));
        }

        // We don't recover from failing to load the stdlib for now.
        cache.load_stdlib().unwrap();
        let initial_ctxt = cache.mk_type_ctxt().unwrap();
        Server {
            connection,
            cache,
            file_uris: HashMap::new(),
            analysis: AnalysisRegistry::default(),
            initial_ctxt,
            initial_term_env: crate::usage::Environment::new(),
            failed_imports: HashMap::new(),
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

    fn linearize_stdlib(&mut self) -> Result<()> {
        self.cache.load_stdlib().unwrap();
        let cache = &mut self.cache;
        for module in stdlib::modules() {
            let file_id = cache.get_submodule_file_id(module).unwrap();
            cache
                .typecheck_with_analysis(
                    file_id,
                    &self.initial_ctxt,
                    &self.initial_term_env,
                    &mut self.analysis,
                )
                .unwrap();

            // Add the std module to the environment (but not `internals`, because those symbols
            // don't get their own namespace, and we don't want to use them for completion anyway).
            if module == StdlibModule::Std {
                // The term should always be populated by typecheck_with_analysis.
                let term = cache.terms().get(&file_id).unwrap();
                let name = module.name().into();
                let def = Def::Let {
                    ident: crate::identifier::LocIdent {
                        ident: name,
                        pos: TermPos::None,
                    },
                    value: term.term.clone(),
                    path: Vec::new(),
                };
                self.initial_term_env.insert(name, def);
            }
        }
        Ok(())
    }

    pub fn run(&mut self) -> Result<()> {
        trace!("Running...");
        self.linearize_stdlib()?;
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

    pub fn file_analysis(&self, file: FileId) -> Result<&Analysis, ResponseError> {
        self.analysis
            .analysis
            .get(&file)
            .ok_or_else(|| ResponseError {
                data: None,
                message: "File has not yet been parsed or cached.".to_owned(),
                code: ErrorCode::ParseError as i32,
            })
    }

    pub fn lookup_term_by_position(&self, pos: RawPos) -> Result<Option<&RichTerm>, ResponseError> {
        Ok(self
            .file_analysis(pos.src_id)?
            .position_lookup
            .get(pos.index))
    }

    pub fn lookup_ident_by_position(
        &self,
        pos: RawPos,
    ) -> Result<Option<crate::identifier::LocIdent>, ResponseError> {
        Ok(self
            .file_analysis(pos.src_id)?
            .position_lookup
            .get_ident(pos.index))
    }

    pub fn issue_diagnostics(&mut self, file_id: FileId, diagnostics: Vec<Diagnostic<FileId>>) {
        let Some(uri) = self.file_uris.get(&file_id).cloned() else {
            warn!("tried to issue diagnostics for unknown file id {file_id:?}");
            return;
        };

        let diagnostics: Vec<_> = diagnostics
            .into_iter()
            .flat_map(|d| lsp_types::Diagnostic::from_codespan(d, self.cache.files_mut()))
            .collect();

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

    /// Finds all the locations at which a term (or possibly an ident within a term) is "defined".
    ///
    /// "Ident within a term" applies when the term is a record or a pattern binding, so that we
    /// can refer to fields in a record, or specific idents in a pattern binding.
    ///
    /// The return value contains all the spans of all the definition locations. It's a span instead
    /// of a `LocIdent` because when `term` is an import, the definition location is the whole
    /// included file. In every other case, the definition location will be the span of a LocIdent.
    pub fn get_defs(&self, term: &RichTerm, ident: Option<LocIdent>) -> Vec<RawSpan> {
        // The inner function returning Option is just for ?-early-return convenience.
        fn inner(
            server: &Server,
            term: &RichTerm,
            ident: Option<LocIdent>,
        ) -> Option<Vec<RawSpan>> {
            let resolver = FieldResolver::new(server);
            let ret = match (term.as_ref(), ident) {
                (Term::Var(id), _) => {
                    let id = LocIdent::from(*id);
                    let def = server.analysis.get_def(&id)?;
                    let cousins = resolver.cousin_defs(def);
                    if cousins.is_empty() {
                        vec![def.ident().pos.unwrap()]
                    } else {
                        cousins
                            .into_iter()
                            .filter_map(|(loc, _)| loc.pos.into_opt())
                            .collect()
                    }
                }
                (Term::Op1(UnaryOp::StaticAccess(id), parent), _) => {
                    let parents = resolver.resolve_record(parent);
                    parents
                        .iter()
                        .filter_map(|parent| {
                            parent
                                .field_loc(id.ident())
                                .and_then(|def| def.pos.into_opt())
                        })
                        .collect()
                }
                (Term::LetPattern(_, pat, value, _), Some(hovered_id)) => {
                    let (mut path, _, _) = pat
                        .matches
                        .iter()
                        .flat_map(|m| m.to_flattened_bindings())
                        .find(|(_path, bound_id, _)| bound_id.ident() == hovered_id.ident)?;
                    path.reverse();
                    let (last, path) = path.split_last()?;
                    let path: Vec<_> = path.iter().map(|id| id.ident()).collect();
                    let parents = resolver.resolve_path(value, path.iter().copied());
                    parents
                        .iter()
                        .filter_map(|parent| {
                            parent
                                .field_loc(last.ident())
                                .and_then(|def| def.pos.into_opt())
                        })
                        .collect()
                }
                (Term::ResolvedImport(file), _) => {
                    let pos = server.cache.terms().get(file)?.term.pos;
                    vec![pos.into_opt()?]
                }
                (Term::RecRecord(..) | Term::Record(_), Some(id)) => {
                    let def = Def::Field {
                        ident: id,
                        value: None,
                        record: term.clone(),
                        metadata: FieldMetadata::default(),
                    };
                    let cousins = resolver.cousin_defs(&def);
                    cousins
                        .into_iter()
                        .filter_map(|(loc, _)| loc.pos.into_opt())
                        .collect()
                }
                _ => {
                    return None;
                }
            };
            Some(ret)
        }

        inner(self, term, ident).unwrap_or_default()
    }

    /// If `span` is pointing at the identifier binding a record field, returns
    /// all the places that the record field is referenced.
    ///
    /// This is a sort of inverse of `get_defs`, at least when the argument to `get_defs`
    /// is a static access: the spans returned by this function are exactly the static accesses
    /// that, when passed to `get_defs`, return `span`.
    ///
    /// This function can be expensive, because it calls `get_defs` on every static access
    /// that could potentially be referencing this field.
    pub fn get_field_refs(&self, span: RawSpan) -> Vec<RawSpan> {
        // The inner function returning Option is just for ?-early-return convenience.
        fn inner(server: &Server, span: RawSpan) -> Option<Vec<RawSpan>> {
            let ident = server.lookup_ident_by_position(span.start_pos()).ok()??;
            let term = server.lookup_term_by_position(span.start_pos()).ok()??;

            if let Term::RecRecord(..) | Term::Record(_) = term.as_ref() {
                let accesses = server.analysis.get_static_accesses(ident.ident);
                Some(
                    accesses
                        .into_iter()
                        .filter_map(|access| {
                            let Term::Op1(UnaryOp::StaticAccess(id), _) = access.as_ref() else {
                                return None;
                            };
                            if server.get_defs(&access, None).contains(&span) {
                                id.pos.into_opt()
                            } else {
                                None
                            }
                        })
                        .collect(),
                )
            } else {
                None
            }
        }
        inner(self, span).unwrap_or_default()
    }
}
