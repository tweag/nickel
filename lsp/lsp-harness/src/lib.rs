mod jsonrpc;
mod output;

use std::collections::{hash_map::Entry, HashMap};

use assert_cmd::prelude::CommandCargoExt;
pub use jsonrpc::Server;
use lsp_types::{
    notification::{Notification, PublishDiagnostics},
    request::{
        Completion, DocumentSymbolRequest, Formatting, GotoDefinition, HoverRequest, References,
        Rename, Request as LspRequest,
    },
    CompletionParams, DocumentFormattingParams, DocumentSymbolParams, GotoDefinitionParams,
    HoverParams, PublishDiagnosticsParams, ReferenceParams, RenameParams, Url,
};
pub use output::LspDebug;
use serde::Deserialize;

/// A text fixture consists of multiple files (each labelled with a filename)
/// and multiple requests to run on those files.
///
/// The test is executed by loading the files into the language server and then
/// executing the requests one-by-one. The responses are printed and compared
/// against a reference response.
pub struct TestFixture {
    pub files: Vec<TestFile>,
    pub reqs: Vec<Request>,
    pub expected_diags: Vec<Url>,
}

pub struct TestFile {
    pub uri: Url,
    pub contents: String,
}

/// A subset of LSP requests that our harness supports.
#[derive(Clone, Deserialize, Debug)]
#[serde(tag = "type")]
pub enum Request {
    GotoDefinition(GotoDefinitionParams),
    References(ReferenceParams),
    Completion(CompletionParams),
    Formatting(DocumentFormattingParams),
    Hover(HoverParams),
    Rename(RenameParams),
    Symbols(DocumentSymbolParams),
}

#[derive(Deserialize, Debug, Default)]
pub struct Requests {
    request: Option<Vec<Request>>,
    // A list of files to compare diagnostic snapshots.
    // TODO: once the background output has settled down a little,
    // consider checking diagnostic snapshots for all tests
    diagnostic: Option<Vec<Url>>,
}

/// Produce an absolute filepath `Url` that is safe to use in tests.
///
/// The `C:\` prefix on Windows is needed both to avoid `Url::from_file_path` failing due to a
/// missing drive letter and to avoid invalid filepath errors.
pub fn file_url_from_path(path: &str) -> Result<Url, String> {
    assert!(path.starts_with('/'));

    let path = if cfg!(unix) {
        path.to_owned()
    } else {
        format!("C:\\{}", &path[1..])
    };

    Url::from_file_path(&path).map_err(|()| format!("Unable to convert filepath {path:?} into Url"))
}

#[cfg(windows)]
fn modify_requests_uris(mut reqs: Requests) -> Requests {
    match reqs.request.iter_mut().next() {
        None => {}
        Some(rs) => {
            for req in rs.iter_mut() {
                modify_request_uri(req);
            }
        }
    }
    match reqs.diagnostic.iter_mut().next() {
        None => {}
        Some(urls) => {
            for url in urls.iter_mut() {
                *url = file_url_from_path(url.path()).unwrap();
            }
        }
    };
    reqs
}

#[cfg(windows)]
fn modify_request_uri(req: &mut Request) {
    fn file_url(url: &Url) -> Url {
        file_url_from_path(url.path()).unwrap()
    }

    match req {
        Request::GotoDefinition(params) => {
            params.text_document_position_params.text_document.uri =
                file_url(&params.text_document_position_params.text_document.uri);
        }
        Request::References(params) => {
            params.text_document_position.text_document.uri =
                file_url(&params.text_document_position.text_document.uri);
        }
        Request::Completion(params) => {
            params.text_document_position.text_document.uri =
                file_url(&params.text_document_position.text_document.uri);
        }
        Request::Formatting(params) => {
            params.text_document.uri = file_url(&params.text_document.uri);
        }
        Request::Hover(params) => {
            params.text_document_position_params.text_document.uri =
                file_url(&params.text_document_position_params.text_document.uri);
        }
        Request::Rename(params) => {
            params.text_document_position.text_document.uri =
                file_url(&params.text_document_position.text_document.uri);
        }
        Request::Symbols(params) => {
            params.text_document.uri = file_url(&params.text_document.uri);
        }
    }
}

impl TestFixture {
    pub fn parse(s: &str) -> Result<Self, String> {
        let mut header_lines = Vec::new();
        let mut content = String::new();
        let mut files = Vec::new();
        let mut push_file = |header: &[&str], content: &mut String| {
            let uri = match header {
                &[path] => file_url_from_path(path),
                _ => Err(format!("Files can only have 1 header line: {header:?}")),
            }?;
            files.push(TestFile {
                uri,
                contents: std::mem::take(content),
            });
            Ok::<(), String>(())
        };

        for line in s.lines() {
            if line.starts_with("###") {
                if !content.is_empty() {
                    push_file(&header_lines, &mut content)?;
                    header_lines.clear();
                }

                header_lines.push(line.trim_start_matches('#').trim());
            } else {
                content.push_str(line);
                content.push('\n');
            }
        }

        if !content.is_empty() {
            // The text fixture ended with a nickel file; there are no lsp
            // requests specified.
            push_file(&header_lines, &mut content)?;
            Ok(TestFixture {
                files,
                reqs: Vec::new(),
                expected_diags: Vec::new(),
            })
        } else {
            // The remaining lines at the end of the file are a toml source
            // listing the LSP requests we need to make and the diagnostics
            // we expect to receive.
            let remaining = header_lines.join("\n");
            let reqs: Requests = toml::from_str(&remaining).unwrap();
            #[cfg(windows)]
            let reqs = modify_requests_uris(reqs);

            Ok(TestFixture {
                files,
                reqs: reqs.request.unwrap_or_default(),
                expected_diags: reqs.diagnostic.unwrap_or_default(),
            })
        }
    }
}

pub struct TestHarness {
    srv: Server,
    pub out: Vec<u8>,
}

impl Default for TestHarness {
    fn default() -> Self {
        TestHarness::new()
    }
}

impl TestHarness {
    pub fn new_with_options(initialization_options: Option<serde_json::Value>) -> Self {
        let cmd = std::process::Command::cargo_bin("nls").unwrap();
        let srv = Server::new_with_options(cmd, initialization_options).unwrap();
        Self {
            srv,
            out: Vec::new(),
        }
    }
    pub fn new() -> Self {
        Self::new_with_options(None)
    }

    pub fn request<T: LspRequest>(&mut self, params: T::Params)
    where
        T::Result: LspDebug,
    {
        let result = self.srv.send_request::<T>(params).unwrap();
        result.debug(&mut self.out).unwrap();
        self.out.push(b'\n');
    }

    pub fn request_dyn(&mut self, req: Request) {
        match req {
            Request::GotoDefinition(d) => self.request::<GotoDefinition>(d),
            Request::Completion(c) => self.request::<Completion>(c),
            Request::Formatting(f) => self.request::<Formatting>(f),
            Request::Hover(h) => self.request::<HoverRequest>(h),
            Request::References(r) => self.request::<References>(r),
            Request::Rename(r) => self.request::<Rename>(r),
            Request::Symbols(s) => self.request::<DocumentSymbolRequest>(s),
        }
    }

    pub fn prepare_files(&mut self, fixture: &TestFixture) {
        let mut file_versions = HashMap::new();

        if fixture.files.is_empty() {
            panic!("no files");
        }

        for file in &fixture.files {
            match file_versions.entry(file.uri.clone()) {
                Entry::Occupied(mut version) => {
                    *version.get_mut() += 1;
                    self.srv
                        .replace_file(file.uri.clone(), *version.get(), &file.contents)
                        .unwrap();
                }
                Entry::Vacant(entry) => {
                    self.send_file(file.uri.clone(), &file.contents);
                    entry.insert(1);
                }
            }
        }
    }

    pub fn send_file(&mut self, uri: Url, contents: &str) {
        self.srv.send_file(uri.clone(), contents).unwrap();
    }

    // Waits (until forever, if necessary) for the first diagnostics, and then
    // returns them.
    pub fn wait_for_diagnostics(&mut self) -> PublishDiagnosticsParams {
        loop {
            match self.srv.recv().unwrap() {
                jsonrpc::ServerMessage::Notification(note) => {
                    if note.method == PublishDiagnostics::METHOD {
                        return serde_json::value::from_value(note.params).unwrap();
                    }
                }
                jsonrpc::ServerMessage::Response(_) => {}
            }
        }
    }

    // For debug purposes, drain and print notifications.
    pub fn drain_diagnostics(&mut self, files: impl Iterator<Item = Url>) {
        let mut diags = self.drain_diagnostics_inner(files);

        // Sort and dedup the diagnostics, for stability of the output.
        let mut files: Vec<_> = diags.keys().cloned().collect();
        files.sort();

        for f in files {
            let mut diags = diags.remove(&f).unwrap();
            diags.sort_by_cached_key(|d| (d.range.start, d.range.end, d.message.clone()));
            diags.dedup_by_key(|d| (d.range.start, d.range.end, d.message.clone()));
            for d in diags {
                (&f, d).debug(&mut self.out).unwrap();
                self.out.push(b'\n');
            }
        }
    }

    fn drain_diagnostics_inner(
        &mut self,
        files: impl Iterator<Item = Url>,
    ) -> HashMap<Url, Vec<lsp_types::Diagnostic>> {
        let mut diags: HashMap<Url, Vec<lsp_types::Diagnostic>> = HashMap::new();

        // This is pretty fragile, but I don't know of a better way to handle notifications: we
        // expect 2 rounds of notifications from each file (one synchronously from typechecking,
        // and one from the background eval). So we just wait until we've received both, and we
        // concatenate their outputs.
        let mut waiting: HashMap<Url, u32> = files.map(|f| (f, 2)).collect();

        // Handle a single diagnostic, returning true if we have enough of them.
        let mut handle_diag = |diag: PublishDiagnosticsParams| -> bool {
            if let Some(remaining) = waiting.get_mut(&diag.uri) {
                *remaining -= 1;
                if *remaining == 0 {
                    waiting.remove(&diag.uri);
                }
                diags
                    .entry(diag.uri.clone())
                    .or_default()
                    .extend(diag.diagnostics);
            }

            waiting.is_empty()
        };

        for msg in self.srv.pending_notifications() {
            if msg.method == PublishDiagnostics::METHOD {
                let diag: PublishDiagnosticsParams =
                    serde_json::value::from_value(msg.params).unwrap();
                if handle_diag(diag) {
                    return diags;
                }
            }
        }

        while !handle_diag(self.wait_for_diagnostics()) {}

        diags
    }
}
