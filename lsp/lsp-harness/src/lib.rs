mod jsonrpc;
mod output;

pub use jsonrpc::Server;
use log::error;
use lsp_types::{
    CompletionParams, DocumentFormattingParams, GotoDefinitionParams, HoverParams, Url,
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
}

pub struct TestFile {
    pub uri: Url,
    pub contents: String,
}

/// A subset of LSP requests that our harness supports.
#[derive(Deserialize, Debug)]
#[serde(tag = "type")]
pub enum Request {
    GotoDefinition(GotoDefinitionParams),
    Completion(CompletionParams),
    Formatting(DocumentFormattingParams),
    Hover(HoverParams),
}

#[derive(Deserialize, Debug, Default)]
pub struct Requests {
    request: Vec<Request>,
}

impl TestFixture {
    pub fn parse(s: &str) -> Option<Self> {
        let mut header_lines = Vec::new();
        let mut content = String::new();
        let mut files = Vec::new();
        let mut push_file = |header: &[&str], content: &mut String| {
            if header.len() > 1 {
                error!("files can only have 1 header line");
                return None;
            }

            let uri = Url::from_file_path(header.first()?).ok()?;
            files.push(TestFile {
                uri,
                contents: std::mem::take(content),
            });
            Some(())
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
            Some(TestFixture {
                files,
                reqs: Vec::new(),
            })
        } else {
            // The remaining lines at the end of the file are a toml source
            // listing the LSP requests we need to make.
            let remaining = header_lines.join("\n");
            let reqs: Requests = toml::from_str(&remaining).unwrap();
            Some(TestFixture {
                files,
                reqs: reqs.request,
            })
        }
    }
}
