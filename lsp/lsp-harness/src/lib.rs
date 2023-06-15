mod jsonrpc;
mod output;

pub use jsonrpc::Server;
use log::error;
use lsp_types::{
    GotoDefinitionParams, Location, Position, Range, TextDocumentIdentifier,
    TextDocumentPositionParams, Url,
};
pub use output::LspDebug;

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

impl TestFixture {
    pub fn parse(s: &str) -> Option<Self> {
        <Self as Parse>::parse(s)
    }
}

pub struct TestFile {
    pub uri: Url,
    pub contents: String,
}

/// A subset of LSP requests that our harness supports.
pub enum Request {
    GotoDefinition(GotoDefinitionParams),
}

/// A private FromStr, with simpler error handling and (because it's
/// private) the ability to impl it for external types.
trait Parse: Sized {
    fn parse(s: &str) -> Option<Self>;
}

impl Parse for Position {
    fn parse(s: &str) -> Option<Self> {
        let (line, char) = s.split_once(':')?;
        Some(Position {
            line: line.parse().ok()?,
            character: char.parse().ok()?,
        })
    }
}

impl Parse for Range {
    fn parse(s: &str) -> Option<Self> {
        let (start, end) = s.split_once('-')?;
        Some(Range {
            start: Position::parse(start)?,
            end: Position::parse(end)?,
        })
    }
}

impl Parse for Location {
    fn parse(s: &str) -> Option<Self> {
        let (path, range) = s.split_once(':')?;
        let uri = Url::from_file_path(path).ok()?;
        let range = Range::parse(range)?;
        Some(Location { uri, range })
    }
}

impl Parse for TextDocumentPositionParams {
    fn parse(s: &str) -> Option<Self> {
        let (path, pos) = s.split_once(':')?;
        let uri = Url::from_file_path(path).ok()?;
        let pos = Position::parse(pos)?;
        Some(TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri },
            position: pos,
        })
    }
}

impl Parse for Request {
    fn parse(s: &str) -> Option<Self> {
        let (method, params) = s.split_once(' ')?;
        match method {
            "GotoDefinition" => Some(Request::GotoDefinition(GotoDefinitionParams {
                text_document_position_params: TextDocumentPositionParams::parse(params)?,
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            })),
            _ => None,
        }
    }
}

impl Parse for TestFixture {
    fn parse(s: &str) -> Option<Self> {
        let mut header_lines = Vec::new();
        let mut content = String::new();
        let mut files = Vec::new();
        for line in s.lines() {
            if line.starts_with("###") {
                if !content.is_empty() {
                    if header_lines.len() > 1 {
                        error!("files can only have 1 header line");
                        return None;
                    }

                    let uri = Url::from_file_path(header_lines.first()?).ok()?;
                    files.push(TestFile {
                        uri,
                        contents: std::mem::take(&mut content),
                    });

                    header_lines.clear();
                }

                header_lines.push(line.trim_start_matches('#').trim());
            } else {
                content.push_str(line);
                content.push('\n');
            }
        }

        // Remaining lines at the end of the file are for requests.
        let mut reqs = Vec::new();
        for req in header_lines {
            reqs.push(Request::parse(req)?);
        }

        Some(TestFixture { files, reqs })
    }
}
