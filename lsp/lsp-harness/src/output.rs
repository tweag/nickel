//! Utilities for printing out LSP responses.
//!
//! Essentially, a less verbose Debug with stable output.

use std::io::Write;

use lsp_types::GotoDefinitionResponse;

pub trait LspDebug {
    fn debug(&self, w: impl Write) -> std::io::Result<()>;

    fn debug_str(&self) -> String {
        let mut ret = Vec::new();
        self.debug(&mut ret).unwrap();
        String::from_utf8(ret).unwrap()
    }
}

impl<'a, T: LspDebug> LspDebug for &'a T {
    fn debug(&self, w: impl Write) -> std::io::Result<()> {
        <T as LspDebug>::debug(*self, w)
    }
}

impl<T: LspDebug> LspDebug for Option<T> {
    fn debug(&self, mut w: impl Write) -> std::io::Result<()> {
        match self {
            Some(s) => s.debug(w),
            None => write!(w, "None"),
        }
    }
}

// A wrapper to let us write a blanket-ish impl for iterators of LspDebugs.
struct Iter<I>(I);

impl<T: LspDebug, I: Iterator<Item = T> + Clone> LspDebug for Iter<I> {
    fn debug(&self, mut w: impl Write) -> std::io::Result<()> {
        write!(
            w,
            "[{}]",
            self.0
                .clone()
                .map(|x| x.debug_str())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

impl LspDebug for lsp_types::Range {
    fn debug(&self, mut w: impl Write) -> std::io::Result<()> {
        write!(
            w,
            "{}:{}-{}:{}",
            self.start.line, self.start.character, self.end.line, self.end.character
        )
    }
}

impl LspDebug for lsp_types::Location {
    fn debug(&self, mut w: impl Write) -> std::io::Result<()> {
        write!(w, "{}:{}", self.uri.as_str(), self.range.debug_str())
    }
}

impl LspDebug for lsp_types::LocationLink {
    fn debug(&self, mut w: impl Write) -> std::io::Result<()> {
        write!(
            w,
            "{}:{}/{}",
            self.target_uri.as_str(),
            self.target_range.debug_str(),
            self.target_selection_range.debug_str()
        )
    }
}

impl LspDebug for lsp_types::GotoDefinitionResponse {
    fn debug(&self, w: impl Write) -> std::io::Result<()> {
        match self {
            GotoDefinitionResponse::Scalar(s) => s.debug(w),
            GotoDefinitionResponse::Array(a) => Iter(a.iter()).debug(w),
            GotoDefinitionResponse::Link(a) => Iter(a.iter()).debug(w),
        }
    }
}
