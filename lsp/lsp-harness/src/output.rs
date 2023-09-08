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

impl LspDebug for lsp_types::CompletionItem {
    fn debug(&self, mut w: impl Write) -> std::io::Result<()> {
        write!(w, "{}", self.label)
    }
}

impl LspDebug for lsp_types::CompletionResponse {
    fn debug(&self, w: impl Write) -> std::io::Result<()> {
        match self {
            lsp_types::CompletionResponse::Array(items) => {
                // The order of completions is non-deterministic, so sort them.
                let mut items = items.clone();
                items.sort_by_key(|i| i.label.clone());
                Iter(items.iter()).debug(w)
            }
            lsp_types::CompletionResponse::List(list) => {
                let mut items = list.items.clone();
                items.sort_by_key(|i| i.label.clone());
                Iter(list.items.iter()).debug(w)
            }
        }
    }
}

impl LspDebug for lsp_types::TextEdit {
    fn debug(&self, mut w: impl Write) -> std::io::Result<()> {
        write!(w, "<{}> {}", self.range.debug_str(), self.new_text)
    }
}

impl LspDebug for Vec<lsp_types::TextEdit> {
    fn debug(&self, w: impl Write) -> std::io::Result<()> {
        Iter(self.iter()).debug(w)
    }
}

impl LspDebug for lsp_types::Hover {
    fn debug(&self, mut w: impl Write) -> std::io::Result<()> {
        write!(w, "<{}>", self.range.debug_str())?;

        match &self.contents {
            lsp_types::HoverContents::Scalar(s) => s.debug(w),
            lsp_types::HoverContents::Array(items) => {
                let mut items = items.clone();
                items.sort_by_cached_key(|i| match i {
                    lsp_types::MarkedString::String(s) => s.clone(),
                    lsp_types::MarkedString::LanguageString(l_str) => l_str.value.clone(),
                });
                Iter(items.iter()).debug(w)
            }
            lsp_types::HoverContents::Markup(s) => s.debug(w),
        }
    }
}

impl LspDebug for lsp_types::MarkedString {
    fn debug(&self, mut w: impl Write) -> std::io::Result<()> {
        match self {
            lsp_types::MarkedString::String(s) => write!(w, "{s}"),
            lsp_types::MarkedString::LanguageString(l_str) => {
                write!(w, "```{}\n{}\n```", l_str.language, l_str.value)
            }
        }
    }
}

impl LspDebug for lsp_types::MarkupContent {
    fn debug(&self, mut w: impl Write) -> std::io::Result<()> {
        write!(w, "{}", self.value)
    }
}
