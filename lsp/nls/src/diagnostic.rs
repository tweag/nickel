use std::ops::Range;

use codespan::{FileId, Files};
use codespan_reporting::diagnostic::{self, Diagnostic};
use lsp_types::NumberOrString;
use nickel_lang_core::position::RawSpan;
use serde::{Deserialize, Serialize};

use crate::codespan_lsp::byte_span_to_range;

/// A more serializable alternative to lsp_types::Diagnostic
///
/// lsp_types::Diagnostic is not serializable to bincode (and therefore not
/// sendable across an ipc-channel channel) because it has optional fields that
/// get skipped serializing if empty. See https://github.com/serde-rs/serde/issues/1732
#[derive(Debug, Serialize, Deserialize)]
pub struct SerializableDiagnostic {
    pub range: lsp_types::Range,
    pub severity: Option<lsp_types::DiagnosticSeverity>,
    pub code: Option<NumberOrString>,
    pub message: String,
}

impl From<SerializableDiagnostic> for lsp_types::Diagnostic {
    fn from(d: SerializableDiagnostic) -> Self {
        Self {
            range: d.range,
            severity: d.severity,
            code: d.code,
            message: d.message,
            ..Default::default()
        }
    }
}

/// Convert [codespan_reporting::diagnostic::Diagnostic] into a list of another type
/// Diagnostics tend to contain a list of labels pointing to errors in the code which
/// we want to extract, hence a list of `Self`
pub trait DiagnosticCompat: Sized {
    fn from_codespan(diagnostic: Diagnostic<FileId>, files: &mut Files<String>) -> Vec<Self>;
}

/// Determine the position of a [codespan_reporting::diagnostic::Label] by looking it up
/// in the file cache
pub trait LocationCompat: Sized {
    fn from_codespan(file_id: &FileId, range: &Range<usize>, files: &Files<String>) -> Self;

    fn from_span(span: &RawSpan, files: &Files<String>) -> Self {
        Self::from_codespan(
            &span.src_id,
            &(span.start.to_usize()..span.end.to_usize()),
            files,
        )
    }
}

impl LocationCompat for lsp_types::Range {
    fn from_codespan(file_id: &FileId, range: &Range<usize>, files: &Files<String>) -> Self {
        byte_span_to_range(files, *file_id, range.clone()).unwrap_or(lsp_types::Range {
            start: Default::default(),
            end: Default::default(),
        })
    }
}

impl LocationCompat for lsp_types::Location {
    fn from_codespan(file_id: &FileId, range: &Range<usize>, files: &Files<String>) -> Self {
        lsp_types::Location {
            uri: lsp_types::Url::from_file_path(files.name(*file_id)).unwrap(),
            range: lsp_types::Range::from_codespan(file_id, range, files),
        }
    }
}

impl DiagnosticCompat for SerializableDiagnostic {
    fn from_codespan(diagnostic: Diagnostic<FileId>, files: &mut Files<String>) -> Vec<Self> {
        let severity = Some(match diagnostic.severity {
            diagnostic::Severity::Bug => lsp_types::DiagnosticSeverity::WARNING,
            diagnostic::Severity::Error => lsp_types::DiagnosticSeverity::ERROR,
            diagnostic::Severity::Warning => lsp_types::DiagnosticSeverity::WARNING,
            diagnostic::Severity::Note => lsp_types::DiagnosticSeverity::INFORMATION,
            diagnostic::Severity::Help => lsp_types::DiagnosticSeverity::HINT,
        });

        // TODO: this is giving repeated diagnostics, all with the same label
        diagnostic
            .labels
            .iter()
            .map(|label| {
                let range = lsp_types::Range::from_codespan(&label.file_id, &label.range, files);

                let code = diagnostic.code.clone().map(NumberOrString::String);
                let message = format!("{}\n{}", diagnostic.message, diagnostic.notes.join("\n"));

                SerializableDiagnostic {
                    range,
                    severity,
                    code,
                    message,
                }
            })
            .collect::<Vec<_>>()
    }
}

impl DiagnosticCompat for lsp_types::Diagnostic {
    fn from_codespan(diagnostic: Diagnostic<FileId>, files: &mut Files<String>) -> Vec<Self> {
        SerializableDiagnostic::from_codespan(diagnostic, files)
            .into_iter()
            .map(From::from)
            .collect()
    }
}
