use std::ops::Range;

use codespan::{FileId, Files};
use codespan_reporting::diagnostic::{self, Diagnostic};
use lsp_types::NumberOrString;
use nickel_lang_core::position::RawSpan;

use crate::codespan_lsp::byte_span_to_range;

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

    fn from_span(span: &RawSpan, files: &Files<String>) -> Self {
        Self::from_codespan(
            &span.src_id,
            &(span.start.to_usize()..span.end.to_usize()),
            files,
        )
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

impl DiagnosticCompat for lsp_types::Diagnostic {
    fn from_codespan(diagnostic: Diagnostic<FileId>, files: &mut Files<String>) -> Vec<Self> {
        let severity = Some(match diagnostic.severity {
            diagnostic::Severity::Bug => lsp_types::DiagnosticSeverity::WARNING,
            diagnostic::Severity::Error => lsp_types::DiagnosticSeverity::ERROR,
            diagnostic::Severity::Warning => lsp_types::DiagnosticSeverity::WARNING,
            diagnostic::Severity::Note => lsp_types::DiagnosticSeverity::INFORMATION,
            diagnostic::Severity::Help => lsp_types::DiagnosticSeverity::HINT,
        });

        diagnostic
            .labels
            .iter()
            .map(|label| {
                let range = lsp_types::Range::from_codespan(&label.file_id, &label.range, files);

                let code = diagnostic.code.clone().map(NumberOrString::String);
                let message = format!("{}\n{}", diagnostic.message, diagnostic.notes.join("\n"));

                lsp_types::Diagnostic {
                    range,
                    severity,
                    code,
                    message,
                    ..Default::default()
                }
            })
            .collect::<Vec<_>>()
    }
}
