use std::ops::Range;

use codespan::{FileId, Files};
use codespan_reporting::diagnostic::{self, Diagnostic};
use lsp_types::{NumberOrString, Position};
use nickel_lang_core::position::RawSpan;

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
        let start = files.location(*file_id, range.start as u32);
        let end = files.location(*file_id, range.end as u32);

        let (line_start, col_start, line_end, col_end) = match (start, end) {
            (Ok(start_loc), Ok(end_loc)) => (
                start_loc.line.0,
                start_loc.column.0,
                end_loc.line.0,
                end_loc.column.0,
            ),
            (Ok(loc), _) | (_, Ok(loc)) => (loc.line.0, loc.column.0, loc.line.0, loc.column.0),
            _ => (0, 0, 0, 0),
        };
        lsp_types::Range {
            start: Position {
                line: line_start,
                character: col_start,
            },
            end: Position {
                line: line_end,
                character: col_end,
            },
        }
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
            diagnostic::Severity::Bug => lsp_types::DiagnosticSeverity::Warning,
            diagnostic::Severity::Error => lsp_types::DiagnosticSeverity::Error,
            diagnostic::Severity::Warning => lsp_types::DiagnosticSeverity::Warning,
            diagnostic::Severity::Note => lsp_types::DiagnosticSeverity::Information,
            diagnostic::Severity::Help => lsp_types::DiagnosticSeverity::Hint,
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
