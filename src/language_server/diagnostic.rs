use codespan::{FileId, Files};
use codespan_reporting::diagnostic::{self, Diagnostic, Label};
use lsp_types::{DiagnosticRelatedInformation, Location, NumberOrString, Position, Range, Url};

use crate::error::TypecheckError;

pub trait DiagnosticCompat: Sized {
    fn from_codespan(diagnostic: Diagnostic<FileId>, files: &mut Files<String>) -> Vec<Self>;
}

pub trait LocationCompat {
    fn from_codespan(label: &Label<FileId>, files: &Files<String>) -> Self;
}

impl LocationCompat for lsp_types::Range {
    fn from_codespan(label: &Label<FileId>, files: &Files<String>) -> Self {
        let start = files.location(label.file_id, label.range.start as u32);
        let end = files.location(label.file_id, label.range.start as u32);

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
        Range {
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
                let range = lsp_types::Range::from_codespan(&label, files);

                let code = diagnostic.code.clone().map(NumberOrString::String);
                let mut message = label.message.clone();
                if message.is_empty() {
                    message = diagnostic.message.clone();
                }

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
