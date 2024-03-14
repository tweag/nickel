use std::ops::Range;

use codespan::{FileId, Files};
use codespan_reporting::diagnostic::{self, Diagnostic, LabelStyle};
use lsp_types::{DiagnosticRelatedInformation, NumberOrString};
use nickel_lang_core::{error::UNKNOWN_SOURCE_NAME, position::RawSpan};
use serde::{Deserialize, Serialize};

use crate::codespan_lsp::byte_span_to_range;

/// A more serializable alternative to lsp_types::Diagnostic
///
/// lsp_types::Diagnostic is not serializable to bincode (and therefore not
/// sendable across an ipc-channel channel) because it has optional fields that
/// get skipped serializing if empty. See <https://github.com/serde-rs/serde/issues/1732>
#[derive(Debug, Serialize, Deserialize)]
pub struct SerializableDiagnostic {
    pub range: lsp_types::Range,
    pub severity: Option<lsp_types::DiagnosticSeverity>,
    pub code: Option<NumberOrString>,
    pub message: String,
    pub related_information: Option<Vec<lsp_types::DiagnosticRelatedInformation>>,
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
    // We convert a single diagnostic into a list of diagnostics: one "main" diagnostic and an
    // additional hint for each label. LSP also has a `related_information` field that we could
    // use for the labels, but we prefer multiple diagnostics because (1) that's what rust-analyzer
    // does and (2) it gives a better experience in helix, at least.
    //
    // We do use the `related_information` field for cross-file diagnostics, because the main
    // diagnostics notification assumes all the diagnostics are for the same file.
    fn from_codespan(
        file_id: FileId,
        diagnostic: Diagnostic<FileId>,
        files: &mut Files<String>,
    ) -> Vec<Self>;
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
    fn from_codespan(
        file_id: FileId,
        diagnostic: Diagnostic<FileId>,
        files: &mut Files<String>,
    ) -> Vec<Self> {
        let severity = Some(match diagnostic.severity {
            diagnostic::Severity::Bug => lsp_types::DiagnosticSeverity::WARNING,
            diagnostic::Severity::Error => lsp_types::DiagnosticSeverity::ERROR,
            diagnostic::Severity::Warning => lsp_types::DiagnosticSeverity::WARNING,
            diagnostic::Severity::Note => lsp_types::DiagnosticSeverity::INFORMATION,
            diagnostic::Severity::Help => lsp_types::DiagnosticSeverity::HINT,
        });

        let code = diagnostic.code.clone().map(NumberOrString::String);

        let mut diagnostics = Vec::new();

        let within_file_labels = diagnostic
            .labels
            .iter()
            .filter(|label| label.file_id == file_id);

        let cross_file_labels = diagnostic.labels.iter().filter(|label| {
            label.file_id != file_id
                // When errors point to generated code, the diagnostic-formatting machinery
                // replaces it with a generated file. This is appropriate for command line errors,
                // but not for us. It would be nice if we could filter this out at an earlier stage.
                && files.name(label.file_id) != UNKNOWN_SOURCE_NAME
        });

        if !diagnostic.message.is_empty() {
            // What location should we use for the "overall" diagnostic? `Diagnostic` doesn't
            // have an "overall" location, so we arbitrarily take the location of the first primary label
            // (falling back to the first label if there's no primary one).
            let maybe_label = within_file_labels
                .clone()
                .find(|lab| lab.style == LabelStyle::Primary)
                .or_else(|| within_file_labels.clone().next());

            if let Some(label) = maybe_label {
                let range = lsp_types::Range::from_codespan(&label.file_id, &label.range, files);
                let message = if diagnostic.notes.is_empty() {
                    diagnostic.message.clone()
                } else {
                    format!("{}\n{}", diagnostic.message, diagnostic.notes.join("\n"))
                };
                diagnostics.push(SerializableDiagnostic {
                    range,
                    severity,
                    code: code.clone(),
                    message,
                    related_information: Some(
                        cross_file_labels
                            .map(|label| DiagnosticRelatedInformation {
                                location: lsp_types::Location::from_codespan(
                                    &label.file_id,
                                    &label.range,
                                    files,
                                ),
                                message: label.message.clone(),
                            })
                            .collect(),
                    ),
                });
            }
        }

        diagnostics.extend(within_file_labels.map(|label| {
            let range = lsp_types::Range::from_codespan(&label.file_id, &label.range, files);

            SerializableDiagnostic {
                range,
                message: label.message.clone(),
                severity: Some(lsp_types::DiagnosticSeverity::HINT),
                code: code.clone(),
                related_information: None,
            }
        }));
        diagnostics
    }
}

impl DiagnosticCompat for lsp_types::Diagnostic {
    fn from_codespan(
        file_id: FileId,
        diagnostic: Diagnostic<FileId>,
        files: &mut Files<String>,
    ) -> Vec<Self> {
        SerializableDiagnostic::from_codespan(file_id, diagnostic, files)
            .into_iter()
            .map(From::from)
            .collect()
    }
}
