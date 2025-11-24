//! Error handling for the CLI.

use std::path::PathBuf;

use nickel_lang_core::{
    error::{
        Diagnostic, IntoDiagnostics, ParseError,
        report::{ColorOpt, ErrorFormat, report},
    },
    files::{FileId, Files},
    program::{FieldOverride, FieldPath},
};

/// Data about an unknown field error.
pub struct UnknownFieldData {
    /// The field that was unknown.
    pub path: FieldPath,
    /// The list of customizable fields used to suggest similar alternative fields.
    pub field_list: Vec<FieldPath>,
}

/// Errors related to mishandling the CLI.
pub enum CliUsageError {
    /// Tried to override a field which doesn't exist.
    UnknownFieldOverride(UnknownFieldData),
    /// Tried to assign a field which doesn't exist.
    UnknownFieldAssignment(UnknownFieldData),
    /// Tried to override an defined field without the `--override` argument.
    CantAssignNonInput { ovd: FieldOverride },
    /// A parse error occurred when trying to parse an assignment.
    AssignmentParseError { error: ParseError },
    /// A parse error occurred when trying to parse a field path.
    FieldPathParseError { error: ParseError },
    /// Couldn't determine the format of an input.
    CantDetectFormat { path: PathBuf },
    #[cfg(feature = "nix-experimental")]
    NoNixConversion { path: PathBuf },
}

pub enum Error {
    Program {
        files: Files,
        error: nickel_lang_core::error::Error,
    },
    Io {
        error: std::io::Error,
    },
    #[cfg(feature = "repl")]
    Repl {
        error: nickel_lang_core::repl::InitError,
    },
    #[cfg(feature = "format")]
    Format {
        error: crate::format::FormatError,
    },
    /// An invalid invocation of the CLI that couldn't be caught by the simple parsing provided by
    /// clap.
    CliUsage {
        files: Files,
        error: CliUsageError,
    },
    #[cfg(feature = "package-experimental")]
    NoManifest,
    #[cfg(feature = "package-experimental")]
    Package {
        error: nickel_lang_package::error::Error,
    },
    FailedTests,
}

impl IntoDiagnostics for CliUsageError {
    fn into_diagnostics(self, files: &mut Files) -> Vec<Diagnostic<FileId>> {
        fn mk_unknown_diags<FileId>(
            data: UnknownFieldData,
            method: &str,
        ) -> Vec<Diagnostic<FileId>> {
            let mut notes = vec![format!(
                "`{path}` doesn't refer to a record field accessible from the root of the \
                 configuration.",
                path = data.path
            )];

            let fields_as_strings: Vec<String> =
                data.field_list.iter().map(ToString::to_string).collect();

            nickel_lang_core::error::suggest::add_suggestion(
                &mut notes,
                &fields_as_strings,
                &data.path.to_string(),
            );

            notes.push(
                "Use `nickel <COMMAND> [OPTIONS] -- list` to show a list of available fields."
                    .to_owned(),
            );

            vec![
                Diagnostic::error()
                    .with_message(format!(
                        "invalid {method}: unknown field `{path}`",
                        path = data.path
                    ))
                    .with_notes(notes),
            ]
        }

        match self {
            CliUsageError::UnknownFieldOverride(data) => mk_unknown_diags(data, "override"),
            CliUsageError::UnknownFieldAssignment(data) => mk_unknown_diags(data, "assignment"),
            CliUsageError::CantAssignNonInput {
                ovd: FieldOverride { path, value, .. },
            } => {
                vec![
                    Diagnostic::error()
                        .with_message(format!("invalid assignment: `{path}` isn't an input"))
                        .with_notes(vec![
                            format!(
                                "`{path}` already has a value and thus can't be assigned \
                            without `--override`."
                            ),
                            format!(
                                "If you really want to override this field, please use \
                            `--override '{path}={value}'` instead."
                            ),
                        ]),
                ]
            }
            CliUsageError::AssignmentParseError { error } => {
                let mut diags = IntoDiagnostics::into_diagnostics(error, files);
                diags.push(
                    Diagnostic::note()
                        .with_message("when parsing a field assignment on the command line")
                        .with_notes(vec![
                            "A field assignment must be of the form `<field path>=<value>`, \
                            where `<field path>` is a dot-separated list of fields and `<value>` \
                            is a valid Nickel expression."
                                .to_owned(),
                            "For example: `config.database.\"$port\"=8080`".to_owned(),
                        ]),
                );
                diags
            }
            CliUsageError::FieldPathParseError { error } => {
                let mut diags = IntoDiagnostics::into_diagnostics(error, files);
                diags.push(
                    Diagnostic::note()
                        .with_message("when parsing a field path on the command line")
                        .with_notes(vec![
                            "A field path must be a dot-separated list of fields. Special \
                            characters must be properly escaped, both for Nickel and for the \
                            shell."
                                .to_owned(),
                            "For example: a field path `config.\"$port\"` in Nickel source code \
                            must be written `config.\\\"\\$port\\\"` or `'config.\"$port\"'` on \
                            a POSIX shell"
                                .to_owned(),
                        ]),
                );
                diags
            }
            CliUsageError::CantDetectFormat { path } => {
                vec![Diagnostic::error().with_message(format!(
                    "could not determine format of input file `{}`",
                    path.display()
                ))]
            }
            #[cfg(feature = "nix-experimental")]
            CliUsageError::NoNixConversion { path } => {
                vec![Diagnostic::error().with_message(format!(
                    "file {} is in nix format, but nix-to-nickel conversion is unsupported",
                    path.display()
                ))]
            }
        }
    }
}

#[derive(Clone, Debug)]
/// Warning emitted by the CLI.
pub enum Warning {
    /// The user queried a program without providing any path. In this case,
    /// querying won't show most information, and it's most probably not
    /// what the user wanted.
    EmptyQueryPath,

    /// A warning caused by doing something with a `Program` (e.g. parsing,
    /// evaluating, ...). The inner warning can reference source file locations,
    /// so it needs to be packaged with a files collection.
    Program {
        files: Files,
        warning: nickel_lang_core::error::Warning,
    },
}

impl Eq for Warning {}

// This impl only compares the content of the warnings and ignores the `Files`
// database for obvious performance reasons. Since this is only used for
// deduplicating warnings, the impact of a false answer is limited.
impl PartialEq for Warning {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Warning::EmptyQueryPath, Warning::EmptyQueryPath) => true,
            (Warning::Program { warning: a, .. }, Warning::Program { warning: b, .. }) => a == b,
            _ => false,
        }
    }
}

impl std::hash::Hash for Warning {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            Warning::Program { warning, .. } => warning.hash(state),
            Warning::EmptyQueryPath => {}
        }
    }
}

impl Warning {
    /// Report this warning on the standard error stream.
    pub fn report(self, format: ErrorFormat, color: ColorOpt) {
        use nickel_lang_core::error::report::report as core_report;
        match self {
            Warning::EmptyQueryPath => {
                let mut files = Files::empty();
                let diag = Diagnostic::warning()
                    .with_message("empty query path")
                    .with_notes(vec![
                        "You queried a value without requesting a specific field path. \
            This operation can't find any metadata, beside listing the fields of a record."
                            .into(),
                        "Try to query the root configuration and provide a query path instead."
                            .into(),
                        "For example, instead of querying the expression \
            `(import \"config.ncl\").module.input` with an empty path, query \
            `config.ncl` with the `module.input` path: \
            \n`nickel query config.ncl --field module.input"
                            .into(),
                    ]);
                core_report(&mut files, diag, format, color);
            }
            Warning::Program { mut files, warning } => {
                core_report(&mut files, warning, format, color)
            }
        }
    }
}

pub type CliResult<T> = Result<T, Error>;

impl From<std::io::Error> for Error {
    fn from(error: std::io::Error) -> Self {
        Error::Io { error }
    }
}

#[cfg(feature = "format")]
impl From<crate::format::FormatError> for Error {
    fn from(error: crate::format::FormatError) -> Self {
        Error::Format { error }
    }
}

#[cfg(feature = "repl")]
impl From<nickel_lang_core::repl::InitError> for Error {
    fn from(error: nickel_lang_core::repl::InitError) -> Self {
        Error::Repl { error }
    }
}

#[cfg(feature = "package-experimental")]
impl From<nickel_lang_package::error::Error> for Error {
    fn from(error: nickel_lang_package::error::Error) -> Self {
        Error::Package { error }
    }
}

// Report a standalone error which doesn't actually refer to any source code.
//
// Wrapping all errors in a diagnostic makes sure all errors are rendered using
// the same format set (potentially by default) by the `--error-format` flag.
// This also makes error styling more consistent.
fn report_standalone(main_label: &str, msg: Option<String>, format: ErrorFormat, color: ColorOpt) {
    let mut dummy_files = Files::empty();
    let diagnostic = Diagnostic::error()
        .with_message(main_label)
        .with_notes(msg.into_iter().collect());

    report(&mut dummy_files, diagnostic, format, color);
}

impl Error {
    /// Report this error on the standard error stream.
    pub fn report(self, format: ErrorFormat, color: ColorOpt) {
        use nickel_lang_core::error::report::report as core_report;

        let report_str = |main_label: &str| report_standalone(main_label, None, format, color);
        let report_with_msg =
            |main_label: &str, msg: String| report_standalone(main_label, Some(msg), format, color);

        match self {
            Error::Program { mut files, error } => core_report(&mut files, error, format, color),
            Error::Io { error } => report_with_msg("IO error", error.to_string()),
            #[cfg(feature = "repl")]
            Error::Repl { error } => {
                use nickel_lang_core::repl::InitError;
                match error {
                    InitError::Stdlib => report_str("failed to initialize the standard library"),
                    InitError::ReadlineError(msg) => {
                        report_with_msg("failed to initialize the terminal interface", msg)
                    }
                }
            }
            #[cfg(feature = "format")]
            Error::Format { error } => report_with_msg("format error", error.to_string()),
            Error::CliUsage { error, mut files } => core_report(&mut files, error, format, color),
            Error::FailedTests => report_str("tests failed"),
            #[cfg(feature = "package-experimental")]
            Error::NoManifest => report_str("failed to find a manifest file"),
            #[cfg(feature = "package-experimental")]
            Error::Package { error } => {
                if let nickel_lang_package::error::Error::ManifestEval {
                    package,
                    mut files,
                    error,
                } = error
                {
                    let msg = if let Some(package) = package {
                        format!("failed to evaluate manifest file for package {package}")
                    } else {
                        "failed to evaluate package manifest".to_owned()
                    };
                    report_str(&msg);
                    core_report(&mut files, *error, format, color);
                } else {
                    report_with_msg("packaging error", error.to_string())
                }
            }
        }
    }
}
