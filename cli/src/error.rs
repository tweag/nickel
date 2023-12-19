//! Error handling for the CLI.

use nickel_lang_core::{
    error::{
        report::{ColorOpt, ErrorFormat},
        Diagnostic, FileId, Files, IntoDiagnostics, ParseError,
    },
    eval::cache::lazy::CBNCache,
    program::{FieldOverride, FieldPath, Program},
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
}

pub enum Error {
    Program {
        program: Program<CBNCache>,
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
        program: Program<CBNCache>,
        error: CliUsageError,
    },
    /// Not an actual failure but a special early return to indicate that information was printed
    /// during the usage of the customize mode, because a subcommand such as `list`, `show`, etc.
    /// was used, and thus no customized program can be returned.
    ///
    /// Upon receiving this error, the caller should simply exit without proceeding with evaluation.
    CustomizeInfoPrinted,
}

impl IntoDiagnostics<FileId> for CliUsageError {
    fn into_diagnostics(
        self,
        files: &mut Files<String>,
        stdlib_ids: Option<&Vec<FileId>>,
    ) -> Vec<Diagnostic<FileId>> {
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

            vec![Diagnostic::error()
                .with_message(format!(
                    "invalid {method}: unknown field `{path}`",
                    path = data.path
                ))
                .with_notes(notes)]
        }

        match self {
            CliUsageError::UnknownFieldOverride(data) => mk_unknown_diags(data, "override"),
            CliUsageError::UnknownFieldAssignment(data) => mk_unknown_diags(data, "assignment"),
            CliUsageError::CantAssignNonInput {
                ovd: FieldOverride { path, value, .. },
            } => {
                vec![Diagnostic::error()
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
                    ])]
            }
            CliUsageError::AssignmentParseError { error } => {
                let mut diags = IntoDiagnostics::into_diagnostics(error, files, stdlib_ids);
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
                let mut diags = IntoDiagnostics::into_diagnostics(error, files, stdlib_ids);
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
        }
    }
}

/// Warning emitted by the CLI.
pub enum Warning {
    /// The user queried a program without providing any path. In this case,
    /// querying won't show most information, and it's most probably not
    /// what the user wanted.
    EmptyQueryPath,
}

impl<FileId> IntoDiagnostics<FileId> for Warning {
    fn into_diagnostics(
        self,
        _files: &mut Files<String>,
        _stdlib_ids: Option<&Vec<FileId>>,
    ) -> Vec<Diagnostic<FileId>> {
        vec![Diagnostic::warning()
            .with_message("empty query path")
            .with_notes(vec![
                "You queried a value without requesting a specific field path. \
            This operation can't find any metadata, beside listing the fields of a record."
                    .into(),
                "Try to query the root configuration and provide a query path instead.".into(),
                "For example, instead of querying the expression \
            `(import \"config.ncl\").module.input` with an empty path, query \
            `config.ncl` with the `module.input` path: \
            \n`nickel query config.ncl --field module.input"
                    .into(),
            ])]
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

pub trait ResultErrorExt<T> {
    fn report_with_program(self, program: Program<CBNCache>) -> CliResult<T>;
}

impl<T> ResultErrorExt<T> for Result<T, nickel_lang_core::error::Error> {
    fn report_with_program(self, program: Program<CBNCache>) -> CliResult<T> {
        self.map_err(|error| Error::Program { program, error })
    }
}

impl Error {
    /// Report this error on the standard error stream.
    pub fn report(self, format: ErrorFormat, color: ColorOpt) {
        // Report a standalone error which doesn't actually refer to any source code.
        let report_standalone = |main_label: &str, msg: Option<String>| {
            use nickel_lang_core::{
                cache::{Cache, ErrorTolerance},
                error::report::report as core_report,
            };

            let mut dummy_cache = Cache::new(ErrorTolerance::Tolerant);
            let diagnostic = Diagnostic::error()
                .with_message(main_label)
                .with_notes(msg.into_iter().collect());

            core_report(&mut dummy_cache, diagnostic, format, color);
        };

        // We try to fit every error in a diagnostic. This makes sure all errors are rendered using
        // the same format set (potentitally by default) by the `--error-format` flag. This also
        // makes error styling more consistent.
        match self {
            Error::Program { mut program, error } => program.report(error, format),
            Error::Io { error } => {
                report_standalone("IO error", Some(error.to_string()));
            }
            #[cfg(feature = "repl")]
            Error::Repl { error } => {
                use nickel_lang_core::repl::InitError;
                match error {
                    InitError::Stdlib => {
                        report_standalone("failed to initialize the standard library", None)
                    }
                    InitError::ReadlineError(msg) => {
                        report_standalone("failed to initialize the terminal interface", Some(msg))
                    }
                }
            }
            #[cfg(feature = "format")]
            Error::Format { error } => report_standalone("format error", Some(error.to_string())),
            Error::CliUsage { error, mut program } => program.report(error, format),
            Error::CustomizeInfoPrinted => {
                // Nothing to do, the caller should simply exit.
            }
        }
    }
}
