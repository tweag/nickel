use nickel_lang_core::{
    error::{Diagnostic, Files, IntoDiagnostics},
    eval::cache::lazy::CBNCache,
    program::Program,
};

/// Errors related to mishandling the CLI.
pub enum CliUsageError {
    /// Tried to override a field which doesn't exist.
    UnknownFieldOverride { path: String },
    /// Tried to assign a field which doesn't exist.
    UnknownFieldAssignment { path: String },
    /// Tried to override an defined field without the `--override` argument.
    CantAssignNonInput { path: String, assignment: String },
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
}

impl<FileId> IntoDiagnostics<FileId> for CliUsageError {
    fn into_diagnostics(
        self,
        _files: &mut Files<String>,
        _stdlib_ids: Option<&Vec<FileId>>,
    ) -> Vec<Diagnostic<FileId>> {
        fn mk_unknown_diags<FileId>(path: String, method: &str) -> Vec<Diagnostic<FileId>> {
            vec![Diagnostic::error()
                .with_message(format!("invalid {method}: unknown field `{path}`"))
                .with_notes(vec![format!(
                    "`{path}` doesn't refer to record field accessible from the root of the \
                        configuration."
                )])]
        }

        match self {
            CliUsageError::UnknownFieldOverride { path } => mk_unknown_diags(path, "override"),
            CliUsageError::UnknownFieldAssignment { path } => mk_unknown_diags(path, "assignment"),
            CliUsageError::CantAssignNonInput { path, assignment } => {
                vec![Diagnostic::error()
                    .with_message(format!("invalid assignment: `{path}` isn't an input"))
                    .with_notes(vec![
                        format!(
                            "`{path}` already has a value and thus can't be assigned \
                            without `--override`."
                        ),
                        format!(
                            "If you really want to override this field, please use \
                            `--override {assignment}` instead."
                        ),
                    ])]
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
            \n`nickel query module.input -f config.ncl`"
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
    pub fn report(self) {
        match self {
            Error::Program { mut program, error } => program.report(error),
            Error::Io { error } => {
                eprintln!("{error}")
            }
            #[cfg(feature = "repl")]
            Error::Repl { error } => {
                use nickel_lang_core::repl::InitError;
                match error {
                    InitError::Stdlib => eprintln!("Failed to load the Nickel standard library"),
                    InitError::ReadlineError(msg) => {
                        eprintln!("Readline intialization failed: {msg}")
                    }
                }
            }
            #[cfg(feature = "format")]
            Error::Format { error } => eprintln!("{error}"),
            Error::CliUsage { error, mut program } => program.report(error),
        }
    }
}
