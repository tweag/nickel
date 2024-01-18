//! Error diagnostics reporting and serialization.
use super::*;

/// Serializable wrapper type to export diagnostics with a top-level attribute.
#[derive(serde::Serialize)]
pub struct DiagnosticsWrapper {
    pub diagnostics: Vec<Diagnostic<FileId>>,
}

impl From<Vec<Diagnostic<FileId>>> for DiagnosticsWrapper {
    fn from(diagnostics: Vec<Diagnostic<FileId>>) -> Self {
        Self { diagnostics }
    }
}

/// Available export formats for error diagnostics.
#[derive(Copy, Clone, Eq, PartialEq, Debug, Default, clap::ValueEnum)]
pub enum ErrorFormat {
    #[default]
    Text,
    Json,
    Yaml,
    Toml,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct ColorOpt(pub(crate) clap::ColorChoice);

impl ColorOpt {
    fn for_terminal(self, is_terminal: bool) -> ColorChoice {
        match self.0 {
            clap::ColorChoice::Auto => {
                if is_terminal {
                    ColorChoice::Auto
                } else {
                    ColorChoice::Never
                }
            }
            clap::ColorChoice::Always => ColorChoice::Always,
            clap::ColorChoice::Never => ColorChoice::Never,
        }
    }
}

impl From<clap::ColorChoice> for ColorOpt {
    fn from(color_choice: clap::ColorChoice) -> Self {
        Self(color_choice)
    }
}

impl Default for ColorOpt {
    fn default() -> Self {
        Self(clap::ColorChoice::Auto)
    }
}

/// Pretty-print an error on stderr.
///
/// # Arguments
///
/// - `cache` is the file cache used during the evaluation, which is required by the reporting
/// infrastructure to point at specific locations and print snippets when needed.
pub fn report<E: IntoDiagnostics<FileId>>(
    cache: &mut Cache,
    error: E,
    format: ErrorFormat,
    color_opt: ColorOpt,
) {
    use std::io::{stderr, IsTerminal};

    let stdlib_ids = cache.get_all_stdlib_modules_file_id();
    report_with(
        &mut StandardStream::stderr(color_opt.for_terminal(stderr().is_terminal())).lock(),
        cache.files_mut(),
        stdlib_ids.as_ref(),
        error,
        format,
    )
}

/// Report an error on `stderr`, provided a file database and a list of stdlib file ids.
pub fn report_with<E: IntoDiagnostics<FileId>>(
    writer: &mut dyn WriteColor,
    files: &mut Files<String>,
    stdlib_ids: Option<&Vec<FileId>>,
    error: E,
    format: ErrorFormat,
) {
    let config = codespan_reporting::term::Config::default();
    let diagnostics = error.into_diagnostics(files, stdlib_ids);
    let stderr = std::io::stderr();

    let result = match format {
        ErrorFormat::Text => diagnostics.iter().try_for_each(|d| {
            codespan_reporting::term::emit(writer, &config, files, d).map_err(|err| err.to_string())
        }),
        ErrorFormat::Json => serde_json::to_writer(stderr, &DiagnosticsWrapper::from(diagnostics))
            .map(|_| eprintln!())
            .map_err(|err| err.to_string()),
        ErrorFormat::Yaml => serde_yaml::to_writer(stderr, &DiagnosticsWrapper::from(diagnostics))
            .map_err(|err| err.to_string()),
        ErrorFormat::Toml => toml::to_string(&DiagnosticsWrapper::from(diagnostics))
            .map(|repr| eprint!("{}", repr))
            .map_err(|err| err.to_string()),
    };

    match result {
        Ok(()) => (),
        Err(err) => panic!("error::report_with(): could not print an error on stderr: {err}"),
    };
}
