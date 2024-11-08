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
#[derive(Copy, Clone, Eq, PartialEq, Debug, Default)]
#[cfg_attr(feature = "clap", derive(clap::ValueEnum))]
pub enum ErrorFormat {
    #[default]
    Text,
    Json,
    Yaml,
    Toml,
}

pub type ColorOpt = colorchoice::ColorChoice;

fn colors_for_terminal(color_opt: ColorOpt, is_terminal: bool) -> ColorChoice {
    match color_opt {
        colorchoice::ColorChoice::Auto => {
            if is_terminal {
                ColorChoice::Auto
            } else {
                ColorChoice::Never
            }
        }
        colorchoice::ColorChoice::Always => ColorChoice::Always,
        colorchoice::ColorChoice::AlwaysAnsi => ColorChoice::AlwaysAnsi,
        colorchoice::ColorChoice::Never => ColorChoice::Never,
    }
}

/// Pretty-print an error on stderr.
///
/// # Arguments
///
/// - `cache` is the file cache used during the evaluation, which is required by the reporting
///   infrastructure to point at specific locations and print snippets when needed.
pub fn report<E: IntoDiagnostics>(
    cache: &mut Cache,
    error: E,
    format: ErrorFormat,
    color_opt: ColorOpt,
) {
    use std::io::{stderr, IsTerminal};

    report_with(
        &mut StandardStream::stderr(colors_for_terminal(color_opt, stderr().is_terminal())).lock(),
        &mut cache.files().clone(),
        error,
        format,
    )
}

/// Pretty-print an error on stdout.
///
/// # Arguments
///
/// - `cache` is the file cache used during the evaluation, which is required by the reporting
///   infrastructure to point at specific locations and print snippets when needed.
pub fn report_to_stdout<E: IntoDiagnostics>(
    cache: &mut Cache,
    error: E,
    format: ErrorFormat,
    color_opt: ColorOpt,
) {
    use std::io::{stdout, IsTerminal};

    report_with(
        &mut StandardStream::stdout(colors_for_terminal(color_opt, stdout().is_terminal())).lock(),
        &mut cache.files().clone(),
        error,
        format,
    )
}

/// Report an error on `stderr`, provided a file database and a list of stdlib file ids.
pub fn report_with<E: IntoDiagnostics>(
    writer: &mut dyn WriteColor,
    files: &mut Files,
    error: E,
    format: ErrorFormat,
) {
    let config = codespan_reporting::term::Config::default();
    let diagnostics = error.into_diagnostics(files);
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
