use std::{
    fmt::Display,
    fs::File,
    io::{stdin, stdout, BufReader, Read, Write},
    path::{Path, PathBuf},
};

use tempfile::NamedTempFile;
use topiary::TopiaryQuery;

use crate::{
    cli::{Files, GlobalOptions},
    error::CliResult,
};

#[derive(Debug)]
pub enum FormatError {
    NotAFile { path: PathBuf },
    TopiaryError(topiary::FormatterError),
}

impl From<topiary::FormatterError> for FormatError {
    fn from(e: topiary::FormatterError) -> Self {
        Self::TopiaryError(e)
    }
}

impl Display for FormatError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FormatError::NotAFile { path } => {
                write!(
                    f,
                    "The path {} does not refer to a file.",
                    path.to_string_lossy()
                )
            }
            FormatError::TopiaryError(e) => write!(f, "{e}"),
        }
    }
}

#[derive(Debug)]
pub enum Output {
    Stdout,
    Disk {
        staged: NamedTempFile,
        output: PathBuf,
    },
}

impl Output {
    pub fn new(path: Option<&Path>) -> CliResult<Self> {
        match path {
            None => Ok(Self::Stdout),
            Some(path) => {
                let path = nickel_lang_core::cache::normalize_path(path)?;
                Ok(Self::Disk {
                    staged: NamedTempFile::new_in(path.parent().ok_or_else(|| {
                        FormatError::NotAFile {
                            path: path.to_owned(),
                        }
                    })?)?,
                    output: path.to_owned(),
                })
            }
        }
    }

    pub fn persist(self) {
        if let Self::Disk { staged, output } = self {
            staged.persist(output).unwrap();
        }
    }
}

impl Write for Output {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        match self {
            Output::Stdout => stdout().write(buf),
            Output::Disk { staged, output: _ } => staged.write(buf),
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        match self {
            Output::Stdout => stdout().flush(),
            Output::Disk { staged, output: _ } => staged.flush(),
        }
    }
}

#[derive(clap::Parser, Debug)]
pub struct FormatOptions {
    #[command(flatten)]
    sources: Files,
}

impl FormatOptions {
    pub fn run(self, _: GlobalOptions) -> CliResult<()> {
        let mut output: Output = Output::new(self.sources.file.as_deref())?;
        let mut input: Box<dyn Read> = match self.sources.file {
            None => Box::new(stdin()),
            Some(f) => Box::new(BufReader::new(File::open(f)?)),
        };
        let topiary_config =
            topiary::Configuration::parse_default_configuration().map_err(FormatError::from)?;
        let language = topiary::SupportedLanguage::Nickel.to_language(&topiary_config);
        let grammar = tree_sitter_nickel::language().into();
        topiary::formatter(
            &mut input,
            &mut output,
            &TopiaryQuery::nickel(),
            language,
            &grammar,
            topiary::Operation::Format {
                skip_idempotence: true,
                tolerate_parsing_errors: true,
            },
        )
        .map_err(FormatError::from)?;
        output.persist();
        Ok(())
    }
}
