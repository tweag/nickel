use std::{
    fmt::Display,
    fs::File,
    io::{self, stdin, stdout, BufReader, Read, Write},
    path::{Path, PathBuf},
    process,
};

use tempfile::NamedTempFile;
use topiary::TopiaryQuery;

#[derive(Debug)]
pub enum FormatError {
    NotAFile { path: PathBuf },
    TopiaryError(topiary::FormatterError),
    IOError(io::Error),
}

impl From<io::Error> for FormatError {
    fn from(e: io::Error) -> Self {
        Self::IOError(e)
    }
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
            FormatError::IOError(e) => write!(f, "{e}"),
        }
    }
}

pub enum Output {
    Stdout,
    Disk {
        staged: NamedTempFile,
        output: PathBuf,
    },
}

impl Output {
    pub fn new(path: Option<&Path>) -> Result<Self, FormatError> {
        match path {
            None => Ok(Self::Stdout),
            Some(path) => {
                let path = path.canonicalize()?;
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

pub fn format(input: Option<&Path>, output: Option<&Path>, in_place: bool) {
    if let Err(e) = do_format(input, output, in_place) {
        eprintln!("{e}");
        process::exit(1);
    }
}

fn do_format(
    input: Option<&Path>,
    output: Option<&Path>,
    in_place: bool,
) -> Result<(), FormatError> {
    let mut output: Output = match (output, input, in_place) {
        (None, None, _) | (None, Some(_), false) => Output::new(None)?,
        (None, Some(file), true) | (Some(file), _, _) => Output::new(Some(file))?,
    };
    let mut input: Box<dyn Read> = match input {
        None => Box::new(stdin()),
        Some(f) => Box::new(BufReader::new(File::open(f)?)),
    };
    let topiary_config = topiary::Configuration::parse_default_configuration()?;
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
    )?;
    output.persist();
    Ok(())
}
