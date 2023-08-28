use std::{
    fmt::Display,
    fs::File,
    io::{stdin, stdout, BufReader, Read, Write},
    path::{Path, PathBuf},
};

use tempfile::NamedTempFile;

use crate::{cli::GlobalOptions, error::CliResult};

#[derive(Debug)]
pub enum FormatError {
    NotAFile { path: PathBuf },
    FormatError(nickel_lang_core::format::FormatError),
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
            FormatError::FormatError(e) => e.fmt(f),
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
    pub fn from_path(path: &Path) -> CliResult<Self> {
        let path = nickel_lang_core::cache::normalize_path(path)?;
        Ok(Self::Disk {
            staged: NamedTempFile::new_in(path.parent().ok_or_else(|| FormatError::NotAFile {
                path: path.to_owned(),
            })?)?,
            output: path.to_owned(),
        })
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
pub struct FormatCommand {
    /// Output file. Standard output by default.
    #[arg(short, long)]
    output: Option<PathBuf>,

    /// Format in place, overwriting the input file.
    #[arg(short, long, requires = "file")]
    in_place: bool,
}

impl FormatCommand {
    pub fn run(self, global: GlobalOptions) -> CliResult<()> {
        let mut output: Output = match (&self.output, &global.file, self.in_place) {
            (None, None, _) | (None, Some(_), false) => Output::Stdout,
            (None, Some(file), true) | (Some(file), _, _) => Output::from_path(file)?,
        };
        let input: Box<dyn Read> = match global.nickel_file()? {
            None => Box::new(stdin()),
            Some(f) => Box::new(BufReader::new(File::open(f)?)),
        };
        nickel_lang_core::format::format(input, &mut output).map_err(FormatError::FormatError)?;
        output.persist();
        Ok(())
    }
}
