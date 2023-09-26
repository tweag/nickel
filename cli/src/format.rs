use std::{
    fmt::Display,
    fs::File,
    io::{stdin, stdout, BufReader, Read, Write},
    path::{Path, PathBuf},
};

use tempfile::NamedTempFile;

use crate::{
    cli::GlobalOptions, customize::NoCustomizeMode, error::CliResult, input::InputOptions,
};

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
    #[command(flatten)]
    input: InputOptions<NoCustomizeMode>,
}

impl FormatCommand {
    pub fn run(self, _global: GlobalOptions) -> CliResult<()> {
        fn format(input: impl Read, mut output: Output) -> CliResult<()> {
            nickel_lang_core::format::format(input, &mut output)
                .map_err(FormatError::FormatError)?;
            output.persist();
            Ok(())
        }
        if self.input.files.is_empty() {
            return format(stdin(), Output::Stdout);
        }

        for file in self.input.files.iter() {
            format(BufReader::new(File::open(file)?), Output::from_path(file)?)?;
        }
        Ok(())
    }
}
