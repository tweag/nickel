use std::{
    fmt::Display,
    fs::File,
    io::{stdin, stdout, BufReader, Read, Write},
    path::{Path, PathBuf},
};

use tempfile::NamedTempFile;

use crate::{
    customize::NoCustomizeMode, error::CliResult, global::GlobalContext, input::InputOptions,
};

#[derive(Debug)]
pub enum FormatError {
    NotAFile {
        path: PathBuf,
    },
    FailedToFormat(nickel_lang_core::format::FormatError),
    /// This file needs formatting
    BadFormat(PathBuf),
    /// Data provided on stdin is badly formatted
    BadFormatStdin,
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
            FormatError::FailedToFormat(e) => e.fmt(f),
            FormatError::BadFormat(path) => {
                write!(
                    f,
                    "Bad format: {path:?}, run `nickel format {path:?}` to fix"
                )
            }
            FormatError::BadFormatStdin => {
                write!(f, "Bad format on stdin")
            }
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

    /// Do not change the input files, instead return non-zero if any of the input file is badly formatted
    #[arg(long)]
    check: bool,
}

impl FormatCommand {
    pub fn run(self, ctxt: &mut GlobalContext) {
        fn format(input: impl Read, mut output: Output) -> CliResult<()> {
            nickel_lang_core::format::format(input, &mut output)
                .map_err(FormatError::FailedToFormat)?;
            output.persist();
            Ok(())
        }

        fn format_path(path: &Path) -> CliResult<()> {
            format(BufReader::new(File::open(path)?), Output::from_path(path)?)
        }

        fn check(mut input: impl Read, on_fmt_error: FormatError) -> CliResult<()> {
            let mut input_buf = vec![];
            input.read_to_end(&mut input_buf)?;
            let cursor = std::io::Cursor::new(&input_buf);
            let mut output = vec![];
            nickel_lang_core::format::format(cursor, &mut output)
                .map_err(FormatError::FailedToFormat)?;
            if input_buf != output {
                Err(on_fmt_error)?
            }
            Ok(())
        }

        fn check_path(path: &Path) -> CliResult<()> {
            check(
                BufReader::new(File::open(path)?),
                FormatError::BadFormat(path.into()),
            )
        }

        if self.input.files.is_empty() {
            if !self.check {
                ctxt.reporter.report_result(format(stdin(), Output::Stdout));
            } else {
                ctxt.reporter
                    .report_result(check(stdin(), FormatError::BadFormatStdin));
            }
        } else if !self.check {
            for file in self.input.files.iter() {
                ctxt.reporter.report_result(format_path(file));
            }
        } else {
            for file in self.input.files.iter() {
                ctxt.reporter.report_result(check_path(file));
            }
        }
    }
}
