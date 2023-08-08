use std::{
    fmt, fs,
    path::{Path, PathBuf},
};

use nickel_lang_core::{
    error::{Error, IOError},
    eval::cache::CacheImpl,
    program::Program,
};

use crate::{
    cli::GlobalOptions,
    error::{CliResult, ResultErrorExt},
    eval,
};

#[derive(Copy, Clone, Eq, PartialEq, Debug, Default, clap::ValueEnum)]
pub enum DocFormat {
    Json,
    #[default]
    Markdown,
}

impl DocFormat {
    pub fn extension(&self) -> &'static str {
        match self {
            Self::Json => "json",
            Self::Markdown => "md",
        }
    }
}

impl fmt::Display for DocFormat {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Json => write!(f, "json"),
            Self::Markdown => write!(f, "markdown"),
        }
    }
}

#[derive(clap::Parser, Debug)]
pub struct DocCommand {
    /// The path of the generated documentation file. Default to
    /// `~/.nickel/doc/<input-file>.md` for input `<input-file>.ncl`, or to
    /// `~/.nickel/doc/out.md` if the input is read from stdin.
    #[arg(short, long)]
    pub output: Option<PathBuf>,
    /// Write documentation to stdout. Takes precedence over `output`
    #[arg(long)]
    pub stdout: bool,
    /// The output format for the generated documentation.
    #[arg(long, value_enum, default_value_t)]
    pub format: crate::doc::DocFormat,
}

impl DocCommand {
    pub fn run(self, global: GlobalOptions) -> CliResult<()> {
        let mut program = eval::prepare(&global)?;
        self.export_doc(&mut program, &global)
            .report_with_program(program)
    }

    fn export_doc(
        self,
        program: &mut Program<CacheImpl>,
        global: &GlobalOptions,
    ) -> Result<(), Error> {
        let doc = program.extract_doc()?;
        let mut out: Box<dyn std::io::Write> = if self.stdout {
            Box::new(std::io::stdout())
        } else {
            Box::new(
                self.output
                    .as_ref()
                    .map(|output| {
                        fs::File::create(output.clone()).map_err(|e| {
                            Error::IOError(IOError(format!(
                                "when opening or creating output file `{}`: {}",
                                output.to_string_lossy(),
                                e
                            )))
                        })
                    })
                    .unwrap_or_else(|| {
                        let docpath = Path::new(".nickel/doc/");
                        fs::create_dir_all(docpath).map_err(|e| {
                            Error::IOError(IOError(format!(
                                "when creating output path `{}`: {}",
                                docpath.to_string_lossy(),
                                e
                            )))
                        })?;
                        let mut output_file = docpath.to_path_buf();

                        let mut has_file_name = false;

                        if let Some(path) = &global.file {
                            if let Some(file_stem) = path.file_stem() {
                                output_file.push(file_stem);
                                has_file_name = true;
                            }
                        }

                        if !has_file_name {
                            output_file.push("out");
                        }

                        output_file.set_extension(self.format.extension());
                        fs::File::create(output_file.clone().into_os_string()).map_err(|e| {
                            Error::IOError(IOError(format!(
                                "when opening or creating output file `{}`: {}",
                                output_file.to_string_lossy(),
                                e
                            )))
                        })
                    })?,
            )
        };
        match self.format {
            DocFormat::Json => doc.write_json(&mut out),
            DocFormat::Markdown => doc.write_markdown(&mut out),
        }
    }
}
