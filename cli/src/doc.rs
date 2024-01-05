use std::{
    borrow::Cow,
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
    customize::ExtractFieldOnly,
    error::{CliResult, ResultErrorExt},
    input::{InputOptions, Prepare},
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

    #[command(flatten)]
    pub input: InputOptions<ExtractFieldOnly>,
}

const DEFAULT_OUT_DIR: &str = ".nickel/doc/";

impl DocCommand {
    pub fn run(self, global: GlobalOptions) -> CliResult<()> {
        let mut program = self.input.prepare(&global)?;
        self.export_doc(&mut program).report_with_program(program)
    }

    fn export_doc(self, program: &mut Program<CacheImpl>) -> Result<(), Error> {
        let doc = program.extract_doc()?;

        let (mut out, out_path): (Box<dyn std::io::Write>, Option<Cow<'_, str>>) = if self.stdout {
            (Box::new(std::io::stdout()), None)
        } else {
            self.output
                .as_ref()
                .map(|output| -> Result<(Box<dyn std::io::Write>, _), Error> {
                    let out = Box::new(fs::File::create(output.clone()).map_err(|e| {
                        Error::IOError(IOError(format!(
                            "when opening or creating output file `{}`: {}",
                            output.to_string_lossy(),
                            e
                        )))
                    })?);

                    Ok((out, Some(output.to_string_lossy())))
                })
                .unwrap_or_else(|| {
                    let docpath = Path::new(DEFAULT_OUT_DIR);

                    fs::create_dir_all(docpath).map_err(|e| {
                        Error::IOError(IOError(format!(
                            "when creating output path `{}`: {}",
                            docpath.to_string_lossy(),
                            e
                        )))
                    })?;

                    let mut output_file = docpath.to_path_buf();

                    let mut has_file_name = false;

                    if let Some(path) = self.input.files.first() {
                        if let Some(file_stem) = path.file_stem() {
                            output_file.push(file_stem);
                            has_file_name = true;
                        }
                    }

                    if !has_file_name {
                        output_file.push("out");
                    }

                    output_file.set_extension(self.format.extension());

                    let out =
                        fs::File::create(output_file.clone().into_os_string()).map_err(|e| {
                            Error::IOError(IOError(format!(
                                "when opening or creating output file `{}`: {}",
                                output_file.to_string_lossy(),
                                e
                            )))
                        })?;

                    Ok((
                        Box::new(out),
                        Some(Cow::Owned(output_file.to_string_lossy().into_owned())),
                    ))
                })?
        };

        match self.format {
            DocFormat::Json => doc.write_json(&mut out),
            DocFormat::Markdown => doc.write_markdown(&mut out),
        }?;

        if let Some(out_path) = out_path {
            eprintln!("Documentation written to {}", out_path.as_ref());
        }

        Ok(())
    }
}
