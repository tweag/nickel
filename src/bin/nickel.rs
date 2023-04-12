//! Entry point of the program.
use core::fmt;
use nickel_lang::error::{Error, IOError};
use nickel_lang::eval::cache::CacheImpl;
use nickel_lang::program::{ColorOpt, Program};
use nickel_lang::repl::query_print;
#[cfg(feature = "repl")]
use nickel_lang::repl::rustyline_frontend;
use nickel_lang::term::{RichTerm, Term};
use nickel_lang::{serialize, serialize::ExportFormat};
use std::path::{Path, PathBuf};
use std::{
    fs::{self, File},
    io::Write,
    process,
};
// use std::ffi::OsStr;
use directories::BaseDirs;
use structopt::StructOpt;

/// Command-line options and subcommands.
#[derive(StructOpt, Debug)]
/// The interpreter of the Nickel language.
struct Opt {
    /// The input file. Standard input by default
    #[structopt(short = "f", long, global = true, parse(from_os_str))]
    file: Option<PathBuf>,

    #[cfg(debug_assertions)]
    /// Skips the standard library import. For debugging only. This does not affect REPL
    #[structopt(long)]
    nostdlib: bool,

    /// Coloring: auto, always, never.
    #[structopt(long, global = true, case_insensitive = true, default_value = "auto")]
    color: ColorOpt,

    #[structopt(subcommand)]
    command: Option<Command>,
}

/// Available subcommands.
#[derive(StructOpt, Debug)]
enum Command {
    /// Converts the parsed representation (AST) back to Nickel source code and prints it. Used for
    /// debugging purpose
    PprintAst {
        /// Performs code transformations before printing
        #[structopt(long)]
        transform: bool,
    },
    /// Exports the result to a different format
    Export {
        /// Available formats: `raw, json, yaml, toml`. Default format: `json`.
        #[structopt(long)]
        format: Option<ExportFormat>,
        /// Output file. Standard output by default
        #[structopt(short = "o", long)]
        #[structopt(parse(from_os_str))]
        output: Option<PathBuf>,
    },
    /// Prints the metadata attached to an attribute, given as a path
    Query {
        path: Option<String>,
        #[structopt(long)]
        doc: bool,
        #[structopt(long)]
        contract: bool,
        #[structopt(long = "type")]
        types: bool,
        #[structopt(long)]
        default: bool,
        #[structopt(long)]
        value: bool,
    },
    /// Typechecks the program but do not run it
    Typecheck,
    /// Starts an REPL session
    Repl {
        #[structopt(long)]
        history_file: Option<PathBuf>,
    },
    /// Generates the documentation files for the specified nickel file
    #[cfg(feature = "doc")]
    Doc {
        /// The path of the generated documentation file. Default to
        /// `~/.nickel/doc/<input-file>.md` for input `<input-file>.ncl`, or to
        /// `~/.nickel/doc/out.md` if the input is read from stdin.
        #[structopt(short = "o", long, parse(from_os_str))]
        output: Option<PathBuf>,
        /// Write documentation to stdout. Takes precedence over `output`
        #[structopt(long)]
        stdout: bool,
        /// The output format for the generated documentation. Possible values:
        /// markdown, json
        #[structopt(long, default_value = "markdown")]
        format: DocFormat,
    },
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Default)]
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

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct ParseFormatError(String);

impl fmt::Display for ParseFormatError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "unsupported export format {}", self.0)
    }
}

impl std::str::FromStr for DocFormat {
    type Err = ParseFormatError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_ref() {
            "json" => Ok(DocFormat::Json),
            "markdown" => Ok(DocFormat::Markdown),
            _ => Err(ParseFormatError(String::from(s))),
        }
    }
}

fn main() {
    let opts = Opt::from_args();

    if let Some(Command::Repl { history_file }) = opts.command {
        let histfile = if let Some(h) = history_file {
            h
        } else {
            BaseDirs::new()
                .expect("Cannot retrieve home directory path")
                .home_dir()
                .join(".nickel_history")
        };
        #[cfg(feature = "repl")]
        if rustyline_frontend::repl(histfile, opts.color).is_err() {
            process::exit(1);
        }

        #[cfg(not(feature = "repl"))]
        eprintln!("error: this executable was not compiled with REPL support");
    } else {
        let mut program = opts
            .file
            .clone()
            .map(Program::new_from_file)
            .unwrap_or_else(Program::new_from_stdin)
            .unwrap_or_else(|err| {
                eprintln!("Error when reading input: {err}");
                process::exit(1)
            });

        #[cfg(debug_assertions)]
        if opts.nostdlib {
            program.set_skip_stdlib();
        }

        program.set_color(opts.color);

        let result = match opts.command {
            Some(Command::PprintAst { transform }) => program.pprint_ast(
                &mut std::io::BufWriter::new(Box::new(std::io::stdout())),
                transform,
            ),
            Some(Command::Export { format, output }) => export(&mut program, format, output),
            Some(Command::Query {
                path,
                doc,
                contract,
                types,
                default,
                value,
            }) => {
                program.query(path).map(|term| {
                    // Print a default selection of attributes if no option is specified
                    let attrs = if !doc && !contract && !types && !default && !value {
                        query_print::Attributes::default()
                    } else {
                        query_print::Attributes {
                            doc,
                            contract,
                            types,
                            default,
                            value,
                        }
                    };

                    query_print::write_query_result(&mut std::io::stdout(), &term, attrs).unwrap()
                })
            }
            Some(Command::Typecheck) => program.typecheck(),
            Some(Command::Repl { .. }) => unreachable!(),
            #[cfg(feature = "doc")]
            Some(Command::Doc {
                output,
                stdout,
                format,
            }) => export_doc(&mut program, opts.file.as_ref(), output, stdout, format),
            None => program
                .eval_full()
                .map(|t| println!("{}", Term::from(t).deep_repr())),
        };

        if let Err(err) = result {
            program.report(err);
            process::exit(1)
        }
    }
}

fn export(
    program: &mut Program<CacheImpl>,
    format: Option<ExportFormat>,
    output: Option<PathBuf>,
) -> Result<(), Error> {
    let rt = program.eval_full_for_export().map(RichTerm::from)?;
    let format = format.unwrap_or_default();

    // We only add a trailing newline for JSON exports. Both YAML and TOML
    // exporters already append a trailing newline by default.
    let trailing_newline = format == ExportFormat::Json;

    serialize::validate(format, &rt)?;

    if let Some(file) = output {
        let mut file = fs::File::create(file).map_err(IOError::from)?;
        serialize::to_writer(&mut file, format, &rt)?;

        if trailing_newline {
            writeln!(file).map_err(IOError::from)?;
        }
    } else {
        serialize::to_writer(std::io::stdout(), format, &rt)?;

        if trailing_newline {
            println!();
        }
    }

    Ok(())
}

#[cfg(feature = "doc")]
fn export_doc(
    program: &mut Program<CacheImpl>,
    file: Option<&PathBuf>,
    output: Option<PathBuf>,
    stdout: bool,
    format: DocFormat,
) -> Result<(), Error> {
    let mut out: Box<dyn std::io::Write> = if stdout {
        Box::new(std::io::stdout())
    } else {
        Box::new(
            output
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

                    if let Some(path) = file {
                        if let Some(file_stem) = path.file_stem() {
                            output_file.push(file_stem);
                            has_file_name = true;
                        }
                    }

                    if !has_file_name {
                        output_file.push("out");
                    }

                    output_file.set_extension(format.extension());
                    File::create(output_file.clone().into_os_string()).map_err(|e| {
                        Error::IOError(IOError(format!(
                            "when opening or creating output file `{}`: {}",
                            output_file.to_string_lossy(),
                            e
                        )))
                    })
                })?,
        )
    };
    let doc = program.extract_doc()?;
    match format {
        DocFormat::Json => doc.write_json(&mut out),
        DocFormat::Markdown => doc.write_markdown(&mut out),
    }
}
