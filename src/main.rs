//! Entry point of the program.
mod cache;
mod error;
mod eval;
mod identifier;
mod label;
mod merge;
mod operation;
mod parser;
mod position;
mod program;
mod repl;
mod serialize;
mod stack;
mod stdlib;
mod term;
mod transformations;
mod typecheck;
mod types;

use crate::error::{Error, IOError, SerializationError};
use crate::program::Program;
use crate::repl::rustyline_frontend;
use crate::term::{RichTerm, Term};
use std::io::Write;
use std::path::PathBuf;
use std::str::FromStr;
use std::{fmt, fs, io, process};
// use std::ffi::OsStr;
use structopt::StructOpt;

extern crate either;

/// Command-line options and subcommands.
#[derive(StructOpt, Debug)]
/// The interpreter of the Nickel language.
struct Opt {
    /// The input file. Standard input by default
    #[structopt(short = "f", long)]
    #[structopt(parse(from_os_str))]
    file: Option<PathBuf>,
    #[structopt(subcommand)]
    command: Option<Command>,
}

/// Available export formats.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum ExportFormat {
    Raw,
    Json,
    Yaml,
    Toml,
}

impl std::default::Default for ExportFormat {
    fn default() -> Self {
        ExportFormat::Json
    }
}

impl fmt::Display for ExportFormat {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Raw => write!(f, "raw"),
            Self::Json => write!(f, "json"),
            Self::Yaml => write!(f, "yaml"),
            Self::Toml => write!(f, "toml"),
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

impl FromStr for ExportFormat {
    type Err = ParseFormatError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_ref() {
            "raw" => Ok(ExportFormat::Raw),
            "json" => Ok(ExportFormat::Json),
            "yaml" => Ok(ExportFormat::Yaml),
            "toml" => Ok(ExportFormat::Toml),
            _ => Err(ParseFormatError(String::from(s))),
        }
    }
}

/// Available subcommands.
#[derive(StructOpt, Debug)]
enum Command {
    /// Export the result to a different format
    Export {
        /// Available formats: `raw, json`. Default format: `json`.
        #[structopt(long)]
        format: Option<ExportFormat>,
        /// Output file. Standard output by default
        #[structopt(short = "o", long)]
        #[structopt(parse(from_os_str))]
        output: Option<PathBuf>,
    },
    /// Print the metadata attached to an attribute, given as a path
    Query {
        path: Option<String>,
        #[structopt(long)]
        doc: bool,
        #[structopt(long)]
        contract: bool,
        #[structopt(long)]
        default: bool,
        #[structopt(long)]
        value: bool,
    },
    /// Typecheck a program, but do not run it
    Typecheck,
    /// Start an REPL session
    REPL,
}

fn main() {
    let opts = Opt::from_args();

    if let Some(Command::REPL) = opts.command {
        #[cfg(feature = "repl")]
        if rustyline_frontend::repl().is_err() {
            process::exit(1);
        }

        #[cfg(not(feature = "repl"))]
        eprintln!("error: this executable was not compiled with REPL support");
    } else {
        let mut program = opts
            .file
            .map(Program::new_from_file)
            .unwrap_or_else(Program::new_from_stdin)
            .unwrap_or_else(|err| {
                eprintln!("Error when reading input: {}", err);
                process::exit(1)
            });

        let result = match opts.command {
            Some(Command::Export { format, output }) => export(&mut program, format, output),
            Some(Command::Query {
                path,
                doc,
                contract,
                default,
                value,
            }) => {
                program.query(path).map(|term| {
                    // Print a default selection of attributes if no option is specified
                    let attrs = if !doc && !contract && !default && !value {
                        repl::query_print::Attributes::default()
                    } else {
                        repl::query_print::Attributes {
                            doc,
                            contract,
                            default,
                            value,
                        }
                    };

                    repl::query_print::print_query_result(&term, attrs)
                })
            }
            Some(Command::Typecheck) => program.typecheck().map(|_| ()),
            Some(Command::REPL) => unreachable!(),
            None => program.eval().map(|t| println!("Done: {:?}", t)),
        };

        if let Err(err) = result {
            program.report(err);
            process::exit(1)
        }
    }
}

fn export(
    program: &mut Program,
    format: Option<ExportFormat>,
    output: Option<PathBuf>,
) -> Result<(), Error> {
    let rt = program.eval_full().map(RichTerm::from)?;
    serialize::validate(&rt)?;

    let format = format.unwrap_or_default();

    if let Some(file) = output {
        let mut file = fs::File::create(&file).map_err(IOError::from)?;

        match format {
            ExportFormat::Json => serde_json::to_writer_pretty(file, &rt)
                .map_err(|err| SerializationError::Other(err.to_string())),
            ExportFormat::Yaml => serde_yaml::to_writer(file, &rt)
                .map_err(|err| SerializationError::Other(err.to_string())),
            ExportFormat::Toml => toml::ser::to_string_pretty(&rt)
                .map_err(|err| SerializationError::Other(err.to_string()))
                .and_then(|s| {
                    file.write_all(s.as_bytes())
                        .map_err(|err| SerializationError::Other(err.to_string()))
                }),
            ExportFormat::Raw => match *rt.term {
                Term::Str(s) => file
                    .write_all(s.as_bytes())
                    .map_err(|err| SerializationError::Other(err.to_string())),
                t => Err(SerializationError::Other(format!(
                    "raw export requires a `Str`, got {}",
                    t.type_of().unwrap()
                ))),
            },
        }?
    } else {
        match format {
            ExportFormat::Json => serde_json::to_writer_pretty(io::stdout(), &rt)
                .map_err(|err| SerializationError::Other(err.to_string())),
            ExportFormat::Yaml => serde_yaml::to_writer(io::stdout(), &rt)
                .map_err(|err| SerializationError::Other(err.to_string())),
            ExportFormat::Toml => toml::ser::to_string_pretty(&rt)
                .map_err(|err| SerializationError::Other(err.to_string()))
                .and_then(|s| {
                    std::io::stdout()
                        .write_all(s.as_bytes())
                        .map_err(|err| SerializationError::Other(err.to_string()))
                }),
            ExportFormat::Raw => match *rt.term {
                Term::Str(s) => std::io::stdout()
                    .write_all(s.as_bytes())
                    .map_err(|err| SerializationError::Other(err.to_string())),
                t => Err(SerializationError::Other(format!(
                    "raw export requires a `Str`, got {}",
                    t.type_of().unwrap()
                ))),
            },
        }?
    }

    Ok(())
}
