//! Entry point of the program.
mod error;
mod eval;
mod identifier;
mod label;
mod merge;
mod operation;
mod parser;
mod position;
mod program;
mod serialize;
mod stack;
mod stdlib;
mod term;
mod transformations;
mod typecheck;
mod types;

use crate::error::{Error, IOError, SerializationError};
use crate::program::Program;
use crate::term::RichTerm;
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
    Json,
}

impl std::default::Default for ExportFormat {
    fn default() -> Self {
        ExportFormat::Json
    }
}

impl fmt::Display for ExportFormat {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "json")
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
            "json" => Ok(ExportFormat::Json),
            _ => Err(ParseFormatError(String::from(s))),
        }
    }
}

/// Available subcommands.
#[derive(StructOpt, Debug)]
enum Command {
    /// Export the result to a different format
    Export {
        /// Available formats: `json`. Default format: `json`.
        #[structopt(long)]
        format: Option<ExportFormat>,
        /// Output file. Standard output by default
        #[structopt(short = "o", long)]
        #[structopt(parse(from_os_str))]
        output: Option<PathBuf>,
    },
    /// Typecheck a program, but do not run it
    Typecheck,
}

fn main() {
    let opts = Opt::from_args();
    let mut program = opts
        .file
        .map(|path: PathBuf| -> io::Result<_> {
            let file = fs::File::open(&path)?;
            Program::new_from_source(file, &path)
        })
        .unwrap_or_else(Program::new_from_stdin)
        .unwrap_or_else(|err| {
            eprintln!("Error when reading input: {}", err);
            process::exit(1)
        });

    let result = match opts.command {
        Some(Command::Export { format, output }) => {
            program.eval_full().map(RichTerm::from).and_then(|rt| {
                serialize::validate(&rt).map_err(Error::from)?;

                let format = format.unwrap_or_default();

                if let Some(file) = output {
                    let file = fs::File::create(&file).map_err(IOError::from)?;

                    match format {
                        ExportFormat::Json => serde_json::to_writer_pretty(file, &rt),
                    }
                    .map_err(|err| SerializationError::Other(err.to_string()))?;
                } else {
                    match format {
                        ExportFormat::Json => serde_json::to_writer_pretty(io::stdout(), &rt),
                    }
                    .map_err(|err| SerializationError::Other(err.to_string()))?;
                }

                Ok(())
            })
        }
        Some(Command::Typecheck) => program.typecheck().map(|_| ()),
        None => program.eval().and_then(|t| {
            println!("Done: {:?}", t);
            Ok(())
        }),
    };

    if let Err(err) = result {
        program.report(err);
        process::exit(1)
    }
}
