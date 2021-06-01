//! Entry point of the program.
use nickel::error::{Error, IOError};
use nickel::program::Program;
use nickel::term::RichTerm;
use nickel::{repl, repl::rustyline_frontend};
use nickel::{serialize, serialize::ExportFormat};
use std::path::PathBuf;
use std::{fs, process};
// use std::ffi::OsStr;
use structopt::StructOpt;

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

                    repl::query_print::write_query_result(&mut std::io::stdout(), &term, attrs).unwrap()
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
    let format = format.unwrap_or_default();

    serialize::validate(format, &rt)?;

    if let Some(file) = output {
        let file = fs::File::create(&file).map_err(IOError::from)?;
        serialize::to_writer(file, format, &rt)?;
    } else {
        serialize::to_writer(std::io::stdout(), format, &rt)?;
    }

    Ok(())
}
