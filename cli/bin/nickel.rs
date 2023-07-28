//! Entry point of the program.
use core::fmt;
use nickel_lang_core::error::{Error, IOError};
use nickel_lang_core::eval::cache::CacheImpl;
use nickel_lang_core::program::Program;
use nickel_lang_core::repl::query_print;
use nickel_lang_core::term::RichTerm;
use nickel_lang_core::{serialize, serialize::ExportFormat};

use git_version::git_version;
use std::path::PathBuf;
use std::{fs, io::Write, process};

/// Command-line options and subcommands.
#[derive(clap::Parser, Debug)]
/// The interpreter of the Nickel language.
struct Opt {
    /// The input file. Standard input by default
    #[arg(short, long, global = true)]
    file: Option<PathBuf>,

    #[cfg(debug_assertions)]
    /// Skips the standard library import. For debugging only. This does not affect REPL
    #[arg(long)]
    nostdlib: bool,

    /// Coloring: auto, always, never.
    #[arg(long, global = true, value_enum, default_value_t)]
    color: clap::ColorChoice,

    #[command(subcommand)]
    command: Option<Command>,

    /// Print version.
    #[arg(long, short = 'V')]
    version: bool,
}

/// Available subcommands.
#[derive(clap::Subcommand, Debug)]
enum Command {
    /// Converts the parsed representation (AST) back to Nickel source code and prints it. Used for
    /// debugging purpose
    PprintAst {
        /// Performs code transformations before printing
        #[arg(long)]
        transform: bool,
    },
    /// Exports the result to a different format
    Export {
        #[arg(long, value_enum, default_value_t)]
        format: ExportFormat,

        /// Output file. Standard output by default
        #[arg(short, long)]
        output: Option<PathBuf>,
    },
    /// Prints the metadata attached to an attribute, given as a path
    Query {
        path: Option<String>,
        #[arg(long)]
        doc: bool,
        #[arg(long)]
        contract: bool,
        #[arg(long = "type")]
        typ: bool,
        #[arg(long)]
        default: bool,
        #[arg(long)]
        value: bool,
    },
    /// Typechecks the program but do not run it
    Typecheck,
    /// Starts an REPL session
    #[cfg(feature = "repl")]
    Repl {
        #[arg(long)]
        history_file: Option<PathBuf>,
    },
    /// Generates the documentation files for the specified nickel file
    #[cfg(feature = "doc")]
    Doc {
        /// The path of the generated documentation file. Default to
        /// `~/.nickel/doc/<input-file>.md` for input `<input-file>.ncl`, or to
        /// `~/.nickel/doc/out.md` if the input is read from stdin.
        #[arg(short, long)]
        output: Option<PathBuf>,
        /// Write documentation to stdout. Takes precedence over `output`
        #[arg(long)]
        stdout: bool,
        /// The output format for the generated documentation.
        #[arg(long, value_enum, default_value_t)]
        format: nickel_lang_cli::doc::DocFormat,
    },
    /// Format a nickel file
    #[cfg(feature = "format")]
    Format {
        /// Output file. Standard output by default.
        #[arg(short, long)]
        output: Option<PathBuf>,
        /// Format in place, overwriting the input file.
        #[arg(short, long, requires = "file")]
        in_place: bool,
    },
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct ParseFormatError(String);

impl fmt::Display for ParseFormatError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "unsupported export format {}", self.0)
    }
}

fn handle_eval_commands(opts: Opt) {
    let mut program = opts
        .file
        .clone()
        .map(|f| Program::new_from_file(f, std::io::stderr()))
        .unwrap_or_else(|| Program::new_from_stdin(std::io::stderr()))
        .unwrap_or_else(|err| {
            eprintln!("Error when reading input: {err}");
            process::exit(1)
        });

    #[cfg(debug_assertions)]
    if opts.nostdlib {
        program.set_skip_stdlib();
    }

    program.set_color(opts.color.into());

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
            typ: types,
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
                        typ: types,
                        default,
                        value,
                    }
                };

                query_print::write_query_result(&mut std::io::stdout(), &term, attrs).unwrap()
            })
        }
        Some(Command::Typecheck) => program.typecheck(),
        #[cfg(feature = "doc")]
        Some(Command::Doc {
            output,
            stdout,
            format,
        }) => nickel_lang_cli::doc::export_doc(
            &mut program,
            opts.file.as_ref(),
            output,
            stdout,
            format,
        ),
        None => program.eval_full().map(|t| println!("{t}")),

        #[cfg(feature = "repl")]
        Some(Command::Repl { .. }) => unreachable!(),
        #[cfg(feature = "format")]
        Some(Command::Format { .. }) => unreachable!(),
    };

    if let Err(err) = result {
        program.report(err);
        process::exit(1)
    }
}

fn main() {
    use clap::Parser;

    let opts = Opt::parse();

    if opts.version {
        println!(
            "{} {} (rev {})",
            env!("CARGO_BIN_NAME"),
            env!("CARGO_PKG_VERSION"),
            git_version!()
        );
        return;
    }

    match opts.command {
        #[cfg(feature = "repl")]
        Some(Command::Repl { history_file }) => {
            nickel_lang_cli::repl::repl(history_file, opts.color.into())
        }
        #[cfg(feature = "format")]
        Some(Command::Format { output, in_place }) => {
            nickel_lang_cli::format::format(opts.file.as_deref(), output.as_deref(), in_place)
        }
        _ => handle_eval_commands(opts),
    }
}

fn export(
    program: &mut Program<CacheImpl>,
    format: ExportFormat,
    output: Option<PathBuf>,
) -> Result<(), Error> {
    let rt = program.eval_full_for_export().map(RichTerm::from)?;

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
