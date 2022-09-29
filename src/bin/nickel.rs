//! Entry point of the program.
use nickel_lang::error::{Error, IOError};
use nickel_lang::eval::cache::CBNCache;
use nickel_lang::program::Program;
use nickel_lang::repl::query_print;
#[cfg(feature = "repl")]
use nickel_lang::repl::rustyline_frontend;
use nickel_lang::term::{RichTerm, Term};
use nickel_lang::{serialize, serialize::ExportFormat};
use std::path::{Path, PathBuf};
use std::{
    fs::{self, File},
    io::Read,
    process,
};
// use std::ffi::OsStr;
use directories::BaseDirs;
use structopt::StructOpt;

type EC = CBNCache;

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

    #[structopt(subcommand)]
    command: Option<Command>,
}

/// Available subcommands.
#[derive(StructOpt, Debug)]
enum Command {
    /// translate Nix input to Nickel code.
    /// Only a POC, main target is to be able to run Nix code on nickel.
    /// May never be a complet source to source transformation.
    Nixin,
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
        /// Specify the path of the generated documentation file. Default to
        /// `~/.nickel/doc/<input-file>.md` for input `<input-file>.ncl`, or to
        /// `~/.nickel/doc/out.md` if the input is read from stdin.
        #[structopt(short = "o", long, parse(from_os_str))]
        output: Option<PathBuf>,
    },
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
        if rustyline_frontend::repl(histfile).is_err() {
            process::exit(1);
        }

        #[cfg(not(feature = "repl"))]
        eprintln!("error: this executable was not compiled with REPL support");
    } else if let Some(Command::Nixin) = opts.command {
        use nickel_lang::cache::Cache;
        use nickel_lang::cache::ErrorTolerance;
        use nickel_lang::pretty::*;
        use pretty::BoxAllocator;

        let mut buf = String::new();
        let mut cache = Cache::new(ErrorTolerance::Strict);
        let mut out: Vec<u8> = Vec::new();
        opts.file
            .map(std::fs::File::open)
            .map(|f| f.and_then(|mut f| f.read_to_string(&mut buf)))
            .unwrap_or(std::io::stdin().read_to_string(&mut buf))
            .unwrap_or_else(|err| {
                eprintln!("Error when reading input: {}", err);
                process::exit(1)
            });
        let allocator = BoxAllocator;
        let file_id = cache.add_source("<stdin>.nix", buf.as_bytes()).unwrap();
        let rt = nickel_lang::nix::parse(&cache, file_id).unwrap();
        let doc: DocBuilder<_, ()> = rt.pretty(&allocator);
        doc.render(80, &mut out).unwrap();
        println!("{}", String::from_utf8_lossy(&out).as_ref());
    } else {
        let mut program = opts
            .file
            .clone()
            .map(Program::new_from_file)
            .unwrap_or_else(Program::new_from_stdin)
            .unwrap_or_else(|err| {
                eprintln!("Error when reading input: {}", err);
                process::exit(1)
            });

        #[cfg(debug_assertions)]
        if opts.nostdlib {
            program.set_skip_stdlib();
        }

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
            Some(Command::Repl { .. }) | Some(Command::Nixin) => unreachable!(),
            #[cfg(feature = "doc")]
            Some(Command::Doc { ref output }) => output
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
                    let mut markdown_file = docpath.to_path_buf();

                    let mut has_file_name = false;

                    if let Some(path) = opts.file {
                        if let Some(file_stem) = path.file_stem() {
                            markdown_file.push(file_stem);
                            has_file_name = true;
                        }
                    }

                    if !has_file_name {
                        markdown_file.push("out");
                    }

                    markdown_file.set_extension("md");
                    File::create(markdown_file.clone().into_os_string()).map_err(|e| {
                        Error::IOError(IOError(format!(
                            "when opening or creating output file `{}`: {}",
                            markdown_file.to_string_lossy(),
                            e
                        )))
                    })
                })
                .and_then(|mut out| program.output_doc(&mut out)),
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
    program: &mut Program<EC>,
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
