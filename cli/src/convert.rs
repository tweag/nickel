use std::{
    io::{Read, Write},
    path::{Path, PathBuf},
};

use clap::ArgGroup;
use nickel_lang_core::{
    ast::{AstAlloc, InputFormat, Node},
    error::{ParseError, Reporter as _},
    files::Files,
    parser::{self, ErrorTolerantParser, lexer::Lexer},
    serialize::yaml::Listify,
};

use crate::{
    error::{CliResult, CliUsageError, Error},
    global::GlobalContext,
};

#[derive(clap::Parser, Debug)]
#[clap(group(ArgGroup::new("fmt_or_file").args(&["file", "stdin_format"]).required(true)))]
pub struct ConvertCommand {
    /// Input file. Omit to read from stdin.
    pub file: Option<PathBuf>,

    /// Specify the format of the input from stdin
    #[arg(long, value_enum, conflicts_with = "file")]
    pub stdin_format: Option<InputFormat>,

    /// Output file. Standard output by default
    #[arg(short, long)]
    pub output: Option<PathBuf>,
}

impl ConvertCommand {
    fn write(&self, output: &str) -> CliResult<()> {
        if let Some(out) = &self.output {
            std::fs::write(out, output)?;
        } else {
            std::io::stdout().write_all(output.as_bytes())?;
        }
        Ok(())
    }

    fn convert(self) -> CliResult<()> {
        let (format, name) = match (&self.file, self.stdin_format) {
            (None, None) | (Some(_), Some(_)) => {
                unreachable!("clap shouldn't allow this")
            }
            (None, Some(stdin_format)) => (stdin_format, Path::new("<stdin>")),
            (Some(path), None) => {
                let format = InputFormat::from_path(path).ok_or_else(|| Error::CliUsage {
                    files: Files::empty(),
                    error: CliUsageError::CantDetectFormat { path: path.clone() },
                })?;
                (format, path.as_path())
            }
        };

        #[cfg(feature = "nix-experimental")]
        if format == InputFormat::Nix {
            return Err(Error::CliUsage {
                files: Files::empty(),
                error: CliUsageError::NoNixConversion {
                    path: name.to_owned(),
                },
            });
        }

        let data = match &self.file {
            None => {
                let mut buf = String::new();
                std::io::stdin().read_to_string(&mut buf)?;
                buf
            }
            Some(path) => std::fs::read_to_string(path)?,
        };

        let mut files = Files::empty();
        let file_id = files.add(name, data.as_str());
        let alloc = AstAlloc::new();
        let ast = match format {
            // In principle, we could just pass Nickel input straight through without parsing it,
            // but then we wouldn't catch parse errors.
            InputFormat::Nickel => parser::grammar::TermParser::new()
                .parse_strict(&alloc, file_id, Lexer::new(&data))
                .map_err(nickel_lang_core::error::Error::from),
            InputFormat::Json => {
                nickel_lang_core::serialize::yaml::load_json(&alloc, &data, Some(file_id))
                    .map_err(|e| e.into())
            }
            InputFormat::Yaml => nickel_lang_core::serialize::yaml::load_yaml(
                &alloc,
                &data,
                Some(file_id),
                Listify::Auto,
            )
            .map_err(|e| e.into()),
            InputFormat::Toml => {
                nickel_lang_core::serialize::toml_deser::ast_from_str(&alloc, &data, file_id)
                    .map_err(|e| ParseError::from_toml(e, file_id).into())
            }
            InputFormat::Text => {
                // We convert text to Nickel by wrapping it in a string.
                Ok(Node::String(alloc.alloc_str(&data)).into())
            }
            #[cfg(feature = "nix-experimental")]
            InputFormat::Nix => {
                // We already errored for InputFormat::Nix
                unreachable!()
            }
        };

        let ast = ast.map_err(|error| Error::Program { files, error })?;

        self.write(&ast.to_string())?;

        Ok(())
    }

    pub fn run(self, ctxt: &mut GlobalContext) {
        if let Err(e) = self.convert() {
            ctxt.reporter.report(e);
        }
    }
}
