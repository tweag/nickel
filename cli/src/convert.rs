use std::{
    io::{Read, Write},
    path::PathBuf,
};

use clap::ArgGroup;
use nickel_lang_core::{
    ast::{AstAlloc, InputFormat, Node},
    error::Reporter as _,
    files::Files,
    parser::{self, ErrorTolerantParser, lexer::Lexer},
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
        let (format, data) = match (&self.file, self.stdin_format) {
            (None, None) | (Some(_), Some(_)) => {
                unreachable!("clap shouldn't allow this")
            }
            (None, Some(stdin_format)) => {
                let mut buf = String::new();
                std::io::stdin().read_to_string(&mut buf)?;
                (stdin_format, buf)
            }
            (Some(path), None) => {
                let format = InputFormat::from_path(path).ok_or_else(|| Error::CliUsage {
                    files: Files::empty(),
                    error: CliUsageError::CantDetectFormat { path: path.clone() },
                })?;
                let data = std::fs::read_to_string(path)?;
                (format, data)
            }
        };

        let mut files = Files::empty();
        let file_id = files.add("todo", data.as_str());
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
            InputFormat::Yaml => {
                nickel_lang_core::serialize::yaml::load_yaml(&alloc, &data, Some(file_id))
                    .map_err(|e| e.into())
            }
            InputFormat::Toml => {
                // Currently, our TOML deserialization goes straight to NickelValue, but we want an
                // Ast...
                todo!()
            }
            InputFormat::Text => {
                // We convert text to Nickel by wrapping it in a string.
                Ok(Node::String(alloc.alloc_str(&data)).into())
            }
            #[cfg(feature = "nix-experimental")]
            InputFormat::Nix => {
                use nickel_lang_core::{error::ParseError, nix_ffi};

                let parent_dir = self.file.as_ref().and_then(|p| p.parent());
                let parent_dir = parent_dir
                    .map(PathBuf::from)
                    .or_else(|| std::env::current_dir().ok())
                    .unwrap_or_default();
                let json = nix_ffi::eval_to_json(&data, &parent_dir).map_err(|e| {
                    ParseError::ExternalFormatError(String::from("nix"), e.to_string(), None).into()
                });
                json.and_then(|json| {
                    nickel_lang_core::serialize::yaml::load_json(&alloc, &json, Some(file_id))
                        .map_err(|e| e.into())
                })
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
