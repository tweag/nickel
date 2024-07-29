use std::{fs, io::Write, path::PathBuf};

use nickel_lang_core::{
    error::{Error, IOError},
    repl::query_print,
    serialize::{self, ExportFormatCommon},
    term::record::Field,
};
use serde::ser::{Serialize, SerializeStruct, Serializer};

use crate::{
    cli::GlobalOptions,
    customize::{Customize, ExtractFieldOnly},
    error::{CliResult, ResultErrorExt, Warning},
    input::{InputOptions, Prepare},
};

#[derive(clap::Parser, Debug)]
pub struct QueryCommand {
    #[arg(long)]
    pub doc: bool,

    #[arg(long)]
    pub contract: bool,

    #[arg(long = "type")]
    pub typ: bool,

    #[arg(long)]
    pub default: bool,

    #[arg(long)]
    pub value: bool,

    #[arg(long, short, value_enum)]
    pub format: Option<ExportFormatCommon>,

    /// Output file. Standard output by default
    #[arg(short, long)]
    pub output: Option<PathBuf>,

    #[command(flatten)]
    pub inputs: InputOptions<ExtractFieldOnly>,
}

#[derive(Clone, Debug)]
struct QueryResult(pub Field);

impl Serialize for QueryResult {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut state = serializer.serialize_struct("QueryResult", 2)?;
        state.serialize_field("value", &self.0.value)?;
        state.serialize_field("metadata", &self.0.metadata)?;
        state.end()
    }
}

impl QueryCommand {
    fn attributes_specified(&self) -> bool {
        self.doc || self.contract || self.typ || self.default || self.value
    }

    fn query_attributes(&self) -> query_print::Attributes {
        // Use a default selection of attributes if no option is specified
        if !self.attributes_specified() {
            query_print::Attributes::default()
        } else {
            query_print::Attributes {
                doc: self.doc,
                contract: self.contract,
                typ: self.typ,
                default: self.default,
                value: self.value,
            }
        }
    }

    pub fn run(self, global: GlobalOptions) -> CliResult<()> {
        let mut program = self.inputs.prepare(&global)?;

        if self.inputs.customize_mode.field().is_none() {
            program.report(Warning::EmptyQueryPath, global.error_format);
        }

        let trailing_newline = match self.format {
            Some(ExportFormatCommon::Json) => true,
            _ => false,
        };

        match self.format {
            None => {
                let found = program
                    .query()
                    .map(|field| {
                        query_print::write_query_result(
                            &mut std::io::stdout(),
                            &field,
                            self.query_attributes(),
                        )
                        .unwrap()
                    })
                    .report_with_program(program)?;

                if !found {
                    eprintln!("No metadata found for this field.")
                }
            }
            Some(format) => {
                let found = program.query().map(|field| QueryResult(field));
                match found {
                    Ok(res) => {
                        if let Some(file) = self.output {
                            let mut file = fs::File::create(file).map_err(IOError::from)?;
                            serialize::to_writer_common(&mut file, format, &res)?;

                            if trailing_newline {
                                writeln!(file).map_err(IOError::from)?;
                            }
                        } else {
                            serialize::to_writer_common(std::io::stdout(), format, &res)?;

                            if trailing_newline {
                                println!();
                            }
                        }
                    }
                    _ => {
                        eprintln!("some error...");
                    }
                }
            }
        }

        Ok(())
    }
}
