use std::{fs, io::Write, path::PathBuf};

use nickel_lang_core::{
    error::{Error, IOError},
    repl::query_print,
    serialize::{self, ExportFormatCommon},
    term::{record::Field, Term},
};
use serde::ser::{Serialize, SerializeStruct, Serializer};
use serde_json::{Map, Value};

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

    /// Export the value and all metadata of selected field in the specified format.
    ///
    /// This flag cannot be used along with the following flags: --doc, --contract, --type, --default, --value
    #[arg(long, short, value_enum, conflicts_with_all(["doc", "contract", "typ", "default", "value"]))]
    pub format: Option<ExportFormatCommon>,

    /// Output file. Standard output by default
    #[arg(short, long, conflicts_with_all(["doc", "contract", "typ", "default", "value"]))]
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

    // This is a near-verbatim copy of ExportCommand::export
    fn export(self, res: QueryResult, format: ExportFormatCommon) -> Result<(), Error> {
        // We only add a trailing newline for JSON exports. Both YAML and TOML
        // exporters already append a trailing newline by default.

        let trailing_newline = format == ExportFormatCommon::Json;

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

        Ok(())
    }

    pub fn run(self, global: GlobalOptions) -> CliResult<()> {
        let mut program = self.inputs.prepare(&global)?;

        if self.inputs.customize_mode.field().is_none() {
            program.report(Warning::EmptyQueryPath, global.error_format);
        }

        if let Some(format) = self.format {
            let query_res = program.query().map(|field| QueryResult(field));
            if let Ok(res) = query_res {
                self.export(res, format).report_with_program(program)?
            } else {
                eprintln!("No metadata found for this field.")
            }
        } else {
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

        Ok(())
    }
}
