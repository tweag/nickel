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

    fn export(self, res: QueryResult, format: ExportFormatCommon) -> Result<(), Error> {
        let query_attributes = self.query_attributes();

        let mut metadata_out = serde_json::to_value(res.0.metadata).unwrap();
        let metadata_out = metadata_out.as_object_mut().unwrap();

        let selected_attrs = serde_json::to_value(&query_attributes).unwrap();
        let selected_attrs = selected_attrs.as_object().unwrap();
        let attrs_to_remove: Vec<_> = selected_attrs
            .into_iter()
            .filter_map(|(ref k, ref v)| match *v == false {
                true => Some(*k),
                false => None,
            })
            .collect();

        println!("{:?}", metadata_out);
        attrs_to_remove.iter().for_each(|attr| {
            metadata_out.remove(*attr);
        });
        println!("{:?}", metadata_out);

        let mut out = serde_json::Map::new();
        out.insert(
            "metadata".to_string(),
            serde_json::to_value(metadata_out).unwrap(),
        );

        if query_attributes.value && res.0.value.is_some() {
            out.insert(
                "value".to_string(),
                serde_json::to_value(res.0.value).unwrap(),
            );
        }

        // We only add a trailing newline for JSON exports. Both YAML and TOML
        // exporters already append a trailing newline by default.

        let trailing_newline = format == ExportFormatCommon::Json;

        // serialize::validate(self.format, &rt)?;

        if let Some(file) = self.output {
            let mut file = fs::File::create(file).map_err(IOError::from)?;
            serialize::to_writer_common(&mut file, format, &out)?;

            if trailing_newline {
                writeln!(file).map_err(IOError::from)?;
            }
        } else {
            serialize::to_writer_common(std::io::stdout(), format, &out)?;

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
