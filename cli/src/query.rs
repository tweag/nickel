use std::{fs, io::Write, path::PathBuf};

use nickel_lang_core::{
    error::{Error, IOError},
    identifier::{Ident, LocIdent},
    pretty::PrettyPrintCap,
    repl::query_print,
    serialize::{self, MetadataExportFormat},
    term::{record::Field, LabeledType, MergePriority, Term},
};
use serde::Serialize;

use crate::{
    cli::GlobalOptions,
    customize::{Customize, ExtractFieldOnly},
    error::{CliResult, ResultErrorExt, Warning},
    input::{InputOptions, Prepare},
};

const VALUE_EXPORT_MAX_WIDTH: usize = 80;

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
    /// Value is exported in its string representation, capped at 80 characters.
    ///
    /// This flag cannot be used along with the following flags: --doc, --contract, --type, --default, --value
    #[arg(long, short, value_enum, default_value_t, conflicts_with_all(["doc", "contract", "typ", "default", "value"]))]
    pub format: MetadataExportFormat,

    /// Output file. Standard output by default
    #[arg(short, long, conflicts_with_all(["doc", "contract", "typ", "default", "value"]))]
    pub output: Option<PathBuf>,

    #[command(flatten)]
    pub inputs: InputOptions<ExtractFieldOnly>,
}

#[derive(Clone, Debug, Serialize)]
struct QueryResult {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub doc: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub typ: Option<LabeledType>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub contracts: Vec<LabeledType>,
    pub optional: bool,
    pub not_exported: bool,
    pub priority: MergePriority,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub sub_fields: Option<Vec<Ident>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub value: Option<String>,
}

impl From<Field> for QueryResult {
    fn from(field: Field) -> Self {
        let sub_fields = match field.value {
            Some(ref val) => match val.as_ref() {
                Term::Record(record) if !record.fields.is_empty() => {
                    let mut fields: Vec<_> = record.fields.keys().collect();
                    fields.sort();
                    Some(fields.into_iter().map(LocIdent::ident).collect())
                }
                Term::RecRecord(record, dyn_fields, ..) if !record.fields.is_empty() => {
                    let mut fields: Vec<_> = record.fields.keys().map(LocIdent::ident).collect();
                    fields.sort();
                    let dynamic = Ident::from("<dynamic>");
                    fields.extend(dyn_fields.iter().map(|_| dynamic));
                    Some(fields)
                }
                // Empty record has empty sub_fields
                Term::Record(..) | Term::RecRecord(..) => Some(Vec::new()),
                // Non-record has no concept of sub-field
                _ => None,
            },
            None => None,
        };

        QueryResult {
            doc: field.metadata.doc,
            typ: field.metadata.annotation.typ,
            contracts: field.metadata.annotation.contracts,
            optional: field.metadata.opt,
            not_exported: field.metadata.not_exported,
            priority: field.metadata.priority,
            sub_fields,
            value: field
                .value
                .map(|v| v.pretty_print_cap(VALUE_EXPORT_MAX_WIDTH)),
        }
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

    fn export<T>(self, res: T, format: MetadataExportFormat) -> Result<(), Error>
    where
        T: Serialize,
    {
        // This is a near-verbatim copy of ExportCommand::export

        // We only add a trailing newline for JSON exports. Both YAML and TOML
        // exporters already append a trailing newline by default.
        let trailing_newline = format == MetadataExportFormat::Json;

        if let Some(file) = self.output {
            let mut file = fs::File::create(file).map_err(IOError::from)?;
            serialize::to_writer_metadata(&mut file, format, &res)?;

            if trailing_newline {
                writeln!(file).map_err(IOError::from)?;
            }
        } else {
            serialize::to_writer_metadata(std::io::stdout(), format, &res)?;

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

        use MetadataExportFormat::*;
        match self.format {
            Markdown => {
                if self.output.is_some() {
                    eprintln!("Output query result in markdown format to a file is currently not supported.")
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
            }
            format @ (Json | Toml | Yaml) => {
                let _ = &program
                    .query()
                    .map(QueryResult::from)
                    .map(|res| self.export(res, format))
                    .report_with_program(program)?;
            }
        }
        Ok(())
    }
}
