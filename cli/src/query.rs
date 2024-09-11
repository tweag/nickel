use std::{fs, io::Write, path::PathBuf};

use nickel_lang_core::{
    error::{Error, IOError},
    identifier::{Ident, LocIdent},
    repl::query_print,
    serialize::{self, MetadataExportFormat},
    term::{record::Field, LabeledType, MergePriority, RichTerm, Term},
};
use serde::Serialize;

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
    pub doc: Option<String>,
    pub typ: Option<LabeledType>,
    pub contracts: Vec<LabeledType>,
    pub optional: bool,
    pub not_exported: bool,
    pub priority: MergePriority,
    pub sub_fields: Option<Vec<Ident>>,
    pub value: Option<RichTerm>,
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
            value: field.value,
        }
    }
}

impl QueryResult {
    pub fn filter_for_export(self) -> serde_json::Map<String, serde_json::Value> {
        let mut query_res = serde_json::to_value(&self).unwrap();
        let query_res = query_res.as_object_mut().unwrap();

        if self.value.is_none() {
            query_res.remove("value");
        }
        if self.doc.is_none() {
            query_res.remove("doc");
        }
        if self.contracts.is_empty() {
            query_res.remove("contracts");
        }
        if self.typ.is_none() {
            query_res.remove("typ");
        }
        if self.sub_fields.is_none() {
            query_res.remove("sub_fields");
        }

        query_res.to_owned()
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
                if let Some(_) = self.output {
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
                    .map(QueryResult::filter_for_export)
                    .map(|res| self.export(res, format))
                    .report_with_program(program)?;
            }
        }
        Ok(())
    }
}
