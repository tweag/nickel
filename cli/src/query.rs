use std::{fs, io::Write, path::PathBuf};

use nickel_lang_core::{
    bytecode::value::{RecordBody, TermBody, ValueContentRef, Container},
    error::{Error, IOError, Reporter as _},
    eval::cache::lazy::CBNCache,
    identifier::{Ident, LocIdent},
    position::PosTable,
    pretty::PrettyPrintCap,
    program::Program,
    repl::query_print,
    serialize::{self, MetadataExportFormat},
    term::{LabeledType, MergePriority, Term, record::Field},
};
use serde::Serialize;

use crate::{
    customize::{Customize, ExtractFieldOnly},
    error::Warning,
    global::GlobalContext,
    input::InputOptions,
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
        let sub_fields = field.value.as_ref().and_then(|val| {
            match val.content_ref() {
                ValueContentRef::Record(Container::Alloc(RecordBody(record))) if !record.fields.is_empty() => {
                    let mut fields: Vec<_> = record.fields.keys().collect();
                    fields.sort();
                    Some(fields.into_iter().map(LocIdent::ident).collect())
                }
                ValueContentRef::Term(TermBody(Term::RecRecord(
                    record,
                    includes,
                    dyn_fields,
                    ..,
                ))) if !record.fields.is_empty() => {
                    let mut fields: Vec<_> = record.fields.keys().map(LocIdent::ident).collect();
                    fields.extend(includes.iter().map(|incl| incl.ident.ident()));
                    fields.sort();
                    let dynamic = Ident::from("<dynamic>");
                    fields.extend(dyn_fields.iter().map(|_| dynamic));
                    Some(fields)
                }
                // Empty record has empty sub_fields
                ValueContentRef::Record(..)
                | ValueContentRef::Term(TermBody(Term::RecRecord(..))) => Some(Vec::new()),
                // Non-record has no concept of sub-field
                _ => None,
            }
        });

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

    fn export<T>(
        &self,
        pos_table: &PosTable,
        res: T,
        format: MetadataExportFormat,
    ) -> Result<(), Error>
    where
        T: Serialize,
    {
        // This is a near-verbatim copy of ExportCommand::export

        // We only add a trailing newline for JSON exports. Both YAML and TOML
        // exporters already append a trailing newline by default.
        let trailing_newline = format == MetadataExportFormat::Json;

        if let Some(file) = &self.output {
            let mut file = fs::File::create(file).map_err(IOError::from)?;
            serialize::to_writer_metadata(&mut file, format, &res)
                .map_err(|error| error.with_pos_table(pos_table.clone()))?;

            if trailing_newline {
                writeln!(file).map_err(IOError::from)?;
            }
        } else {
            serialize::to_writer_metadata(std::io::stdout(), format, &res)
                .map_err(|error| error.with_pos_table(pos_table.clone()))?;

            if trailing_newline {
                println!();
            }
        }

        Ok(())
    }

    pub fn run(self, ctxt: &mut GlobalContext) {
        if self.inputs.customize_mode.field().is_none() {
            ctxt.reporter.report(Warning::EmptyQueryPath);
        }

        ctxt.with_program(&self.inputs, |program| self.output(program));
    }

    fn output(
        &self,
        program: &mut Program<CBNCache>,
    ) -> Result<(), nickel_lang_core::error::Error> {
        use MetadataExportFormat::*;
        match self.format {
            Markdown => {
                if self.output.is_some() {
                    eprintln!(
                        "Output query result in markdown format to a file is currently not supported."
                    )
                } else {
                    let found = program.query().map(|field| {
                        query_print::write_query_result(
                            &mut std::io::stdout(),
                            &field,
                            self.query_attributes(),
                        )
                        .unwrap()
                    })?;

                    if !found {
                        eprintln!("No metadata found for this field.")
                    }
                }
            }
            format @ (Json | Toml | Yaml) => {
                let _ = &program
                    .query()
                    .map(QueryResult::from)
                    .map(|res| self.export(program.pos_table(), res, format))?;
            }
        }
        Ok(())
    }
}
