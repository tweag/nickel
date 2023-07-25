use std::collections::{BTreeMap, HashMap};
use std::ffi::OsString;
use std::io::Write;
use std::{fs, path::PathBuf};

use clap::parser::ValueSource;
use clap::Command;
use nickel_lang_core::error::Error;
use nickel_lang_core::identifier::Ident;
use nickel_lang_core::term::record::{Field, RecordData};
use nickel_lang_core::term::{IndexMap, LabeledType, MergePriority, Term, TypeAnnotation};
use nickel_lang_core::typ::{RecordRowF, RecordRowsIteratorItem, Type, TypeF};
use nickel_lang_core::{
    error::IOError,
    eval::cache::lazy::CBNCache,
    program::Program,
    serialize::{self, ExportFormat},
    term::RichTerm,
};

use crate::error::{CliResult, ResultErrorExt};
use crate::{cli::GlobalOptions, eval::EvalCommand};

#[derive(clap::Parser, Debug)]
pub struct ExportCommand {
    #[arg(long, value_enum, default_value_t)]
    pub format: ExportFormat,

    /// Output file. Standard output by default
    #[arg(short, long)]
    pub output: Option<PathBuf>,

    #[command(flatten)]
    pub evaluation: EvalCommand,

    #[arg(last = true)]
    pub freeform: Vec<OsString>,
}

#[derive(Debug, Clone)]
struct Interface {
    fields: BTreeMap<Ident, InterfaceField>,
}

#[derive(Debug, Clone)]
struct InterfaceAnnotation {
    typ: Option<Type>,
    contracts: Vec<Type>,
}

#[derive(Debug, Clone)]
struct InterfaceField {
    subfields: Option<Interface>,
    documentation: Option<String>,
    default: Option<String>,
    annotation: InterfaceAnnotation,
}

trait Merge {
    fn merge(first: Self, second: Self) -> Self;
}

impl Interface {
    fn empty() -> Interface {
        Interface {
            fields: BTreeMap::new(),
        }
    }
}

impl Merge for Interface {
    fn merge(first: Self, second: Self) -> Self {
        let Interface { mut fields } = first;
        for (id, field) in second.fields.into_iter() {
            if let Some(prev) = fields.remove(&id) {
                fields.insert(id, Merge::merge(prev, field));
            } else {
                fields.insert(id, field);
            }
        }
        Interface { fields }
    }
}

impl Merge for Option<Interface> {
    fn merge(first: Self, second: Self) -> Self {
        match (first, second) {
            (None, None) => None,
            (None, Some(intf)) | (Some(intf), None) => Some(intf),
            (Some(intf1), Some(intf2)) => Some(Merge::merge(intf1, intf2)),
        }
    }
}

impl Merge for InterfaceAnnotation {
    fn merge(mut first: Self, second: Self) -> Self {
        first.contracts.extend(second.contracts);
        InterfaceAnnotation {
            typ: first.typ.or(second.typ),
            contracts: first.contracts,
        }
    }
}

impl Merge for InterfaceField {
    fn merge(first: Self, second: Self) -> Self {
        InterfaceField {
            subfields: Merge::merge(first.subfields, second.subfields),
            documentation: first.documentation.or(second.documentation),
            default: first.default.or(second.default),
            annotation: Merge::merge(first.annotation, second.annotation),
        }
    }
}

impl From<&RecordData> for Interface {
    fn from(value: &RecordData) -> Self {
        Interface {
            fields: value
                .fields
                .iter()
                .map(|(id, field)| (*id, field.into()))
                .collect(),
        }
    }
}

impl From<&Term> for Interface {
    fn from(term: &Term) -> Self {
        term.extract_interface().unwrap_or_else(Interface::empty)
    }
}

trait ExtractInterface {
    fn extract_interface(&self) -> Option<Interface>;
}

impl ExtractInterface for &Term {
    fn extract_interface(&self) -> Option<Interface> {
        if let Term::Record(rd) = self {
            Some(Interface::from(rd))
        } else {
            None
        }
    }
}

impl ExtractInterface for Field {
    fn extract_interface(&self) -> Option<Interface> {
        self.value.as_ref().map(|t| Interface::from(t.as_ref()))
    }
}

impl ExtractInterface for Type {
    fn extract_interface(&self) -> Option<Interface> {
        match &self.typ {
            TypeF::Flat(rt) => rt.as_ref().extract_interface(),
            TypeF::Record(rrows) => Some(Interface {
                fields: rrows
                    .iter()
                    .filter_map(|item| {
                        if let RecordRowsIteratorItem::Row(rrow) = item {
                            Some((rrow.id, InterfaceField::from(&rrow)))
                        } else {
                            None
                        }
                    })
                    .collect(),
            }),
            _ => None,
        }
    }
}

impl ExtractInterface for LabeledType {
    fn extract_interface(&self) -> Option<Interface> {
        self.typ.extract_interface()
    }
}

impl From<&Field> for InterfaceField {
    fn from(field: &Field) -> Self {
        let subfields = field
            .metadata
            .annotation
            .iter()
            .map(LabeledType::extract_interface)
            .chain(std::iter::once(field.extract_interface()))
            .reduce(Merge::merge)
            .flatten();

        InterfaceField {
            subfields,
            documentation: field.metadata.doc.clone(),
            default: if field.metadata.priority >= MergePriority::Neutral {
                None
            } else {
                field.value.as_ref().map(RichTerm::to_string)
            },
            annotation: InterfaceAnnotation::from(&field.metadata.annotation),
        }
    }
}

impl From<&RecordRowF<&Type>> for InterfaceField {
    fn from(rrow: &RecordRowF<&Type>) -> Self {
        InterfaceField {
            subfields: rrow.typ.extract_interface(),
            documentation: None,
            default: None,
            annotation: InterfaceAnnotation {
                typ: Some(rrow.typ.clone()),
                contracts: vec![],
            },
        }
    }
}

impl From<&TypeAnnotation> for InterfaceAnnotation {
    fn from(ann: &TypeAnnotation) -> Self {
        InterfaceAnnotation {
            typ: ann.typ.as_ref().map(|t| t.typ.clone()),
            contracts: ann.contracts.iter().map(|t| t.typ.clone()).collect(),
        }
    }
}

impl InterfaceField {
    fn as_arg(
        &self,
        cmd: clap::Command,
        path: Vec<String>,
        paths: &mut HashMap<clap::Id, Vec<String>>,
    ) -> clap::Command {
        let id = path.join(".");
        let prev = paths.insert(clap::Id::from(&id), path.clone());
        assert!(matches!(prev, None));

        let arg = clap::Arg::new(&id)
            .long(id)
            .value_name(get_value_name(&self.annotation))
            .required(false); // XXX: Use `optional` correctly and create clap argument groups

        let arg = match &self.documentation {
            None => arg,
            Some(doc) => arg.help(doc),
        };

        let arg = match &self.default {
            None => arg,
            Some(default) => arg.default_value(default),
        };

        let mut cmd = cmd.arg(arg);

        for (id, field) in self.subfields.iter().flat_map(|intf| intf.fields.iter()) {
            let mut path = path.clone();
            path.push(id.to_string());
            cmd = field.as_arg(cmd, path, paths);
        }

        cmd
    }
}

fn get_value_name(_annotation: &InterfaceAnnotation) -> String {
    "STRING".to_owned()
}

fn build_clap(
    mut cmd: clap::Command,
    interface: &Interface,
) -> (clap::Command, HashMap<clap::Id, Vec<String>>) {
    let mut paths = HashMap::new();
    for (id, field) in &interface.fields {
        cmd = field.as_arg(cmd, vec![id.to_string()], &mut paths)
    }
    (cmd, paths)
}

impl ExportCommand {
    pub fn run(self, global: GlobalOptions) -> CliResult<()> {
        let mut program = self.evaluation.prepare(&global)?;

        self.export(&mut program).report_with_program(program)
    }

    // XXX: do we want this attached specifically to the `export` command?
    // XXX: we should give a nice error message when someone tries to evaluate some
    //      expression that has unset values, telling them they can set them using
    //      this method
    fn export(self, program: &mut Program<CBNCache>) -> Result<(), Error> {
        let overrides = if self.freeform.is_empty() {
            None
        } else {
            // TODO: rename eval_for_doc something more general. `.record_normal_form()`?
            let evaled = program.eval_for_doc()?;

            let (cmd, paths) = build_clap(
                Command::new("extra-args").no_binary_name(true),
                &Interface::from(evaled.as_ref()),
            );
            let arg_matches = cmd.get_matches_from(self.freeform);

            Some(
                arg_matches
                    .ids()
                    .filter_map(|id| -> Option<(Vec<String>, String)> {
                        (!matches!(
                            arg_matches.value_source(id.as_str()),
                            Some(ValueSource::DefaultValue)
                        ))
                        .then(|| {
                            (
                                paths.get(id).unwrap().clone(),
                                arg_matches.get_one::<String>(id.as_str()).unwrap().clone(),
                            )
                        })
                    })
                    .collect::<IndexMap<_, _>>(),
            )
        };

        let rt = program.eval_full_for_export(overrides)?;

        // We only add a trailing newline for JSON exports. Both YAML and TOML
        // exporters already append a trailing newline by default.
        let trailing_newline = self.format == ExportFormat::Json;

        serialize::validate(self.format, &rt)?;

        if let Some(file) = self.output {
            let mut file = fs::File::create(file).map_err(IOError::from)?;
            serialize::to_writer(&mut file, self.format, &rt)?;

            if trailing_newline {
                writeln!(file).map_err(IOError::from)?;
            }
        } else {
            serialize::to_writer(std::io::stdout(), self.format, &rt)?;

            if trailing_newline {
                println!();
            }
        }

        Ok(())
    }
}
