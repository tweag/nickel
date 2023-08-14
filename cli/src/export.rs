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

/// Interface of a configuration (in practice, a Nickel program), representing all nested fields
/// that can be reached with a field path from the root together with their associated metadata.
///
/// Interface is used to derive a command-line interface from a configuration when using the
/// `freeform` option.
#[derive(Debug, Clone, Default)]
struct Interface {
    fields: BTreeMap<Ident, InterfaceField>,
}

#[derive(Debug, Clone, Default)]
struct InterfaceAnnotation {
    typ: Option<Type>,
    contracts: Vec<Type>,
}

#[derive(Debug, Clone, Default)]
struct InterfaceField {
    /// The interface of the subfields of this field, if it's a record itself.
    subfields: Option<Interface>,
    documentation: Option<String>,
    /// A string representation of the default value for this field, if any.
    default: Option<String>,
    opt: bool,
    /// If the corresponding record field has an attached value or not.
    is_defined: bool,
    annotation: InterfaceAnnotation,
}

/// A trait for things that can be merged.
trait Merge {
    fn merge(first: Self, second: Self) -> Self;
}

impl Interface {
    /// Create a new, empty interface.
    fn new() -> Self {
        Self::default()
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

impl<T: Merge> Merge for Option<T> {
    fn merge(first: Self, second: Self) -> Self {
        match (first, second) {
            (None, maybe_defn) | (maybe_defn, None) => maybe_defn,
            (Some(defn1), Some(defn2)) => Some(Merge::merge(defn1, defn2)),
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
            opt: first.opt && second.opt,
            is_defined: first.is_defined || second.is_defined,
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
        term.extract_interface().unwrap_or_else(Interface::new)
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
            default: if let MergePriority::Bottom = field.metadata.priority {
                field.value.as_ref().map(RichTerm::to_string)
            } else {
                None
            },
            opt: field.metadata.opt,
            is_defined: field.value.is_some(),
            annotation: InterfaceAnnotation::from(&field.metadata.annotation),
        }
    }
}

impl From<&RecordRowF<&Type>> for InterfaceField {
    fn from(rrow: &RecordRowF<&Type>) -> Self {
        InterfaceField {
            subfields: rrow.typ.extract_interface(),
            annotation: InterfaceAnnotation {
                typ: Some(rrow.typ.clone()),
                contracts: vec![],
            },
            ..Default::default()
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
    /// Take a clap command and enrich it with all the input fields defined by this record
    /// (including subfields). See [build_clap].
    ///
    /// Doing so, `add_args` updates `paths`, which is mapping from clap argument ids to the
    /// corresponding field path represented as an array of field names.
    fn add_args(
        &self,
        cmd: clap::Command,
        path: Vec<String>,
        paths: &mut HashMap<clap::Id, Vec<String>>,
    ) -> clap::Command {
        let id = path.join(".");
        let prev = paths.insert(clap::Id::from(&id), path.clone());
        debug_assert!(matches!(prev, None));

        let mut arg = clap::Arg::new(&id)
            .long(id)
            .value_name(get_value_name(&self.annotation))
            // TODO: Create clap argument groups
            .required(!self.opt);

        if let Some(doc) = &self.documentation {
            arg = arg.help(doc);
        };

        if let Some(default) = &self.default {
            arg = arg.default_value(default);
        };

        let mut cmd = cmd.arg(arg);

        for (id, field) in self.subfields.iter().flat_map(|intf| intf.fields.iter()) {
            let mut path = path.clone();
            path.push(id.to_string());
            cmd = field.add_args(cmd, path, paths);
        }

        cmd
    }

    /// Define if a field is an input of a configuration that is intended to be filled, and will be
    /// given a dedicated CLI argument. If the field is not an input, it can only be overridden via
    /// `--override`.
    ///
    /// Currently, the difference is mostly conceptual: in practice, we simply gather the arguments
    /// and their values, either via direct arguments or `--override` (although the latter sets a
    /// different priority), and then merge the elaborated record with original term.
    ///
    /// However, from a user experience point of view, we want to make a clear distinction between
    /// filling a bespoke input of a configuration and overriding an existing value. The latter is
    /// more "low-level" and should only be done knowingly.
    ///
    /// Currently, the logic is simply that a field is an input if it doesn't have a definition or
    /// it has a default priority. This definition might evolve, as there are ongoing discussions
    /// on what is should be the meaning of "input", "output", and if those concept should be made
    /// first-class ([related issue](https://github.com/tweag/nickel/issues/1505)). For now, this
    /// logic seems to be a reasonable first approximation.
    fn is_input(&self) -> bool {
        !self.is_defined || self.default.is_some()
    }
}

fn get_value_name(_annotation: &InterfaceAnnotation) -> String {
    "STRING".to_owned()
}

/// Build a clap command description from the interface of a configuration.
///
/// This method recursively lists all existing field paths, and report input fields (as defined per
/// [Interface::is_input]) as stand-alone arguments. For example, if there the configuration
/// contains:
///
/// ```nickel
/// foo.bar.baz | Number,
/// ```
///
/// This will give rise to a corresponding `--foo.bar.baz` argument listed in the `help` message
/// which can be set.
///
/// Non-inputs fields are still listed in the description of the `--override` command, which has
/// the ability of setting any field.
///
/// # Return
///
/// In addition to the updated command, `build_clap` returns a mapping from clap argument ids to
/// their corresponding full field path as an array of fields.
fn build_clap(
    mut cmd: clap::Command,
    interface: &Interface,
) -> (clap::Command, HashMap<clap::Id, Vec<String>>) {
    let mut paths = HashMap::new();

    for (id, field) in &interface.fields {
        cmd = field.add_args(cmd, vec![id.to_string()], &mut paths)
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
            let evaled = program.eval_record_spine()?;

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
