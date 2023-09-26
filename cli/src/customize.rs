use std::{
    collections::{BTreeMap, HashMap},
    ffi::OsString,
};

use clap::{parser::ValueSource, ArgAction, Command};
use nickel_lang_core::{
    combine::Combine,
    eval::cache::lazy::CBNCache,
    identifier::LocIdent,
    program::{FieldOverride, Program},
    repl::query_print::{write_query_result, Attributes},
    term::{
        record::{Field, RecordData},
        LabeledType, MergePriority, RuntimeContract, Term, TypeAnnotation,
    },
    typ::{RecordRowF, RecordRowsIteratorItem, Type, TypeF},
};

use crate::error::CliResult;

/// The maximal number of overridable fields displayed. Because there might be a lot of them, we
/// don't list them all by default.
const OVERRIDES_LIST_MAX_COUNT: usize = 15;

/// The value name used through the CLI to indicate that an option accepts any Nickel expression as
/// a value.
const NICKEL_VALUE_NAME: &str = "NICKEL EXPRESSION";

const EXPERIMENTAL_MSG: &str =
    "[WARNING] Customize mode is experimental. Its interface is subject to breaking changes.";

#[derive(clap::Parser, Debug)]
pub struct CustomizeMode {
    /// \[WARNING\] Customize mode is experimental. Its interface is subject to breaking changes.
    ///
    /// Customize mode turns the nickel invocation into a new CLI based on the configuration to be
    /// exported, where the value of fields can be set directly through CLI arguments. Print the
    /// corresponding help (`nickel export -f <file.ncl> -- --help`) to see available options.
    #[arg(last = true)]
    pub customize_mode: Vec<OsString>,
}
/// The interface of a configuration (a Nickel program) which represents all nested field paths
/// that are accessible from the root term together with their associated metadata.
///
/// Interface is used to derive a command-line interface from a configuration when using the
/// `customize_mode` option.
#[derive(Debug, Clone, Default)]
struct TermInterface {
    // We use a BTreeMap so that the end result is being sorted as we build the interface.
    fields: BTreeMap<LocIdent, FieldInterface>,
}

/// The interface of a specific input field.
#[derive(Debug, Clone, Default)]
struct FieldInterface {
    /// The interface of the subfields of this field, if it's a record itself.
    subfields: Option<TermInterface>,
    field: Field,
}

/// The interface of a non-input field (overridable).
#[derive(Debug, Clone, Default)]
struct OverrideInterface {
    /// The path of the overridable field.
    path: Vec<String>,
    /// An optional description, built from the field's metadata, and inserted into the general
    /// help message of the `--override` flag.
    // help isn't used yet, hence the leading underscore. We need to think first about how to
    // display it without cluttering the customize mode's help message.
    _help: Option<String>,
}

impl TermInterface {
    /// Create a new, empty interface.
    fn new() -> Self {
        Self::default()
    }

    /// Build a command description from this interface.
    ///
    /// This method recursively lists all existing field paths, and reports input fields (as
    /// defined per [FieldInterface::is_input]) as stand-alone arguments. For example, if the
    /// configuration contains:
    ///
    /// ```nickel
    /// foo.bar.baz | Number,
    /// ```
    ///
    /// This will give rise to a corresponding `--foo.bar.baz` argument listed in the `help` message
    /// which can be set.
    ///
    /// Non-input fields are still listed in the description of the `--override` command, which
    /// has the ability of setting any field.
    ///
    /// # Return
    ///
    /// In addition to the updated command, `build_cmd` returns a mapping from clap argument ids to
    /// their corresponding full field path as an array of fields.
    fn build_cmd(&self) -> TermCommand {
        let clap_cmd = Command::new("customize-mode")
            .about("Customize a Nickel configuration through the command line before exporting")
            .after_help(EXPERIMENTAL_MSG)
            .no_binary_name(true);

        let mut term_cmd = TermCommand::new(clap_cmd);

        for (id, field) in &self.fields {
            term_cmd = field.add_args(term_cmd, vec![id.to_string()])
        }

        let mut overrides_list: Vec<String> = term_cmd
            .overrides
            .keys()
            .take(OVERRIDES_LIST_MAX_COUNT)
            .map(|field_path| format!("- {field_path}"))
            .collect();

        if term_cmd.overrides.len() > OVERRIDES_LIST_MAX_COUNT {
            overrides_list.push("- ...".into());
        }

        let has_override = term_cmd.inputs.contains_key("override");
        let has_help = term_cmd.inputs.contains_key("help");

        let override_arg_label = "override";
        let override_help = format!(
            "Override any field of the configuration with a valid Nickel expression provided as \
            a string. The new value will be merged with the configuration with a `force` \
            priority.\n\n\
            Overridable fields:\n{}\n\n",
            overrides_list.join("\n")
        );
        let override_arg = clap::Arg::new(override_arg_label)
            .long(override_arg_label)
            .value_name(NICKEL_VALUE_NAME.to_owned())
            .number_of_values(2)
            .value_names(["field", "value"])
            .action(ArgAction::Append)
            .required(false)
            .help(override_help);

        term_cmd.clap_cmd = term_cmd.clap_cmd.arg(override_arg);

        if has_help || has_override {
            let conflict_field = if has_override {
                "override"
            } else if has_help {
                "help"
            } else {
                // We tested in the parent `if` that one of those two booleans must be set
                unreachable!()
            };

            let extra = if has_help && has_override {
                ". The same applies to the conflicting `help` field"
            } else {
                ""
            };

            term_cmd.clap_cmd = term_cmd.clap_cmd.after_long_help(format!(
                "\
            [CONFLICT] This configuration has a field named `{conflict_field}` which conflicts \
            with the built-in `--{conflict_field}` argument. To set this field to e.g. \
            \"some_value\", use `--override {conflict_field} \"some_value\"` instead of \
            `--{conflict_field} \"some_value\"`\
            {extra}\n\n\
            {EXPERIMENTAL_MSG}"
            ));
        }

        term_cmd
    }
}

impl Combine for TermInterface {
    fn combine(first: Self, second: Self) -> Self {
        let TermInterface { mut fields } = first;

        for (id, field) in second.fields.into_iter() {
            if let Some(prev) = fields.remove(&id) {
                fields.insert(id, Combine::combine(prev, field));
            } else {
                fields.insert(id, field);
            }
        }

        TermInterface { fields }
    }
}

impl Combine for FieldInterface {
    fn combine(first: Self, second: Self) -> Self {
        FieldInterface {
            subfields: Combine::combine(first.subfields, second.subfields),
            field: Field {
                metadata: Combine::combine(first.field.metadata, second.field.metadata),
                // Value is used only to show a default value, and to determine if a field has a
                // definition. We don't bother actually merging the content, but just keep any
                // side that is defined.
                value: first.field.value.or(second.field.value),
                ..Default::default()
            },
        }
    }
}

impl From<&RecordData> for TermInterface {
    fn from(value: &RecordData) -> Self {
        TermInterface {
            fields: value
                .fields
                .iter()
                .map(|(id, field)| (*id, field.into()))
                .collect(),
        }
    }
}

impl From<&Term> for TermInterface {
    fn from(term: &Term) -> Self {
        term.extract_interface().unwrap_or_else(TermInterface::new)
    }
}

trait ExtractInterface {
    fn extract_interface(&self) -> Option<TermInterface>;
}

impl ExtractInterface for &Term {
    fn extract_interface(&self) -> Option<TermInterface> {
        if let Term::Record(rd) = self {
            Some(TermInterface::from(rd))
        } else {
            None
        }
    }
}

impl ExtractInterface for Field {
    fn extract_interface(&self) -> Option<TermInterface> {
        self.value.as_ref().map(|t| TermInterface::from(t.as_ref()))
    }
}

impl ExtractInterface for Type {
    fn extract_interface(&self) -> Option<TermInterface> {
        match &self.typ {
            TypeF::Record(rrows) => Some(TermInterface {
                fields: rrows
                    .iter()
                    .filter_map(|item| {
                        if let RecordRowsIteratorItem::Row(rrow) = item {
                            Some((rrow.id, FieldInterface::from(&rrow)))
                        } else {
                            None
                        }
                    })
                    .collect(),
            }),
            // Contract information is already extracted from runtime contracts, which are
            // evaluated. Here, we focus on pure static type annotations and ignore flat types as
            // well
            _ => None,
        }
    }
}

impl ExtractInterface for RuntimeContract {
    fn extract_interface(&self) -> Option<TermInterface> {
        self.contract.as_ref().extract_interface()
    }
}

impl ExtractInterface for LabeledType {
    fn extract_interface(&self) -> Option<TermInterface> {
        self.typ.extract_interface()
    }
}

impl From<&Field> for FieldInterface {
    fn from(field: &Field) -> Self {
        // We collect field information from all the sources we can: static types and contracts
        // (both either as type annotations or contract annotations), and the value of the field if
        // it's a record.

        let subfields_from_types = field
            .pending_contracts
            .iter()
            .map(ExtractInterface::extract_interface);

        let subfields_from_contracts = field
            .metadata
            .annotation
            .iter()
            .map(ExtractInterface::extract_interface);

        let subfields_from_value = std::iter::once(field.extract_interface());

        let subfields = subfields_from_types
            .chain(subfields_from_contracts)
            .chain(subfields_from_value)
            .reduce(Combine::combine)
            .flatten();

        FieldInterface {
            subfields,
            field: field.clone(),
        }
    }
}

impl From<&RecordRowF<&Type>> for FieldInterface {
    fn from(rrow: &RecordRowF<&Type>) -> Self {
        FieldInterface {
            subfields: rrow.typ.extract_interface(),
            ..Default::default()
        }
    }
}

impl FieldInterface {
    /// Take a clap command and enrich it with either one argument if this subfield is an input
    /// field, or with all the subfields that are inputs. If this field or some of its subfields
    /// aren't inputs, they are pushed to `overrides`. See [TermInterface::build_cmd].
    ///
    /// `add_args` updates `paths` as well, which maps clap argument ids to the corresponding field
    /// path represented as an array of string names.
    fn add_args(&self, mut term_cmd: TermCommand, path: Vec<String>) -> TermCommand {
        // This is a terminal field, which gives rise to an argument or an overridable value.
        if !self.has_subfields() {
            if self.is_input() {
                match path.as_slice() {
                    [ref field] if field == "override" || field == "help" => {
                        term_cmd.register_input(path.clone())
                    }
                    _ => {
                        term_cmd.register_arg(path.clone());
                        term_cmd.clap_cmd = self.add_arg(term_cmd.clap_cmd, path.join("."))
                    }
                };
            } else {
                term_cmd.register_override(path.clone(), self.help());
            }
        }

        for (id, field) in self.subfields.iter().flat_map(|intf| intf.fields.iter()) {
            let mut path = path.clone();
            path.push(id.to_string());

            term_cmd = field.add_args(term_cmd, path);
        }

        term_cmd
    }

    /// Add a single argument to the CLI `cmd`, based on the interface of this field, and return
    /// the updated command.
    fn add_arg(&self, cmd: clap::Command, path: String) -> clap::Command {
        let mut arg = clap::Arg::new(&path)
            .long(path)
            .value_name(get_value_name(&self.field.metadata.annotation))
            // TODO: Create clap argument groups
            .required(!self.is_default());

        if let Some(help) = &self.help() {
            arg = arg.help(help);
        };

        if let Some(default) = self.default_value() {
            arg = arg.default_value(default);
        }

        cmd.arg(arg)
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
        !self.is_defined() || self.is_default()
    }

    /// Return `true` is the field has a value.
    fn is_defined(&self) -> bool {
        self.field.value.is_some()
    }

    /// Return true is the field's merge priority is `default`.
    fn is_default(&self) -> bool {
        matches!(self.field.metadata.priority, MergePriority::Bottom)
    }

    fn has_subfields(&self) -> bool {
        matches!(&self.subfields, Some(ref intf) if !intf.fields.is_empty())
    }

    /// Return the default value, if any.
    fn default_value(&self) -> Option<String> {
        match (&self.field.metadata.priority, &self.field.value) {
            (MergePriority::Bottom, Some(value)) => Some(value.to_string()),
            _ => None,
        }
    }

    /// Render a help message similar to the output of a metadata query to serve as an help text
    /// for this argument.
    fn help(&self) -> Option<String> {
        let mut output: Vec<u8> = Vec::new();

        // We only need to render the documentation: the rest is printed separately as part of the
        // clap command that is built.
        let attributes = Attributes {
            doc: true,
            contract: false,
            typ: false,
            default: false,
            value: false,
        };

        write_query_result(&mut output, &self.field, attributes)
            .unwrap_or(false)
            .then(|| {
                String::from_utf8(output)
                    .expect("the query printer should always output valid utf8")
            })
    }
}

fn get_value_name(annotation: &TypeAnnotation) -> String {
    if annotation.is_empty() {
        NICKEL_VALUE_NAME.into()
    } else {
        let anns: Vec<String> = annotation
            .iter()
            .map(|ctr| ctr.label.typ.to_string())
            .collect();
        anns.join(",")
    }
}

/// A command dynamically built from a Nickel term.
struct TermCommand {
    /// The corresponding clap command
    clap_cmd: clap::Command,
    /// A mapping between clap argument ids and the corresponding field path (for inputs).
    args: HashMap<clap::Id, Vec<String>>,
    /// Mapping between existing input fields (as a dot-separated path string) and their path
    /// represented as a vector. This is used both as a `HashSet` to check the existence of an
    /// input field, and to sidestep the issue of parsing a path again when reading an input
    /// argument.
    inputs: HashMap<String, Vec<String>>,
    /// A mapping between a field path and the corresponding override data.
    overrides: BTreeMap<String, OverrideInterface>,
}

impl TermCommand {
    fn new(command: clap::Command) -> Self {
        TermCommand {
            clap_cmd: command,
            overrides: BTreeMap::new(),
            inputs: HashMap::new(),
            args: HashMap::new(),
        }
    }

    /// Register a new argument corresponding to an input. Add the paths and the argument's id to
    /// the relevant maps.
    fn register_arg(&mut self, path: Vec<String>) {
        let path_string = path.join(".");
        let arg_id = clap::Id::from(path_string.clone());

        debug_assert!(!self.args.contains_key(&arg_id));
        debug_assert!(!self.inputs.contains_key(&path_string));

        self.args.insert(arg_id, path.clone());
        self.inputs.insert(path_string, path);
    }

    /// Register an input, but don't register the corresponding CLI argument. Used for `override`
    /// and `help` which conflict with the built-in `--override` and `--help` arguments.
    fn register_input(&mut self, path: Vec<String>) {
        let path_string = path.join(".");

        debug_assert!(!self.inputs.contains_key(&path_string));

        self.inputs.insert(path_string, path);
    }

    fn register_override(&mut self, path: Vec<String>, help: Option<String>) {
        let path_string = path.join(".");

        self.overrides
            .insert(path_string, OverrideInterface { path, _help: help });
    }
}

pub trait Customize {
    fn customize(&self, program: Program<CBNCache>) -> CliResult<Program<CBNCache>>;
}

impl Customize for CustomizeMode {
    // XXX: we should give a nice error message when someone tries to evaluate some
    //      expression that has unset values, telling them they can set them using
    //      this method
    fn customize(&self, mut program: Program<CBNCache>) -> CliResult<Program<CBNCache>> {
        if self.customize_mode.is_empty() {
            return Ok(program);
        }

        let evaled = match program.eval_record_spine() {
            Ok(evaled) => evaled,
            // We need a `return` control-flow to be able to take `program` out
            Err(error) => return CliResult::Err(crate::error::Error::Program { error, program }),
        };

        let cmd = TermInterface::from(evaled.as_ref()).build_cmd();
        let arg_matches = cmd.clap_cmd.get_matches_from(self.customize_mode.iter());

        let force_overrides: Result<Vec<_>, super::error::CliUsageError> = arg_matches
            .get_occurrences("override")
            .into_iter()
            .flat_map(|occurs| {
                occurs.map(|mut pair| {
                    let (path, value): (&String, &String) =
                        (pair.next().unwrap(), pair.next().unwrap());

                    cmd.overrides
                        .get(path)
                        .map(|intf| intf.path.clone())
                        // We allow --override to set inputs as well, in particular as an
                        // escape mechanism to set an input field named `override` or `help`,
                        // which would conflict with the corresponding builtin flags.
                        .or(cmd.inputs.get(path).cloned())
                        .map(|path| FieldOverride {
                            path,
                            value: value.clone(),
                            priority: MergePriority::Top,
                        })
                        .ok_or_else(|| super::error::CliUsageError::InvalidOverride {
                            path: path.clone(),
                        })
                })
            })
            .collect();

        let force_overrides = match force_overrides {
            Ok(force_overrides) => force_overrides,
            Err(error) => return CliResult::Err(crate::error::Error::CliUsage { error, program }),
        };

        program.add_overrides(
            arg_matches
                .ids()
                .filter_map(|id| -> Option<FieldOverride> {
                    (!matches!(
                        arg_matches.value_source(id.as_str()),
                        Some(ValueSource::DefaultValue)
                    ) && id.as_str() != "override")
                        .then(|| FieldOverride {
                            path: cmd.args.get(id).unwrap().clone(),
                            value: arg_matches.get_one::<String>(id.as_str()).unwrap().clone(),
                            priority: MergePriority::default(),
                        })
                })
                .chain(force_overrides),
        );
        Ok(program)
    }
}

#[derive(clap::Args, Debug)]
pub struct NoCustomizeMode;

impl Customize for NoCustomizeMode {
    fn customize(&self, program: Program<CBNCache>) -> CliResult<Program<CBNCache>> {
        Ok(program)
    }
}
