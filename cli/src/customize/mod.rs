//! Customize mode parser and evaluation logic.
//!
//! The customize mode is a way to interact with a specific Nickel program on the command line by
//! setting or overriding values directly from the command line. It's currently enabled by the
//! freeform marker `--`.
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

pub mod interface;

use interface::{CustomizableField, FieldInterface, TermInterface};

/// The maximal number of overridable fields displayed. Because there might be a lot of them, we
/// don't list them all by default.
const OVERRIDES_LIST_MAX_COUNT: usize = 15;

/// The value name used through the CLI to indicate that an option accepts any Nickel expression as
/// a value.
const ASSIGNMENT_SYNTAX: &str = "FIELD_PATH=NICKEL EXPRESSION";

const EXPERIMENTAL_MSG: &str =
    "[WARNING] Customize mode is experimental. Its interface is subject to breaking changes.";

#[derive(clap::Parser, Debug)]
pub struct CustomizeMode {
    /// \[WARNING\] Customize mode is experimental. Its interface is subject to breaking changes.
    ///
    /// Customize mode turns the nickel invocation into a new CLI based on the configuration to be
    /// exported, where the value of fields can be set directly through CLI arguments. Print the
    /// corresponding help (`nickel export <file.ncl> -- --help`) to see available options.
    #[arg(last = true)]
    pub customize_mode: Vec<OsString>,
}

impl CustomizeMode {
    fn make_cmd() -> clap::Command {
        let assignment = clap::Arg::new("assignment").action(ArgAction::Append);

        let override_arg = clap::Arg::new("override")
            .long("override")
            .value_name(ASSIGNMENT_SYNTAX.to_owned())
            .action(ArgAction::Append)
            .help(
                "\
                Override any field of the configuration with a valid Nickel expression provided as \
                a string. The new value will be merged with the configuration with a `force` \
                priority.",
            );

        Command::new("customize-mode")
            .about("Customize a Nickel configuration through the command line before exporting")
            .after_help(EXPERIMENTAL_MSG)
            .no_binary_name(true)
            .arg(override_arg)
    }
}

/// An assignment of the form `path.to.field=value`.
struct FieldAssignment {
    path: Vec<String>,
    value: String,
}

/// Fields of the configuration which aren't themselves records and can be assigned or overridden
/// through the customize mode.
#[derive(Clone, Debug, Default)]
struct CustomizableFields {
    inputs: BTreeMap<Vec<LocIdent>, CustomizableField>,
    overrides: BTreeMap<Vec<LocIdent>, CustomizableField>,
}

impl CustomizableFields {
    /// Create data from a term interface.
    fn new(term_iface: TermInterface) -> Self {
        let mut this = Self::default();

        for (id, field_iface) in term_iface.fields {
            this.extend_with(field_iface, vec![id]);
        }

        this
    }

    /// Register a customizable field that can be assigned (input).
    fn register_input(&mut self, path: Vec<LocIdent>, interface: FieldInterface) {
        debug_assert!(!self.inputs.contains_key(&path));
        debug_assert!(!self.overrides.contains_key(&path));

        self.inputs
            .insert(path.clone(), CustomizableField { path, interface });
    }

    /// Register a customizable field that can be overridden.
    fn register_override(&mut self, path: Vec<LocIdent>, interface: FieldInterface) {
        debug_assert!(!self.inputs.contains_key(&path));
        debug_assert!(!self.overrides.contains_key(&path));

        self.overrides
            .insert(path.clone(), CustomizableField { path, interface });
    }

    /// Enrich the current list of customizable fields with the field(s) exposed by a given field
    /// interface.
    fn extend_with(&mut self, field_iface: FieldInterface, path: Vec<LocIdent>) {
        // This is a terminal field, which gives rise to an argument or an overridable value.
        if !field_iface.has_subfields() {
            if field_iface.is_input() {
                self.register_input(path.clone(), field_iface);
            } else {
                self.register_override(path.clone(), field_iface);
            }
        } else {
            for (id, child_iface) in field_iface
                .subfields
                .into_iter()
                .flat_map(|intf| intf.fields.into_iter())
            {
                let mut path = path.clone();
                path.push(id);

                self.extend_with(child_iface, path);
            }
        }
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

        let customizable_fields = CustomizableFields::new(TermInterface::from(evaled.as_ref()));
        let clap_cmd = Self::make_cmd();
        let arg_matches = clap_cmd.get_matches_from(self.customize_mode.iter());

        let assignment_overrides: Result<Vec<_>, super::error::CliUsageError> = arg_matches
            .get_occurrences("assignment")
            .into_iter()
            .flat_map(|occurs| {
                occurs.map(|mut occurrence_value| {
                    let assignment: &String = occurrence_value.next().unwrap();
                    let (path_str, value_str): (&String, &String) = todo!();
                    let path: Vec<LocIdent> = todo!();

                    customizable_fields
                        .inputs
                        .get(&path)
                        .map(|intf| FieldOverride {
                            path: intf.path.clone(),
                            value: value_str.clone(),
                            priority: MergePriority::default(),
                        })
                        .ok_or_else(|| {
                            if customizable_fields.inputs.contains_key(&path) {
                                super::error::CliUsageError::CantAssignNonInput {
                                    path: path_str.clone(),
                                    assignment: todo!(),
                                }
                            } else {
                                super::error::CliUsageError::UnknownFieldOverride {
                                    path: path_str.clone(),
                                }
                            }
                        })
                })
            })
            .collect();

        let assignment_overrides = match assignment_overrides {
            Ok(assignment_overrides) => assignment_overrides,
            Err(error) => return CliResult::Err(crate::error::Error::CliUsage { error, program }),
        };

        let force_overrides: Result<Vec<_>, super::error::CliUsageError> = arg_matches
            .get_occurrences("override")
            .into_iter()
            .flat_map(|occurs| {
                occurs.map(|mut pair| {
                    let (path_str, value_str): (&String, &String) =
                        (pair.next().unwrap(), pair.next().unwrap());
                    let path: Vec<LocIdent> = todo!();

                    customizable_fields
                        .overrides
                        .get(&path)
                        // We allow --override to set inputs as well.
                        .or(customizable_fields.inputs.get(&path))
                        .map(|intf| FieldOverride {
                            path: intf.path.clone(),
                            value: value_str.clone(),
                            priority: MergePriority::Top,
                        })
                        .ok_or_else(|| super::error::CliUsageError::UnknownFieldOverride {
                            path: path_str.clone(),
                        })
                })
            })
            .collect();

        let force_overrides = match force_overrides {
            Ok(force_overrides) => force_overrides,
            Err(error) => return CliResult::Err(crate::error::Error::CliUsage { error, program }),
        };

        program.add_overrides(assignment_overrides.into_iter().chain(force_overrides));
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
