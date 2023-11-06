//! Customize mode parser and evaluation logic.
//!
//! The customize mode is a way to interact with a specific Nickel program on the command line by
//! setting or overriding values directly from the command line. It's currently enabled by the
//! freeform marker `--`.
use std::{collections::HashMap, ffi::OsString};

use clap::Parser;
use nickel_lang_core::{
    combine::Combine,
    eval::cache::lazy::CBNCache,
    identifier::LocIdent,
    program::{FieldPath, Program},
    repl::query_print,
    repl::query_print::{write_query_result, Attributes},
    term::{
        record::{Field, RecordData},
        LabeledType, MergePriority, RuntimeContract, Term,
    },
    typ::{RecordRowF, RecordRowsIteratorItem, Type, TypeF},
};

use crate::error::CliResult;

pub mod interface;

use interface::{CustomizableField, FieldInterface, TermInterface};

/// The value name used through the CLI to indicate that an option accepts any Nickel expression as
/// a value.
const ASSIGNMENT_SYNTAX: &str = "FIELD_PATH=NICKEL_EXPRESSION";

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

#[derive(clap::Parser)]
#[command(
    name = "customize-mode",
    after_help = EXPERIMENTAL_MSG,
    no_binary_name = true,
)]
/// Customize a Nickel configuration through the command line.
struct CustomizeOptions {
    /// Assign a valid Nickel expression to an input field of the configuration. The new value
    /// will be merged with the configuration with priority 0 (the one assigned by default in
    /// Nickel when no explicit merge priority is provided).
    ///
    /// Assignment can only set input fields, that is fields without definition or fields with
    /// a default value. To override an existing value, use `--override` instead.
    ///
    /// Note that you might have to escape special characters or enclose assignments in quotes to
    /// prevent shell interpretation.
    ///
    /// Example: `nickel eval config.ncl -- 'http.enabled="yes"' protocol=\'ftp`
    #[clap(value_name = ASSIGNMENT_SYNTAX)]
    assignment: Vec<String>,

    /// Override any field of the configuration with a valid Nickel expression. The new value
    /// will be merged with the configuration with a `force` priority.
    ///
    /// Note that you might have to escape special characters or enclose assignments in quotes to
    /// prevent shell interpretation.
    ///
    /// Example: `-- input.value=false --override m.count=2 --override m.type=\'server`
    #[clap(long = "override", value_name = ASSIGNMENT_SYNTAX)]
    ovd: Vec<String>,

    #[command(subcommand)]
    pub command: Option<CustomizeCommand>,
}

#[derive(clap::Subcommand)]
enum CustomizeCommand {
    /// Show the documentation of a particular field.
    Show(ShowCommand),
    /// List the input fields and the overridable fields of the configuration.
    List(ListCommand),
}

#[derive(clap::Parser)]
struct ListCommand;

impl ListCommand {
    fn run(self, customize_fields: &CustomizableFields) -> CliResult<()> {
        let mut inputs = customize_fields
            .inputs
            .values()
            .map(|field| field.path.to_string())
            .collect::<Vec<_>>();
        inputs.sort();

        let mut overrides = customize_fields
            .overrides
            .values()
            .map(|field| field.path.to_string())
            .collect::<Vec<_>>();
        overrides.sort();

        println!("Input fields:");
        for input in inputs {
            println!("- {}", input);
        }

        println!("\nOverridable fields (require `--override`):");
        for override_ in overrides {
            println!("- {}", override_);
        }

        Ok(())
    }
}

#[derive(clap::Parser)]
struct DoCommand {
    /// Assign a valid Nickel expression to an input field of the configuration. The new value
    /// will be merged with the configuration with priority 0 (the one assigned by default in
    /// Nickel when no explicit merge priority is provided).
    ///
    /// Assignment can only set input fields, that is fields without definition or fields with
    /// a default value. To override an existing value, use `--override` instead.
    #[clap(value_name = ASSIGNMENT_SYNTAX)]
    assignment: Vec<String>,

    /// Override any field of the configuration with a valid Nickel expression. The new value
    /// will be merged with the configuration with a `force` priority.
    #[clap(long = "override", value_name = ASSIGNMENT_SYNTAX)]
    ovd: Vec<String>,
}

impl DoCommand {}

#[derive(clap::Parser)]
struct ShowCommand {
    /// Print the documentation and metadata of a particular overridable field.
    #[clap(value_name = "FIELD_PATH")]
    field: String,
}

impl ShowCommand {
    fn run(
        self,
        mut program: Program<CBNCache>,
        customizable_fields: &CustomizableFields,
    ) -> CliResult<()> {
        let path = match program.parse_field_path(self.field) {
            Ok(path) => path,
            Err(parse_error) => {
                return CliResult::Err(crate::error::Error::CliUsage {
                    error: crate::error::CliUsageError::FieldPathParseError { error: parse_error },
                    program,
                })
            }
        };

        let field = match customizable_fields
            .inputs
            .get(&path)
            .or(customizable_fields.overrides.get(&path))
        {
            Some(field) => &field.interface.field,
            None => {
                return CliResult::Err(crate::error::Error::CliUsage {
                    error: crate::error::CliUsageError::UnknownField { path },
                    program,
                })
            }
        };

        query_print::write_query_result(&mut std::io::stdout(), field, Default::default()).unwrap();

        Ok(())
    }
}

/// Fields of the configuration which aren't themselves records and can be assigned or overridden
/// through the customize mode.
#[derive(Clone, Debug, Default)]
struct CustomizableFields {
    inputs: HashMap<FieldPath, CustomizableField>,
    overrides: HashMap<FieldPath, CustomizableField>,
}

impl CustomizableFields {
    /// Create data from a term interface.
    fn new(term_iface: TermInterface) -> Self {
        let mut this = Self::default();

        for (id, field_iface) in term_iface.fields {
            this.extend_with(field_iface, FieldPath(vec![id]));
        }

        this
    }

    /// Register a customizable field that can be assigned (input).
    fn register_input(&mut self, path: FieldPath, interface: FieldInterface) {
        debug_assert!(!self.inputs.contains_key(&path));
        debug_assert!(!self.overrides.contains_key(&path));

        self.inputs
            .insert(path.clone(), CustomizableField { path, interface });
    }

    /// Register a customizable field that can be overridden.
    fn register_override(&mut self, path: FieldPath, interface: FieldInterface) {
        debug_assert!(!self.inputs.contains_key(&path));
        debug_assert!(!self.overrides.contains_key(&path));

        self.overrides
            .insert(path.clone(), CustomizableField { path, interface });
    }

    /// Enrich the current list of customizable fields with the field(s) exposed by a given field
    /// interface.
    fn extend_with(&mut self, field_iface: FieldInterface, path: FieldPath) {
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
                path.0.push(id);

                self.extend_with(child_iface, path);
            }
        }
    }
}

pub trait Customize {
    fn customize(&self, program: Program<CBNCache>) -> CliResult<Program<CBNCache>>;
}

impl CustomizeOptions {
    fn do_customize(
        self,
        customizable_fields: CustomizableFields,
        mut program: Program<CBNCache>,
    ) -> CliResult<Program<CBNCache>> {
        let assignment_overrides: Result<Vec<_>, super::error::CliUsageError> = self
            .assignment
            .into_iter()
            .map(|assignment| {
                let ovd = program
                    .parse_override(assignment, MergePriority::default())
                    .map_err(|error| super::error::CliUsageError::AssignmentParseError { error })?;

                if !customizable_fields.inputs.contains_key(&ovd.path) {
                    if customizable_fields.overrides.contains_key(&ovd.path) {
                        Err(super::error::CliUsageError::CantAssignNonInput { ovd })
                    } else {
                        Err(super::error::CliUsageError::UnknownFieldAssignment { path: ovd.path })
                    }
                } else {
                    Ok(ovd)
                }
            })
            .collect();

        let assignment_overrides = match assignment_overrides {
            Ok(assignment_overrides) => assignment_overrides,
            Err(error) => return CliResult::Err(crate::error::Error::CliUsage { error, program }),
        };

        let force_overrides: Result<Vec<_>, super::error::CliUsageError> = self
            .ovd
            .into_iter()
            .map(|assignment| {
                let ovd = program
                    .parse_override(assignment, MergePriority::Top)
                    .map_err(|error| super::error::CliUsageError::AssignmentParseError { error })?;

                if !customizable_fields.inputs.contains_key(&ovd.path)
                    && !customizable_fields.overrides.contains_key(&ovd.path)
                {
                    Err(super::error::CliUsageError::UnknownFieldOverride { path: ovd.path })
                } else {
                    Ok(ovd)
                }
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
        let opts = CustomizeOptions::parse_from(self.customize_mode.iter());

        match opts.command {
            None => opts.do_customize(customizable_fields, program),
            Some(CustomizeCommand::List(list_command)) => {
                list_command.run(&customizable_fields)?;
                Err(crate::error::Error::CustomizeInfoPrinted)
            }
            Some(CustomizeCommand::Show(show_command)) => {
                show_command.run(program, &customizable_fields)?;
                Err(crate::error::Error::CustomizeInfoPrinted)
            }
        }
    }
}

#[derive(clap::Args, Debug)]
pub struct NoCustomizeMode;

impl Customize for NoCustomizeMode {
    fn customize(&self, program: Program<CBNCache>) -> CliResult<Program<CBNCache>> {
        Ok(program)
    }
}
