//! Load the Nickel standard library in strings at compile-time.

use crate::identifier::Ident;
use crate::term::make as mk_term;
use crate::term::RichTerm;

/// This is an array containing all the Nickel standard library modules.
pub fn modules() -> [StdlibModule; 9] {
    [
        StdlibModule::Builtin,
        StdlibModule::Contract,
        StdlibModule::Array,
        StdlibModule::Record,
        StdlibModule::String,
        StdlibModule::Number,
        StdlibModule::Function,
        StdlibModule::Enum,
        StdlibModule::Internals,
        StdlibModule::Compat,
    ]
}

/// Represents a particular Nickel standard library module.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum StdlibModule {
    Builtin,
    Contract,
    Array,
    Record,
    String,
    Number,
    Function,
    Enum,
    Internals,
    Compat,
}

impl StdlibModule {
    pub fn file_name(&self) -> &'static str {
        match self {
            StdlibModule::Builtin => "<stdlib/builtin.ncl>",
            StdlibModule::Contract => "<stdlib/contract.ncl>",
            StdlibModule::Array => "<stdlib/array.ncl>",
            StdlibModule::Record => "<stdlib/record.ncl>",
            StdlibModule::String => "<stdlib/string.ncl>",
            StdlibModule::Number => "<stdlib/number.ncl>",
            StdlibModule::Function => "<stdlib/function.ncl>",
            StdlibModule::Enum => "<stdlib/enum.ncl>",
            StdlibModule::Internals => "<stdlib/internals.ncl>",
            StdlibModule::Compat => "<stdlib/compat.ncl>",
        }
    }

    pub const fn content(&self) -> &'static str {
        match self {
            StdlibModule::Builtin => include_str!("../stdlib/builtin.ncl"),
            StdlibModule::Contract => include_str!("../stdlib/contract.ncl"),
            StdlibModule::Array => include_str!("../stdlib/array.ncl"),
            StdlibModule::Record => include_str!("../stdlib/record.ncl"),
            StdlibModule::String => include_str!("../stdlib/string.ncl"),
            StdlibModule::Number => include_str!("../stdlib/number.ncl"),
            StdlibModule::Function => include_str!("../stdlib/function.ncl"),
            StdlibModule::Enum => include_str!("../stdlib/enum.ncl"),
            StdlibModule::Internals => include_str!("../stdlib/internals.ncl"),
            StdlibModule::Compat => include_str!("../stdlib/compat.ncl"),
        }
    }
}

pub struct UnknownStdlibModule;

impl TryFrom<Ident> for StdlibModule {
    type Error = UnknownStdlibModule;

    fn try_from(value: Ident) -> Result<Self, Self::Error> {
        let module = match value.label() {
            "builtin" => StdlibModule::Builtin,
            "contract" => StdlibModule::Contract,
            "array" => StdlibModule::Array,
            "record" => StdlibModule::Record,
            "string" => StdlibModule::String,
            "number" => StdlibModule::Number,
            "function" => StdlibModule::Function,
            "enum" => StdlibModule::Enum,
            "internals" => StdlibModule::Internals,
            "compat" => StdlibModule::Compat,
            _ => return Err(UnknownStdlibModule),
        };
        Ok(module)
    }
}

impl From<StdlibModule> for Ident {
    fn from(module: StdlibModule) -> Self {
        let name = match module {
            StdlibModule::Builtin => "builtin",
            StdlibModule::Contract => "contract",
            StdlibModule::Array => "array",
            StdlibModule::Record => "record",
            StdlibModule::String => "string",
            StdlibModule::Number => "number",
            StdlibModule::Function => "function",
            StdlibModule::Enum => "enum",
            StdlibModule::Internals => "internals",
            StdlibModule::Compat => "Compat",
        };
        Ident::from(name)
    }
}

macro_rules! generate_accessor {
    ($value:ident) => {
        pub fn $value() -> RichTerm {
            mk_term::var(format!("${}", stringify!($value)))
        }
    };
}

/// Accessors to the builtin contracts and other internals that aren't accessible from user code.
pub mod internals {
    use super::*;

    // `dyn` is a reserved keyword in rust
    pub fn dynamic() -> RichTerm {
        mk_term::var("$dyn")
    }

    generate_accessor!(num);
    generate_accessor!(bool);
    generate_accessor!(string);
    generate_accessor!(array);
    generate_accessor!(func);
    generate_accessor!(forall_var);
    generate_accessor!(forall);
    generate_accessor!(fail);
    generate_accessor!(enums);
    generate_accessor!(enum_fail);
    generate_accessor!(record);
    generate_accessor!(dyn_record);
    generate_accessor!(record_extend);
    generate_accessor!(forall_tail);
    generate_accessor!(dyn_tail);
    generate_accessor!(empty_tail);
    generate_accessor!(contract_equal);

    generate_accessor!(rec_default);
    generate_accessor!(rec_force);
}

/// Contains functions helper for Nix evaluation by Nickel.
pub mod compat {
    use super::*;
    use crate::mk_app;
    use crate::term::make::op1;
    use crate::term::{array::Array, Term, UnaryOp};

    /// helper function to perform a Nix like update (`//` operator).
    pub fn update() -> RichTerm {
        op1(
            UnaryOp::StaticAccess("update_all".into()),
            Term::Var("compat".into()),
        )
    }

    /// helper function to check if a record has a nested field.
    pub fn has_field_path() -> RichTerm {
        op1(
            UnaryOp::StaticAccess("has_field_path".into()),
            Term::Var("compat".into()),
        )
    }

    /// Generate the `with` compatibility Nickel function which may be applied to an `Ident`
    /// you have to pass a list of with records in ordered from outer-most to inner-most one.
    pub fn with(array: Array) -> RichTerm {
        mk_app!(
            op1(
                UnaryOp::StaticAccess("with".into()),
                Term::Var("compat".into()),
            ),
            Term::Array(array, Default::default())
        )
    }

    pub fn add() -> RichTerm {
        op1(
            UnaryOp::StaticAccess("add".into()),
            Term::Var("compat".into()),
        )
    }
}
