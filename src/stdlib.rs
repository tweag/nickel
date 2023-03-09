//! Load the Nickel standard library in strings at compile-time.

use crate::identifier::Ident;
use crate::term::make as mk_term;
use crate::term::RichTerm;

/// This is an array containing all the Nickel standard library modules.
pub fn modules() -> [StdlibModule; 8] {
    [
        StdlibModule::Builtin,
        StdlibModule::Contract,
        StdlibModule::Array,
        StdlibModule::Record,
        StdlibModule::String,
        StdlibModule::Number,
        StdlibModule::Function,
        StdlibModule::Internals,
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
    Internals,
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
            StdlibModule::Internals => "<stdlib/internals.ncl>",
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
            StdlibModule::Internals => include_str!("../stdlib/internals.ncl"),
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
            "internals" => StdlibModule::Internals,
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
            StdlibModule::Internals => "internals",
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

/// Accessors to the builtin contracts.
pub mod contract {
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
    generate_accessor!(fail);
    generate_accessor!(enums);
    generate_accessor!(enum_fail);
    generate_accessor!(record);
    generate_accessor!(dyn_record);
    generate_accessor!(record_extend);
    generate_accessor!(forall_tail);
    generate_accessor!(dyn_tail);
    generate_accessor!(empty_tail);
}

pub mod internals {
    use super::*;

    generate_accessor!(rec_default);
    generate_accessor!(rec_force);
}
