//! Load the Nickel standard library in strings at compile-time.

use crate::identifier::Ident;
use crate::term::make as mk_term;
use crate::term::RichTerm;

pub const BUILTIN: (StdlibModule, &str, &str) = (
    StdlibModule::Builtin,
    "<stdlib/builtin.ncl>",
    include_str!("../stdlib/builtin.ncl"),
);
pub const CONTRACT: (StdlibModule, &str, &str) = (
    StdlibModule::Contract,
    "<stdlib/contract.ncl>",
    include_str!("../stdlib/contract.ncl"),
);
pub const ARRAY: (StdlibModule, &str, &str) = (
    StdlibModule::Array,
    "<stdlib/array>",
    include_str!("../stdlib/array.ncl"),
);
pub const RECORD: (StdlibModule, &str, &str) = (
    StdlibModule::Record,
    "<stdlib/record>",
    include_str!("../stdlib/record.ncl"),
);
pub const STRING: (StdlibModule, &str, &str) = (
    StdlibModule::String,
    "<stdlib/string>",
    include_str!("../stdlib/string.ncl"),
);
pub const NUM: (StdlibModule, &str, &str) = (
    StdlibModule::Num,
    "<stdlib/num>",
    include_str!("../stdlib/num.ncl"),
);
pub const FUNCTION: (StdlibModule, &str, &str) = (
    StdlibModule::Function,
    "<stdlib/function>",
    include_str!("../stdlib/function.ncl"),
);
pub const INTERNALS: (StdlibModule, &str, &str) = (
    StdlibModule::Internals,
    "<stdlib/internals>",
    include_str!("../stdlib/internals.ncl"),
);

/// Return the list `(name, source_code)` of all the stdlib modules.
pub fn modules() -> Vec<(StdlibModule, &'static str, &'static str)> {
    // If you change the order of this, please also modify the
    // `get_module_id` function correspondinly
    vec![
        BUILTIN, CONTRACT, ARRAY, RECORD, STRING, NUM, FUNCTION, INTERNALS,
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
    Num,
    Function,
    Internals,
}

impl From<Ident> for StdlibModule {
    fn from(name: Ident) -> Self {
        match name.label() {
            "builtin" => StdlibModule::Builtin,
            "contract" => StdlibModule::Contract,
            "array" => StdlibModule::Array,
            "record" => StdlibModule::Record,
            "string" => StdlibModule::String,
            "num" => StdlibModule::Num,
            "function" => StdlibModule::Function,
            "internals" => StdlibModule::Internals,
            _ => StdlibModule::Array,
        }
    }
}

/// Get the index of the given module in the vector returned by [`modules`]
pub fn get_module_id(name: StdlibModule) -> usize {
    match name {
        StdlibModule::Builtin => 0,
        StdlibModule::Contract => 1,
        StdlibModule::Array => 2,
        StdlibModule::Record => 3,
        StdlibModule::String => 4,
        StdlibModule::Num => 5,
        StdlibModule::Function => 6,
        StdlibModule::Internals => 7,
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

    generate_accessor!(push_default);
    generate_accessor!(push_force);
}
