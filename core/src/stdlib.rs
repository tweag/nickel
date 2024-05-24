//! Load the Nickel standard library in strings at compile-time.
use crate::term::make as mk_term;
use crate::term::RichTerm;

/// This is an array containing all the Nickel standard library modules. Currently, this is one
/// monolithic `std` module, and the definitions of `internals` living at the toplevel.
///
/// Using a dedicated enum tpe, handling arrays, etc. for two modules can seem a bit overkill, but
/// we'll probably extend `StdlibModule` when we'll split the `std` module into several files.
pub fn modules() -> [StdlibModule; 2] {
    [StdlibModule::Std, StdlibModule::Internals]
}

/// Represents a particular Nickel standard library module.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum StdlibModule {
    Std,
    Internals,
}

impl StdlibModule {
    pub fn file_name(&self) -> &'static str {
        match self {
            StdlibModule::Std => "<stdlib/std.ncl>",
            StdlibModule::Internals => "<stdlib/internals.ncl>",
        }
    }

    /// The name of the module. Used to determine its namespace in the initial environment (the
    /// module named `std` will be put under the `std` identifier). `StdlibModule::Internals` is an
    /// exception, because although it has a name, it's not put under any namespace but directly at
    /// top-level in the environment.
    pub fn name(&self) -> &'static str {
        match self {
            StdlibModule::Std => "std",
            StdlibModule::Internals => "internals",
        }
    }

    pub fn content(&self) -> &'static str {
        match self {
            StdlibModule::Std => include_str!("../stdlib/std.ncl"),
            StdlibModule::Internals => include_str!("../stdlib/internals.ncl"),
        }
    }
}

pub struct UnknownStdlibModule;

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

    // `enum` is a reserved keyword in rust
    pub fn enumeration() -> RichTerm {
        mk_term::var("$enum")
    }

    generate_accessor!(num);
    generate_accessor!(bool);
    generate_accessor!(foreign_id);
    generate_accessor!(string);
    generate_accessor!(fail);

    generate_accessor!(array);
    generate_accessor!(array_dyn);

    generate_accessor!(func);
    generate_accessor!(func_dom);
    generate_accessor!(func_codom);
    generate_accessor!(func_dyn);

    generate_accessor!(forall_var);
    generate_accessor!(forall);

    generate_accessor!(enum_fail);
    generate_accessor!(enum_variant);
    generate_accessor!(forall_enum_tail);

    generate_accessor!(record);
    generate_accessor!(record_extend);
    generate_accessor!(forall_record_tail);
    generate_accessor!(dyn_tail);
    generate_accessor!(empty_tail);

    generate_accessor!(dict_type);
    generate_accessor!(dict_contract);
    generate_accessor!(dict_dyn);

    generate_accessor!(stdlib_contract_equal);

    generate_accessor!(rec_default);
    generate_accessor!(rec_force);
}
