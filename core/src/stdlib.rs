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
    ($value:ident / immediate) => {
        pub fn $value() -> RichTerm {
            mk_term::var(format!("${}/immediate", stringify!($value)))
        }
    };
    ($value:ident / delayed) => {
        pub fn $value() -> RichTerm {
            mk_term::var(format!("${}/delayed", stringify!($value)))
        }
    };
    ($value:ident) => {
        pub fn $value() -> RichTerm {
            mk_term::var(format!("${}", stringify!($value)))
        }
    };
}

/// Accessors to the builtin contracts and other internals that aren't accessible from user code.
pub mod internals {
    use super::*;

    //TODO
    // Same, `enum` is a reserved keyword in Rust.
    pub fn enumeration() -> RichTerm {
        mk_term::var("$enum")
    }

    pub mod immediate {
        use super::*;

        // `dyn` is a reserved keyword in Rust, so the macro `generate_accessor` doesn't work.
        pub fn dynamic() -> RichTerm {
            mk_term::var("$dyn/immediate")
        }

        // `enum` is a reserved keyword in Rust, so the macro `generate_accessor` doesn't work.
        pub fn enumeration() -> RichTerm {
            mk_term::var("$enum/immediate")
        }

        generate_accessor!(number / immediate);
        generate_accessor!(bool / immediate);
        generate_accessor!(foreign_id / immediate);
        generate_accessor!(string / immediate);
        generate_accessor!(fail / immediate);

        generate_accessor!(forall / immediate);

        generate_accessor!(array / immediate);
        generate_accessor!(array_dyn / immediate);

        generate_accessor!(func / immediate);
        generate_accessor!(enum_variant / immediate);

        generate_accessor!(record / immediate);
        generate_accessor!(record_type / immediate);

        generate_accessor!(dict_dyn / immediate);
    }

    pub mod delayed {
        use super::*;

        // `enum` is a reserved keyword in Rust, so the macro `generate_accessor` doesn't work.
        pub fn enumeration() -> RichTerm {
            mk_term::var("$enum/delayed")
        }

        generate_accessor!(array / delayed);

        generate_accessor!(forall / delayed);

        generate_accessor!(func / delayed);
        generate_accessor!(func_dom / delayed);
        generate_accessor!(func_codom / delayed);

        generate_accessor!(record / delayed);
        generate_accessor!(record_type / delayed);
        generate_accessor!(dict_contract / delayed);
        generate_accessor!(dict_type / delayed);
    }

    generate_accessor!(forall_var);

    generate_accessor!(enum_fail);
    generate_accessor!(forall_enum_tail);

    generate_accessor!(forall_record_tail);
    generate_accessor!(dyn_tail);
    generate_accessor!(empty_tail);

    generate_accessor!(stdlib_contract_equal);

    generate_accessor!(prepare_contract);

    generate_accessor!(rec_default);
    generate_accessor!(rec_force);
}
