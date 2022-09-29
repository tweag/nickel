//! Load the Nickel standard library in strings at compile-time.

use crate::term::make as mk_term;
use crate::term::RichTerm;

pub const BUILTIN: (&str, &str) = (
    "<stdlib/builtin.ncl>",
    include_str!("../stdlib/builtin.ncl"),
);
pub const CONTRACT: (&str, &str) = (
    "<stdlib/contract.ncl>",
    include_str!("../stdlib/contract.ncl"),
);
pub const ARRAY: (&str, &str) = ("<stdlib/array>", include_str!("../stdlib/array.ncl"));
pub const RECORD: (&str, &str) = ("<stdlib/record>", include_str!("../stdlib/record.ncl"));
pub const STRING: (&str, &str) = ("<stdlib/string>", include_str!("../stdlib/string.ncl"));
pub const NUM: (&str, &str) = ("<stdlib/num>", include_str!("../stdlib/num.ncl"));
pub const FUNCTION: (&str, &str) = ("<stdlib/function>", include_str!("../stdlib/function.ncl"));
pub const COMPAT: (&str, &str) = ("<stdlib/compat>", include_str!("../stdlib/compat.ncl"));

/// Return the list `(name, source_code)` of all the stdlib modules.
pub fn modules() -> Vec<(&'static str, &'static str)> {
    vec![
        BUILTIN, CONTRACT, ARRAY, RECORD, STRING, NUM, FUNCTION, COMPAT,
    ]
}

/// Accessors to the builtin contracts.
pub mod contract {
    use super::*;

    macro_rules! generate_accessor {
        ($value:ident) => {
            pub fn $value() -> RichTerm {
                mk_term::var(format!("${}", stringify!($value)))
            }
        };
    }

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
    generate_accessor!(row_extend);
    generate_accessor!(record);
    generate_accessor!(dyn_record);
    generate_accessor!(record_extend);
    generate_accessor!(forall_tail);
    generate_accessor!(dyn_tail);
    generate_accessor!(empty_tail);
}

/// Contains functions helper for Nix evaluation by Nickel.
pub mod compat {
    use super::*;
    use crate::identifier::*;
    use crate::mk_app;
    use crate::term::make::op1;
    use crate::term::{Term, UnaryOp};

    /// Generate the `with` compatibility Nickel function which may be applied to an `Ident`
    /// you have to pass a list of with records in ordered from outer-most to inner-most one.
    pub fn with(array: Vec<RichTerm>) -> RichTerm {
        mk_app!(
            op1(
                UnaryOp::StaticAccess("with".into()),
                Term::Var("compat".into()),
            ),
            Term::Array(array, Default::default())
        )
    }
}
