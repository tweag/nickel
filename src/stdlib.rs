//! Load the Nickel standard library in strings at compile-time.

use crate::term::make as mk_term;
use crate::term::RichTerm;

pub const BUILTINS: &str = include_str!("../stdlib/builtins.ncl");
pub const CONTRACTS: &str = include_str!("../stdlib/contracts.ncl");
pub const LISTS: &str = include_str!("../stdlib/lists.ncl");
pub const RECORDS: &str = include_str!("../stdlib/records.ncl");

/// Accessors to the builtin contracts.
pub mod contracts {
    use super::*;

    macro_rules! generate_accessor {
        ($value:ident) => {
            pub fn $value() -> RichTerm {
                mk_term::var(stringify!($value))
            }
        };
    }

    // `dyn` is a reserved keyword in rust
    pub fn dynamic() -> RichTerm {
        mk_term::var("dyn")
    }

    generate_accessor!(num);
    generate_accessor!(bool);
    generate_accessor!(string);
    generate_accessor!(list);
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
