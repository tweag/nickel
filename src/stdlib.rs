//! Load the Nickel standard library in strings at compile-time.
pub const CONTRACTS: &str = include_str!("../stdlib/contracts.ncl");
pub const LISTS: &str = include_str!("../stdlib/lists.ncl");
