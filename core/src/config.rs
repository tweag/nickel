//! Configuration for the nickel interpreter.

use crate::error::report::ColorOpt;

#[derive(Default)]
pub struct Config {
    /// The color option to use when reporting errors.
    pub color_opt: ColorOpt,
    /// If true, the type-checker will start in strict mode.
    pub strict_typechecking: bool,
}
