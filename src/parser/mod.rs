use crate::identifier::Ident;
use crate::term::RichTerm;
use lalrpop_util::lalrpop_mod;

lalrpop_mod!(
    #[allow(clippy::all)]
    #[allow(unused_parens)]
    pub grammar);

pub mod error;
pub mod lexer;
#[cfg(test)]
mod tests;
pub mod utils;

/// Either a term or a toplevel let declaration.
pub enum ExtendedTerm {
    RichTerm(RichTerm),
    ToplevelLet(Ident, RichTerm),
}
