use crate::error::{ParseError, ParseErrors};
use crate::identifier::Ident;
use crate::term::RichTerm;
use codespan::FileId;
use lalrpop_util::lalrpop_mod;

lalrpop_mod!(
    #[allow(clippy::all)]
    #[allow(unused_parens)]
    pub grammar);

pub mod error;
pub mod lexer;
pub mod uniterm;
pub mod utils;

#[cfg(test)]
mod tests;

/// Either a term or a toplevel let declaration.
/// Used exclusively in the REPL to allow the defining of variables without having to specify `in`.
/// For instance:
/// ```text
/// nickel>let foo = 1
/// nickel>foo
/// 1
/// ```
pub enum ExtendedTerm {
    RichTerm(RichTerm),
    ToplevelLet(Ident, RichTerm),
}

impl grammar::ExtendedTermParser {
    pub fn parse_term_tolerant(
        &self,
        file_id: FileId,
        lexer: lexer::Lexer,
    ) -> Result<(ExtendedTerm, ParseErrors), ParseError> {
        let mut parse_errors = Vec::new();
        let result = self
            .parse(file_id, &mut parse_errors, lexer)
            .map_err(|err| ParseError::from_lalrpop(err, file_id));

        let parse_errors = ParseErrors::from_recoverable(parse_errors, file_id);
        match result {
            Ok(t) => Ok((t, parse_errors)),
            Err(e) => Err(e),
        }
    }

    pub fn parse_term(
        &self,
        file_id: FileId,
        lexer: lexer::Lexer,
    ) -> Result<ExtendedTerm, ParseErrors> {
        match self.parse_term_tolerant(file_id, lexer) {
            Ok((t, e)) if e.no_errors() => Ok(t),
            Ok((_, e)) => Err(e),
            Err(e) => Err(e.into()),
        }
    }
}

impl grammar::TermParser {
    /// Parse a term from a token stream in an error tolerant way: parse errors are accumulated and
    /// returned, instead of returning an error. The `Err` variant corresponds to a non-recoverable
    /// error.
    pub fn parse_term_lax(
        &self,
        file_id: FileId,
        lexer: lexer::Lexer,
    ) -> Result<(RichTerm, ParseErrors), ParseError> {
        let mut parse_errors = Vec::new();
        let result = self
            .parse(file_id, &mut parse_errors, lexer)
            .map_err(|err| ParseError::from_lalrpop(err, file_id));

        let parse_errors = ParseErrors::from_recoverable(parse_errors, file_id);
        match result {
            Ok(t) => Ok((t, parse_errors)),
            Err(e) => Err(e),
        }
    }

    pub fn parse_term(
        &self,
        file_id: FileId,
        lexer: lexer::Lexer,
    ) -> Result<RichTerm, ParseErrors> {
        match self.parse_term_lax(file_id, lexer) {
            Ok((t, e)) if e.no_errors() => Ok(t),
            Ok((_, e)) => Err(e),
            Err(e) => Err(e.into()),
        }
    }
}
