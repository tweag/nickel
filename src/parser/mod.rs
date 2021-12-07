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
#[cfg(test)]
mod tests;
pub mod utils;

/// Either a term or a toplevel let declaration.
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
    pub fn parse_term_tolerant(
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
        match self.parse_term_tolerant(file_id, lexer) {
            Ok((t, e)) if e.no_errors() => Ok(t),
            Ok((_, e)) => Err(e),
            Err(e) => Err(e.into()),
        }
    }
}
