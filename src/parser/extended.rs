//! Extended parser that accepts toplevel declarations as enabled in the REPL (see [repl](../repl/index.html)).
use super::{grammar, lexer};
use crate::error::ParseError;
use crate::identifier::Ident;
use crate::term::RichTerm;
use codespan::FileId;

/// Either a term or a toplevel let declaration.
pub enum ExtendedTerm {
    RichTerm(RichTerm),
    ToplevelLet(Ident, RichTerm),
}

impl From<RichTerm> for ExtendedTerm {
    fn from(rt: RichTerm) -> Self {
        ExtendedTerm::RichTerm(rt)
    }
}

/// Wrapper around generated parsers for standard terms and toplevel declarations.
pub struct ExtendedParser {
    term_parser: grammar::TermParser,
    let_parser: grammar::ToplevelLetParser,
}

impl ExtendedParser {
    pub fn new() -> Self {
        ExtendedParser {
            term_parser: grammar::TermParser::new(),
            let_parser: grammar::ToplevelLetParser::new(),
        }
    }

    /// Try to parse an input which may either be a standard term or a toplevel let declaration.
    ///
    /// First try the standard term parser. Only try the toplevel let parser if the first fails
    /// with unexpected EOF. If both parser fail, return the original unexpected EOF error of the
    /// first try.
    pub fn parse<'input>(
        &self,
        file_id: FileId,
        input: &'input str,
    ) -> Result<ExtendedTerm, ParseError> {
        match self
            .term_parser
            .parse(file_id, lexer::Lexer::new(input))
            .map_err(|err| ParseError::from_lalrpop(err, file_id))
        {
            Ok(rt) => Ok(rt.into()),
            // Only try the toplevel let parser on unterminated inputs
            Err(fst_err @ ParseError::UnexpectedEOF(..)) => {
                match self.let_parser.parse(file_id, lexer::Lexer::new(input)) {
                    Ok((id, rt)) => Ok(ExtendedTerm::ToplevelLet(id, rt)),
                    // Reporting the initial error.
                    Err(_) => Err(fst_err),
                }
            }
            Err(err) => Err(err),
        }
    }
}
