use crate::error::{ParseError, ParseErrors};
use crate::identifier::Ident;
use crate::term::RichTerm;
use crate::types::Types;
use codespan::FileId;
use lalrpop_util::lalrpop_mod;

lalrpop_mod!(
    #[allow(clippy::all)]
    #[allow(unused_parens)]
    #[allow(unused_imports)]
    pub grammar, "/parser/grammar.rs");

use grammar::__ToTriple;

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

/// The interface of LALRPOP-generated parsers, for each public rule. This trait is used to
/// implement parser-independent features (such as error tolerance helpers), which don't have to be
/// reimplemented for each and every parser.
///
/// The type of `parse` was just copy-pasted from the generated code of LALRPOP.
pub trait LalrpopParser<T> {
    fn parse<'input, 'err, 'wcard, __TOKEN, __TOKENS>(
        &self,
        src_id: FileId,
        errors: &'err mut Vec<
            lalrpop_util::ErrorRecovery<usize, lexer::Token<'input>, self::error::ParseError>,
        >,
        next_wildcard_id: &'wcard mut usize,
        __tokens0: __TOKENS,
    ) -> Result<T, lalrpop_util::ParseError<usize, lexer::Token<'input>, self::error::ParseError>>
    where
        __TOKEN: __ToTriple<'input, 'err, 'wcard>,
        __TOKENS: IntoIterator<Item = __TOKEN>;
}

/// Generate boiler-plate code to implement the trait [`LalrpopParser`] for a parser generated by
/// LALRPOP.
macro_rules! generate_lalrpop_parser_impl {
    ($parser:ty, $output:ty) => {
        impl LalrpopParser<$output> for $parser {
            fn parse<'input, 'err, 'wcard, __TOKEN, __TOKENS>(
                &self,
                src_id: FileId,
                errors: &'err mut Vec<
                    lalrpop_util::ErrorRecovery<
                        usize,
                        lexer::Token<'input>,
                        self::error::ParseError,
                    >,
                >,
                next_wildcard_id: &'wcard mut usize,
                __tokens0: __TOKENS,
            ) -> Result<
                $output,
                lalrpop_util::ParseError<usize, lexer::Token<'input>, self::error::ParseError>,
            >
            where
                __TOKEN: __ToTriple<'input, 'err, 'wcard>,
                __TOKENS: IntoIterator<Item = __TOKEN>,
            {
                Self::parse(self, src_id, errors, next_wildcard_id, __tokens0)
            }
        }
    };
}

generate_lalrpop_parser_impl!(grammar::ExtendedTermParser, ExtendedTerm);
generate_lalrpop_parser_impl!(grammar::TermParser, RichTerm);
generate_lalrpop_parser_impl!(grammar::FixedTypeParser, Types);

pub trait ErrorTolerantParser<T>: LalrpopParser<T> {
    fn parse_tolerant(
        &self,
        file_id: FileId,
        lexer: lexer::Lexer,
    ) -> Result<(T, ParseErrors), ParseError> {
        let mut parse_errors = Vec::new();
        let mut next_wildcard_id = 0;
        let result = self
            .parse(file_id, &mut parse_errors, &mut next_wildcard_id, lexer)
            .map_err(|err| ParseError::from_lalrpop(err, file_id));

        let parse_errors = ParseErrors::from_recoverable(parse_errors, file_id);
        match result {
            Ok(t) => Ok((t, parse_errors)),
            Err(e) => Err(e),
        }
    }

    fn parse_strict(&self, file_id: FileId, lexer: lexer::Lexer) -> Result<T, ParseErrors> {
        match self.parse_tolerant(file_id, lexer) {
            Ok((t, e)) if e.no_errors() => Ok(t),
            Ok((_, e)) => Err(e),
            Err(e) => Err(e.into()),
        }
    }
}

impl<T, P> ErrorTolerantParser<T> for P where P: LalrpopParser<T> {}
