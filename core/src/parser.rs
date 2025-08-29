use crate::bytecode::ast::compat::{FromAst, ToMainline};

use nickel_lang_parser::{
    ast::{Ast, AstAlloc},
    error::{ParseError, ParseErrors},
    files::FileId,
    identifier::LocIdent,
    lexer, metrics,
    position::RawSpan,
    CliFieldAssignmentParser, ErrorTolerantParser, ExtendedTerm, ExtendedTermParser,
    FixedTypeParser, StaticFieldPathParser, TermParser,
};

/// General interface of the various specialized Nickel parsers.
///
/// This trait is a compatibility layer version of [ErrorTolerantParser]. It produces data of the
/// old, mainline types because the current pipeline still depends on them (defined in
/// [crate::term]). Eventually we'll get rid of it and only use [ErrorTolerantParser], which
/// produces the new AST instead.
pub trait ErrorTolerantParserCompat<T> {
    /// Parse a value from a lexer with the given `file_id` in an error-tolerant way. This methods
    /// can still fail for non-recoverable errors.
    fn parse_tolerant_compat(
        &self,
        file_id: FileId,
        lexer: lexer::Lexer,
    ) -> Result<(T, ParseErrors), ParseError>;

    /// Parse a value from a lexer with the given `file_id`, failing at the first encountered
    /// error.
    fn parse_strict_compat(&self, file_id: FileId, lexer: lexer::Lexer) -> Result<T, ParseErrors>;
}

impl<'ast> FromAst<ExtendedTerm<Ast<'ast>>> for ExtendedTerm<crate::term::RichTerm> {
    fn from_ast(ast: &ExtendedTerm<Ast<'ast>>) -> Self {
        match ast {
            ExtendedTerm::Term(t) => ExtendedTerm::Term(t.to_mainline()),
            ExtendedTerm::ToplevelLet(ident, t) => {
                ExtendedTerm::ToplevelLet(*ident, t.to_mainline())
            }
        }
    }
}

// Generate boilerplate impl to produce legacy mainline types from the available parsers.
macro_rules! generate_compat_impl {
    ($parser:ty, $output:ty) => {
        impl ErrorTolerantParserCompat<$output> for $parser {
            fn parse_tolerant_compat(
                &self,
                file_id: FileId,
                lexer: lexer::Lexer,
            ) -> Result<($output, ParseErrors), ParseError> {
                let alloc = AstAlloc::new();
                self.parse_tolerant(&alloc, file_id, lexer).map(|(t, e)| {
                    (
                        metrics::measure_runtime!("runtime:ast_conversion", t.to_mainline()),
                        e,
                    )
                })
            }

            fn parse_strict_compat(
                &self,
                file_id: FileId,
                lexer: lexer::Lexer,
            ) -> Result<$output, ParseErrors> {
                let alloc = AstAlloc::new();
                self.parse_strict(&alloc, file_id, lexer)
                    .map(|t| metrics::measure_runtime!("runtime:ast_conversion", t.to_mainline()))
            }
        }
    };
}

generate_compat_impl!(ExtendedTermParser, ExtendedTerm<crate::term::RichTerm>);
generate_compat_impl!(TermParser, crate::term::RichTerm);
generate_compat_impl!(FixedTypeParser, crate::typ::Type);

impl ErrorTolerantParserCompat<(Vec<LocIdent>, crate::term::RichTerm, RawSpan)>
    for CliFieldAssignmentParser
{
    fn parse_tolerant_compat(
        &self,
        file_id: FileId,
        lexer: lexer::Lexer,
    ) -> Result<((Vec<LocIdent>, crate::term::RichTerm, RawSpan), ParseErrors), ParseError> {
        self.parse_tolerant(&AstAlloc::new(), file_id, lexer)
            .map(|((path, term, span), e)| ((path, term.to_mainline(), span), e))
    }

    fn parse_strict_compat(
        &self,
        file_id: FileId,
        lexer: lexer::Lexer,
    ) -> Result<(Vec<LocIdent>, crate::term::RichTerm, RawSpan), ParseErrors> {
        self.parse_strict(&AstAlloc::new(), file_id, lexer)
            .map(|(path, term, span)| (path, term.to_mainline(), span))
    }
}

// This implementation doesn't do any conversion, but hide away the (useless, in this case)
// [crate::bytecode::ast::AstAlloc] parameter.
impl ErrorTolerantParserCompat<Vec<LocIdent>> for StaticFieldPathParser {
    fn parse_tolerant_compat(
        &self,
        file_id: FileId,
        lexer: lexer::Lexer,
    ) -> Result<(Vec<LocIdent>, ParseErrors), ParseError> {
        self.parse_tolerant(&AstAlloc::new(), file_id, lexer)
    }

    fn parse_strict_compat(
        &self,
        file_id: FileId,
        lexer: lexer::Lexer,
    ) -> Result<Vec<LocIdent>, ParseErrors> {
        self.parse_strict(&AstAlloc::new(), file_id, lexer)
    }
}
