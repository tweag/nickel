use codespan::ByteIndex;
use codespan_reporting::{diagnostic::Label, files::Files as _};
use lalrpop_util::ErrorRecovery;

use crate::{
    files::{FileId, Files},
    identifier::{Ident, LocIdent},
    lexer::Token,
    position::RawSpan,
    utils::mk_span,
};
use std::ops::Range;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum LexicalError {
    /// A closing brace '}' does not match an opening brace '{'.
    UnmatchedCloseBrace(usize),
    /// Invalid escape sequence in a string literal.
    InvalidEscapeSequence(usize),
    /// Invalid escape ASCII code in a string literal.
    InvalidAsciiEscapeCode(usize),
    /// A multiline string was closed with a delimiter which has a `%` count higher than the
    /// opening delimiter.
    StringDelimiterMismatch {
        opening_delimiter: Range<usize>,
        closing_delimiter: Range<usize>,
    },
    /// Generic lexer error
    Generic(Range<usize>),
}

/// Error indicating that a construct is not allowed when trying to interpret a `UniRecord` as a
/// record type in a strict way.
///
/// See `parser::uniterm::UniRecord::into_type_strict`.
#[derive(Debug, Copy, Clone, PartialEq, Eq, rkyv::Archive)]
pub enum InvalidRecordTypeError {
    /// The record type had an invalid field, for example because it had a contract,
    /// an assigned value, or lacked a type annotation.
    InvalidField(RawSpan),
    /// The record had an ellipsis.
    IsOpen(RawSpan),
    /// The record has `include` statements.
    HasInclude(RawSpan),
    /// The record type had a field whose name used string interpolation.
    InterpolatedField(RawSpan),
    /// A field name was repeated.
    RepeatedField { orig: RawSpan, dup: RawSpan },
}

impl InvalidRecordTypeError {
    pub fn labels(&self) -> Vec<Label<FileId>> {
        let label = |span: &RawSpan| {
            Label::secondary(span.src_id, span.start.to_usize()..span.end.to_usize())
        };
        match self {
            InvalidRecordTypeError::InvalidField(pos) => {
                vec![label(pos).with_message("invalid field for a record type literal")]
            }
            InvalidRecordTypeError::IsOpen(pos) => {
                vec![label(pos).with_message("cannot have ellipsis in a record type literal")]
            }
            InvalidRecordTypeError::HasInclude(pos) => {
                vec![label(pos).with_message("cannot have `include` statements in a record type")]
            }
            InvalidRecordTypeError::InterpolatedField(pos) => {
                vec![label(pos).with_message("this field uses interpolation")]
            }
            InvalidRecordTypeError::RepeatedField { orig, dup } => {
                vec![
                    label(orig).with_message("first occurrence"),
                    label(dup).with_message("second occurrence"),
                ]
            }
        }
    }

    pub fn notes(&self) -> Option<String> {
        match self {
            InvalidRecordTypeError::InvalidField(_) => Some(
                "Value assignments such as `<field> = <expr>`, and metadata \
                    annotation (annotation, documentation, etc.) are forbidden."
                    .into(),
            ),
            InvalidRecordTypeError::InterpolatedField(_) => {
                Some("String interpolation in field names is forbidden in record types".into())
            }
            InvalidRecordTypeError::RepeatedField { .. } => {
                Some("Repeated field names are forbidden".into())
            }
            _ => None,
        }
    }
}

/// An error occurring during parsing.
#[derive(Debug, PartialEq, Eq, Clone, rkyv::Archive)]
pub enum ParseError {
    /// Unexpected end of file.
    UnexpectedEOF(FileId, /* tokens expected by the parser */ Vec<String>),
    /// Unexpected token.
    UnexpectedToken(
        RawSpan,
        /* tokens expected by the parser */ Vec<String>,
    ),
    /// Superfluous, unexpected token.
    ExtraToken(RawSpan),
    /// A closing brace '}' does not match an opening brace '{'. This rather precise error is
    /// detected because of how interpolated strings are lexed.
    UnmatchedCloseBrace(RawSpan),
    /// Invalid escape sequence in a string literal.
    InvalidEscapeSequence(RawSpan),
    /// Invalid ASCII escape code in a string literal.
    InvalidAsciiEscapeCode(RawSpan),
    /// A multiline string was closed with a delimiter which has a `%` count higher than the
    /// opening delimiter.
    StringDelimiterMismatch {
        opening_delimiter: RawSpan,
        closing_delimiter: RawSpan,
    },
    /// Error when parsing an external format such as JSON, YAML, etc.
    ExternalFormatError(
        String, /* format */
        String, /* error message */
        Option<RawSpan>,
    ),
    /// Unbound type variable
    UnboundTypeVariables(Vec<LocIdent>),
    /// Illegal record type literal.
    ///
    /// This occurs when failing to convert from the uniterm syntax to a record type literal.
    /// See [RFC002](../../rfcs/002-merge-types-terms-syntax.md) for more details.
    InvalidRecordType {
        /// The position of the invalid record.
        record_span: RawSpan,
        /// Position of the tail, if there was one.
        tail_span: Option<RawSpan>,
        /// The reason that interpretation as a record type failed.
        cause: InvalidRecordTypeError,
    },
    /// A recursive let pattern was encountered. They are not currently supported because we
    /// decided it was too involved to implement them.
    RecursiveLetPattern(RawSpan),
    /// Let blocks can currently only contain plain bindings, not pattern bindings.
    PatternInLetBlock(RawSpan),
    /// A type variable is used in ways that imply it has muiltiple different kinds.
    ///
    /// This can happen in several situations, for example:
    /// - a variable is used as both a type variable and a row type variable,
    ///   e.g. in the signature `forall r. { ; r } -> r`,
    /// - a variable is used as both a record and enum row variable, e.g. in the
    ///   signature `forall r. [| ; r |] -> { ; r }`.
    TypeVariableKindMismatch { ty_var: LocIdent, span: RawSpan },
    /// A record literal, which isn't a record type, has a field with a type annotation but without
    /// a definition. While we could technically handle this situation, this is most probably an
    /// error from the user, because this type annotation is useless and, maybe non-intuitively,
    /// won't have any effect as part of a larger contract:
    ///
    /// ```nickel
    /// let MixedContract = {foo : String, bar | Number} in
    /// { foo = 1, bar = 2} | MixedContract
    /// ```
    ///
    /// This example works, because the `foo : String` annotation doesn't propagate, and contract
    /// application is mostly merging, which is probably not the intent. It might become a warning
    /// in a future version, but we don't have warnings for now, so we rather forbid such
    /// constructions.
    TypedFieldWithoutDefinition {
        /// The position of the field definition.
        field_span: RawSpan,
        /// The position of the type annotation.
        annot_span: RawSpan,
    },
    /// The user provided a field path on the CLI, which is expected to be only composed of
    /// literals, but the parsed field path contains string interpolation.
    InterpolationInStaticPath {
        input: String,
        path_elem_span: RawSpan,
    },
    /// A duplicate binding was encountered in a record destructuring pattern.
    DuplicateIdentInRecordPattern {
        /// The duplicate identifier.
        ident: LocIdent,
        /// The previous instance of the duplicated identifier.
        prev_ident: LocIdent,
    },
    /// A duplicate binding was encountered in a let block.
    DuplicateIdentInLetBlock {
        /// The duplicate identifier.
        ident: LocIdent,
        /// The previous instance of the duplicated identifier.
        prev_ident: LocIdent,
    },
    /// There was an attempt to use a feature that hasn't been enabled.
    DisabledFeature { feature: String, span: RawSpan },
    /// A term was used as a contract in type position, but this term has no chance to make any
    /// sense as a contract. What terms make sense might evolve with time, but any given point in
    /// time, there are a set of expressions that can be excluded syntactically. Currently, it's
    /// mostly constants.
    InvalidContract(RawSpan),
    /// Unrecognized explicit import format tag
    InvalidImportFormat { span: RawSpan },
    /// A CLI sigil expression such as `@env:FOO` is invalid because no `:` separator was found.
    SigilExprMissingColon(RawSpan),
    /// A CLI sigil expression is unknown or unsupported, such as `@unknown:value`.
    UnknownSigilSelector { selector: String, span: RawSpan },
    /// A CLI sigil attribute is unknown or unsupported, such as `@file/unsupported:value`.
    UnknownSigilAttribute {
        selector: String,
        attribute: String,
        span: RawSpan,
    },
    /// An included field has several definitions. While we could just merge both at runtime like a
    /// piecewise field definition, we entirely forbid this situation for now.
    MultipleFieldDecls {
        /// The identifier.
        ident: Ident,
        /// The identifier and the position of the include expression. The ident part is the same
        /// as the ident part of `ident`.
        include_span: RawSpan,
        /// The span of the other declaration, which can be either a field
        /// definition or an include expression as well.
        other_span: RawSpan,
    },
}

impl ParseError {
    pub(crate) fn from_lalrpop<T>(
        error: lalrpop_util::ParseError<usize, T, ParseOrLexError>,
        file_id: FileId,
    ) -> ParseError {
        match error {
            lalrpop_util::ParseError::InvalidToken { location } => {
                ParseError::UnexpectedToken(mk_span(file_id, location, location + 1), Vec::new())
            }
            lalrpop_util::ParseError::UnrecognizedToken {
                token: (start, _, end),
                expected,
            } => ParseError::UnexpectedToken(mk_span(file_id, start, end), expected),
            lalrpop_util::ParseError::UnrecognizedEof { expected, .. } => {
                ParseError::UnexpectedEOF(file_id, expected)
            }
            lalrpop_util::ParseError::ExtraToken {
                token: (start, _, end),
            } => ParseError::ExtraToken(mk_span(file_id, start, end)),
            lalrpop_util::ParseError::User {
                error: ParseOrLexError::Lexical(e),
            } => Self::from_lexical(e, file_id),
            lalrpop_util::ParseError::User {
                error: ParseOrLexError::Parse(e),
            } => e,
        }
    }

    fn from_lexical(error: LexicalError, file_id: FileId) -> ParseError {
        match error {
            LexicalError::Generic(range) => {
                ParseError::UnexpectedToken(mk_span(file_id, range.start, range.end), Vec::new())
            }
            LexicalError::UnmatchedCloseBrace(location) => {
                ParseError::UnmatchedCloseBrace(mk_span(file_id, location, location + 1))
            }
            LexicalError::InvalidEscapeSequence(location) => {
                ParseError::InvalidEscapeSequence(mk_span(file_id, location, location + 1))
            }
            LexicalError::InvalidAsciiEscapeCode(location) => {
                ParseError::InvalidAsciiEscapeCode(mk_span(file_id, location, location + 2))
            }
            LexicalError::StringDelimiterMismatch {
                opening_delimiter,
                closing_delimiter,
            } => ParseError::StringDelimiterMismatch {
                opening_delimiter: mk_span(file_id, opening_delimiter.start, opening_delimiter.end),
                closing_delimiter: mk_span(file_id, closing_delimiter.start, closing_delimiter.end),
            },
        }
    }

    pub fn from_serde_json(error: serde_json::Error, location: Option<(FileId, &Files)>) -> Self {
        use codespan::ByteOffset;

        // error.line() should start at `1` according to the documentation, but in practice, it may
        // be 0 for the error `json parse error: data did not match any variant of untagged enum
        // Term`. Although this error should not happen, if it does, it's better to get a message
        // than a panic message `subtract with overflow`.
        let line_span = if error.line() == 0 {
            None
        } else {
            location.and_then(|(file_id, files)| files.line_index(file_id, error.line() - 1).ok())
        };

        let start =
            line_span.map(|ls| ByteIndex::from(((ls + error.column()) as u32).saturating_sub(1)));
        ParseError::ExternalFormatError(
            String::from("json"),
            error.to_string(),
            start.map(|start| RawSpan {
                // unwrap: if start is Some, location was Some.
                src_id: location.unwrap().0,
                start,
                end: start + ByteOffset::from(1),
            }),
        )
    }

    pub fn from_yaml(error: saphyr_parser::ScanError, file_id: Option<FileId>) -> Self {
        use codespan::{ByteIndex, ByteOffset};

        let start = ByteIndex::from(error.marker().index() as u32);
        ParseError::ExternalFormatError(
            String::from("yaml"),
            error.to_string(),
            file_id.map(|src_id| RawSpan {
                src_id,
                start,
                end: start + ByteOffset::from(1),
            }),
        )
    }

    pub fn from_toml(error: toml_edit::TomlError, file_id: FileId) -> Self {
        use codespan::{ByteIndex, ByteOffset};

        let span = error.span();
        ParseError::ExternalFormatError(
            String::from("toml"),
            error.to_string(),
            span.map(|span| RawSpan {
                src_id: file_id,
                start: ByteIndex::from(span.start as u32),
                end: ByteIndex(span.end as u32) + ByteOffset::from(1),
            }),
        )
    }

    #[cfg(feature = "nix-experimental")]
    pub fn from_nix(error: &str, _file_id: FileId) -> Self {
        // Span is shown in the nix error message
        ParseError::ExternalFormatError(String::from("nix"), error.to_string(), None)
    }
}

// TODO: make this non-pub. It currently leaks out in some lalrpop-generated code and I haven't
// figured out how to make it not.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ParseOrLexError {
    Lexical(LexicalError),
    Parse(ParseError),
}

impl From<ParseError> for ParseOrLexError {
    fn from(e: ParseError) -> Self {
        Self::Parse(e)
    }
}

impl From<LexicalError> for ParseOrLexError {
    fn from(e: LexicalError) -> Self {
        Self::Lexical(e)
    }
}

impl<T> From<ParseError> for lalrpop_util::ParseError<usize, T, ParseOrLexError> {
    fn from(e: ParseError) -> Self {
        lalrpop_util::ParseError::User {
            error: ParseOrLexError::Parse(e),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub struct ParseErrors {
    pub errors: Vec<ParseError>,
}

impl ParseErrors {
    pub fn new(errors: Vec<ParseError>) -> ParseErrors {
        ParseErrors { errors }
    }

    pub fn errors(self) -> Option<Vec<ParseError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(self.errors)
        }
    }

    pub fn no_errors(&self) -> bool {
        self.errors.is_empty()
    }

    pub const fn none() -> ParseErrors {
        ParseErrors { errors: Vec::new() }
    }

    pub(crate) fn from_recoverable(
        errs: Vec<ErrorRecovery<usize, Token<'_>, ParseOrLexError>>,
        file_id: FileId,
    ) -> Self {
        ParseErrors {
            errors: errs
                .into_iter()
                .map(|e| ParseError::from_lalrpop(e.error, file_id))
                .collect(),
        }
    }
}

impl From<ParseError> for ParseErrors {
    fn from(e: ParseError) -> ParseErrors {
        ParseErrors { errors: vec![e] }
    }
}

impl From<Vec<ParseError>> for ParseErrors {
    fn from(errors: Vec<ParseError>) -> ParseErrors {
        ParseErrors { errors }
    }
}
