use codespan::FileId;
use codespan_reporting::diagnostic::Label;

use crate::{identifier::LocIdent, position::RawSpan};
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

/// Error indicating that a construct is not allowed when trying to interpret an `UniRecord` as a
/// record type in a strict way.
///
/// See [`UniRecord::into_type_strict`](crate::parser::uniterm::UniRecord::into_type_strict).
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum InvalidRecordTypeError {
    /// The record type had an invalid field, for example because it had a contract,
    /// an assigned value, or lacked a type annotation.
    InvalidField(RawSpan),
    /// The record had an ellipsis.
    IsOpen(RawSpan),
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

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ParseError {
    /// A specific lexical error
    Lexical(LexicalError),
    /// Unbound type variable(s)
    UnboundTypeVariables(Vec<LocIdent>),
    /// Illegal record type literal.
    ///
    /// This occurs when failing to convert from the uniterm syntax to a record type literal.
    /// See [RFC002](../../rfcs/002-merge-types-terms-syntax.md) for more details.
    InvalidRecordType {
        record_span: RawSpan,
        tail_span: Option<RawSpan>,
        cause: InvalidRecordTypeError,
    },
    /// A recursive let pattern was encountered. They are not currently supported because we
    /// decided it was too involved to implement them.
    RecursiveLetPattern(RawSpan),
    /// A duplicate binding was encountered in a record destructuring pattern.
    DuplicateIdentInRecordPattern {
        /// The duplicate identifier.
        ident: LocIdent,
        /// The previous instance of the duplicated identifier.
        prev_ident: LocIdent,
    },
    /// A type variable is used in ways that imply it has multiple different kinds.
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
        /// The position of the field definition (the identifier only).
        field_span: RawSpan,
        /// The position of the type annotation.
        annot_span: RawSpan,
    },
    /// The user provided a field path on the CLI, which is expected to be only composed of
    /// literals, but the parsed field path contains string interpolation.
    InterpolationInStaticPath { path_elem_span: RawSpan },
    /// There was an attempt to use a feature that hasn't been enabled.
    DisabledFeature { feature: String, span: RawSpan },
    /// A term was used as a contract in type position, but this term has no chance to make any
    /// sense as a contract. What terms make sense might evolve with time, but any given point in
    /// time, there are a set of expressions that can be excluded syntactically. Currently, it's
    /// mostly constants.
    InvalidContract(RawSpan),
}
