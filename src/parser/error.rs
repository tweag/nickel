use crate::{identifier::Ident, position::RawSpan};

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum LexicalError {
    /// A closing brace '}' does not match an opening brace '{'.
    UnmatchedCloseBrace(usize),
    /// Invalid escape sequence in a string literal.
    InvalidEscapeSequence(usize),
    /// Invalid escape ASCII code in a string literal.
    InvalidAsciiEscapeCode(usize),
    /// Generic lexer error
    Generic(usize, usize),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ParseError {
    /// A specific lexical error
    Lexical(LexicalError),
    /// Unbound type variable(s)
    UnboundTypeVariables(Vec<Ident>, RawSpan),
    /// Illegal record literal in the uniterm syntax. In practice, this is a record with a
    /// polymorphic tail that contains a construct that wasn't permitted inside a record type in
    /// the original syntax. Typically, a field assignment:
    ///
    /// ```nickel
    /// forall a. {foo : Num; a} # allowed
    /// forall a. {foo : Num = 1; a} # InvalidUniRecord error: giving a value to foo is forbidden
    /// ```
    ///
    /// See [RFC002](../../rfcs/002-merge-types-terms-syntax.md) for more details.
    InvalidUniRecord(
        RawSpan, /* illegal (in conjunction with a tail) construct position */
        RawSpan, /* tail position */
        RawSpan, /* whole record position */
    ),
    /// A recursive let pattern was encountered. They are not currently supported because we
    /// decided it was too involved to implement them.
    RecursiveLetPattern(RawSpan),
    /// A type variable is used in ways that imply it has muiltiple different kinds.
    ///
    /// This can happen in several situations, for example:
    /// - a variable is used as both a type variable and a row type variable,
    ///   e.g. in the signature `forall r. { ; r } -> r`,
    /// - a variable is used as both a record and enum row variable, e.g. in the
    ///   signature `forall r. [| ; r |] -> { ; r }`.
    TypeVariableKindMismatch { ty_var: Ident, span: RawSpan },
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
}
