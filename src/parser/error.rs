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
}
