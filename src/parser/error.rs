use crate::{identifier::Ident, position::RawSpan};

#[derive(Clone, PartialEq, Debug)]
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

#[derive(Clone, PartialEq, Debug)]
pub enum ParseError {
    /// A specific lexical error
    Lexical(LexicalError),
    /// Unbound type variable(s)
    UnboundTypeVariables(Vec<Ident>, RawSpan),
    /// Illegal record literal in the uniterm syntax (see RFC002). In practice, this is a record
    /// mixing a tail and non-type constructs.
    InvalidUniRecord(
        RawSpan, /* non-type construct position */
        RawSpan, /* tail position */
        RawSpan, /* whole record position */
    ),
}
