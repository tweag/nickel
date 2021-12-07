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
}
