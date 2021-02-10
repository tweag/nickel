//! The lexer, transforming an input string to a stream of tokens.
//!
//! A modal lexer is implemented on top of two standard
//! [logos](https://github.com/maciejhirsz/logos) lexers in order to support arbitrary interpolated
//! expressions, which is not possible using LALRPOP's generated lexer. To see why, consider the
//! following string:
//!
//! ```ignore
//! "hello, I have 1 + ${ {a = "40"}.a } + 1 bananas."
//! ```
//!
//! Once the `${` token is encountered, the lexer has to switch back to lexing expressions as
//! usual. But at the end of the interpolated expression, `+ 1 bananas.` needs to be parsed as a
//! string again, and not as normal program tokens. Since the interpolated expression is arbitrary,
//! it can contains nested `{` and `}` (as here, with records) and strings which themselves have
//! interpolated expression, and so on.
//!
//! This is typically not lexable using only regular expressions. To handle this, we use a *modal*
//! lexer. As hinted by the name, a modal lexer have several modes in which the same tokens can be
//! parsed differently. Ours can be in *normal* mode or in *string* mode.
//!
//! It also maintains a stack of brace counters, required inside an interpolated expression to
//! decide if a closing brace `}` belongs to the expression or is actually the closing brace of the
//! interpolated expression, indicating that we should switch back to string mode.
//!
//! When entering a string, the `Str` mode is entered. When a `${` is encountered in a string,
//! starting an interpolated expression, the normal mode is pushed. At each starting `{` in normal
//! mode, the brace counter is incremented. At each closing '}', it is decremented. When it reaches
//! `0`, this is the end of the current interpolated expressions, and we leave the normal mode and
//! go back to string mode. In our example, this is the second `}`: at this point, the lexer knows
//! that the coming characters must be lexed as string tokens, and not as normal tokens.
use logos::Logos;

/// The tokens in normal mode.
#[derive(Logos, Debug, PartialEq, Clone)]
pub enum NormalToken<'input> {
    #[regex("[ \r\t\n]+", logos::skip)]
    #[error]
    Error,

    #[regex("_?[a-zA-Z][_a-zA-Z0-9]*")]
    Identifier(&'input str),
    #[regex("[0-9]*\\.?[0-9]+", |lex| lex.slice().parse())]
    NumLiteral(f64),

    #[token("Dyn")]
    Dyn,
    #[token("Num")]
    Num,
    #[token("Bool")]
    Bool,
    #[token("Str")]
    Str,
    #[token("List")]
    List,

    #[token("if")]
    If,
    #[token("then")]
    Then,
    #[token("else")]
    Else,
    #[token("forall")]
    Forall,
    #[token("in")]
    In,
    #[token("let")]
    Let,
    #[token("switch")]
    Switch,

    #[token("null")]
    Null,
    #[token("true")]
    True,
    #[token("false")]
    False,

    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token("$")]
    Dollar,
    #[token("=")]
    Equals,
    #[token("!=")]
    NotEquals,
    #[token("&")]
    Ampersand,
    #[token(".")]
    Dot,
    #[token("$[")]
    DollarBracket,
    #[token("\"")]
    DoubleQuote,
    #[token("-$")]
    MinusDollar,

    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Times,
    #[token("/")]
    Div,
    #[token("%")]
    Percent,
    #[token("++")]
    DoublePlus,
    #[token("==")]
    DoubleEq,
    #[token("@")]
    At,
    #[token("&&")]
    DoubleAnd,
    #[token("||")]
    DoublePipe,
    #[token("!")]
    Bang,

    #[token("fun")]
    Fun,
    #[token("import")]
    Import,
    #[token("|")]
    Pipe,
    #[token("->")]
    SimpleArrow,
    #[token("=>")]
    DoubleArrow,
    #[token("#")]
    Hash,
    #[token("`")]
    Backtick,
    #[token("_")]
    Underscore,
    #[regex("m(#+)\"", |lex| lex.slice().len())]
    MultiStringStart(usize),

    #[token("%tag%")]
    Tag,
    #[token("%isNum%")]
    IsNum,
    #[token("%isBool%")]
    IsBool,
    #[token("%isStr%")]
    IsStr,
    #[token("%isFun%")]
    IsFun,
    #[token("%isList%")]
    IsList,
    #[token("%isRecord%")]
    IsRecord,

    #[token("%blame%")]
    Blame,
    #[token("%chngPol%")]
    ChangePol,
    #[token("%polarity%")]
    Polarity,
    #[token("%goDom%")]
    GoDom,
    #[token("%goCodom%")]
    GoCodom,
    #[token("%goField%")]
    GoField,
    #[token("%goList%")]
    GoList,

    #[token("%wrap%")]
    Wrap,
    #[token("%unwrap%")]
    Unwrap,
    #[token("%embed%")]
    Embed,
    #[token("%recordMap%")]
    RecordMap,
    #[token("%seq%")]
    Seq,
    #[token("%deepSeq%")]
    DeepSeq,
    #[token("%head%")]
    Head,
    #[token("%tail%")]
    Tail,
    #[token("%length%")]
    Length,
    #[token("%fieldsOf%")]
    FieldsOf,
    #[token("%valuesOf%")]
    ValuesOf,
    #[token("%pow%")]
    Pow,

    #[token("%hasField%")]
    HasField,
    #[token("%map%")]
    Map,
    #[token("%elemAt%")]
    ElemAt,
    #[token("%generate%")]
    ListGen,
    #[token("merge")]
    Merge,
    #[token("default")]
    Default,
    #[token("doc")]
    Doc,

    #[token("%hash%")]
    OpHash,
    #[token("%serialize%")]
    Serialize,
    #[token("%deserialize%")]
    Deserialize,
    #[token("%strSplit%")]
    StrSplit,
    #[token("%strTrim%")]
    StrTrim,
    #[token("%strChars%")]
    StrChars,
    #[token("%charCode%")]
    CharCode,
    #[token("%charFromCode%")]
    CharFromCode,
    #[token("%strUppercase%")]
    StrUppercase,
    #[token("%strLowercase%")]
    StrLowercase,
    #[token("%strContains%")]
    StrContains,
    #[token("%strReplace%")]
    StrReplace,
    #[token("%strMatch")]
    StrMatch,
    #[token("%strLength")]
    StrLength,
    #[token("%strSubstr%")]
    StrSubstr,
    #[token("%strFrom%")]
    StrFrom,
    #[token("%numFrom%")]
    NumFrom,
    #[token("%enumFrom%")]
    EnumFrom,

    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("<")]
    LAngleBracket,
    #[token("<=")]
    LessOrEq,
    #[token(">")]
    RAngleBracket,
    #[token(">=")]
    GreaterOrEq,
    #[regex("//[^\n]*")]
    LineComment,
}

/// The tokens in string mode.
#[derive(Logos, Debug, PartialEq, Clone)]
pub enum StringToken<'input> {
    #[error]
    Error,

    #[regex("[^\"#\\\\]+")]
    Literal(&'input str),

    #[token("\"")]
    DoubleQuote,
    // Has lower matching priority than `HashBrace` according to Logos' rules.
    #[token("#")]
    Hash(&'input str),
    #[token("#{")]
    HashBrace,
    #[regex("\\\\.", |lex| lex.slice().chars().nth(1))]
    EscapedChar(char),
    // Repetition range `{2}` was not supported at the time of writing this regex.
    #[regex("\\\\x[A-Fa-f0-9][A-Fa-f0-9]", |lex| &lex.slice()[2..4])]
    EscapedAscii(&'input str),
}

/// The tokens in multiline string mode.
#[derive(Logos, Debug, PartialEq, Clone)]
pub enum MultiStringToken<'input> {
    #[error]
    Error,

    #[regex("[^\"#]+")]
    Literal(&'input str),

    // A token that starts as a multiline end delimiter, but is not one. To avoid hacking
    // look-aheads in the lexer (which Logos doesn't support for performance reason), we just use a
    // separate token. This one has lowest matching priority according to Logos' rules, so it is
    // matched only if `CandidateEnd` cannot be
    #[regex("\"#*")]
    FalseEnd(&'input str),
    // A candidate end. A multiline string starting delimiter `MultiStringStart` can have a
    // variable number of `#` character, so the lexer matchs candidate end delimiter, compare the
    // number of characters, and either emit the `End` token above, or turn the `CandidateEnd` to a
    // `FalseEnd` otherwise
    #[regex("\"#+m")]
    CandidateEnd(&'input str),
    // Same as `FalseEnd` and `CandidateEnd` but for an interpolation sequence.
    #[regex("#+")]
    FalseInterpolation(&'input str),
    #[regex("#+\\{")]
    CandidateInterpolation(&'input str),
    // Token emitted by the modal lexer for the parser once it has decided that a `CandidateEnd` is
    // an actual end token.
    End,
    Interpolation,
}

/// The tokens of the modal lexer.
#[derive(Debug, PartialEq, Clone)]
pub enum Token<'input> {
    Normal(NormalToken<'input>),
    Str(StringToken<'input>),
    MultiStr(MultiStringToken<'input>),
}

type NormalLexer<'input> = logos::Lexer<'input, NormalToken<'input>>;
type StrLexer<'input> = logos::Lexer<'input, StringToken<'input>>;
type MultiStrLexer<'input> = logos::Lexer<'input, MultiStringToken<'input>>;

pub enum ModalLexer<'input> {
    Normal(NormalLexer<'input>),
    Str(StrLexer<'input>),
    MultiStr(MultiStrLexer<'input>),
}

// Wrap the `next()` function of the underlying lexer.
impl<'input> Iterator for ModalLexer<'input> {
    type Item = Token<'input>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            ModalLexer::Normal(lexer) => lexer.next().map(Token::Normal),
            ModalLexer::Str(lexer) => lexer.next().map(Token::Str),
            ModalLexer::MultiStr(lexer) => lexer.next().map(Token::MultiStr),
        }
    }
}

// Wrap the `span()` function of the underlying lexer.
impl<'input> ModalLexer<'input> {
    pub fn span(&self) -> std::ops::Range<usize> {
        match self {
            ModalLexer::Normal(lexer) => lexer.span(),
            ModalLexer::Str(lexer) => lexer.span(),
            ModalLexer::MultiStr(lexer) => lexer.span(),
        }
    }
}

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

#[derive(Clone, PartialEq, Eq, Debug, Copy)]
pub enum ModeElt {
    Str,
    MultiStr(usize),
    Normal(usize),
}

pub struct Lexer<'input> {
    // We are forced to use an `Option` in order to be able to switch mode without cloning the
    // underlying lexer. Logos offers a `morph()` function for a in-place conversion between
    // lexers, that we want to use to transform a normal mode lexer to a string mode lexer. But
    // Rust's borrowing system won't let us take ownership of the underlying lexer without
    // replacing it first by something else, whence the `Option`. `lexer` should never be none
    // excepted in an non observable intermediate state during mode switching.
    /// The modal lexer.
    pub lexer: Option<ModalLexer<'input>>,
    /// A counter:
    ///  - in normal mode, the current brace counter to determine if a closing brace is the end of
    ///  an interpolated expression.
    ///  - in multiline string mode, this is the number of characters of the starting delimiter.
    ///  This is required to correctly detect the end of such multi-line strings.
    ///  - in string mode, it is unused, and is always `0`
    pub count: usize,
    /// The stack of brace counters.
    ///
    /// As interpolated strings can be nested, we can start to lex a new string while we were
    /// already inside an interpolated expression. In this case, once this string ends, we must
    /// restore the original brace counter, which is what this stack is used for.
    pub stack: Vec<ModeElt>,
}

impl<'input> Lexer<'input> {
    pub fn new(s: &'input str) -> Self {
        Lexer {
            lexer: Some(ModalLexer::Normal(NormalToken::lexer(s))),
            stack: Vec::new(),
            count: 0,
        }
    }

    fn enter_strlike<F>(&mut self, morph: F)
    where
        F: FnOnce(NormalLexer<'input>) -> ModalLexer<'input>,
    {
        match self.lexer.take() {
            // Cannot transition from a string mode to another one, so the current mode must be
            //  `Normal`
            Some(ModalLexer::Normal(lexer)) => {
                self.stack.push(ModeElt::Normal(self.count));
                self.lexer.replace(morph(lexer));
            }
            _ => panic!("lexer::enter_strlike"),
        }
    }

    fn enter_str(&mut self) {
        self.enter_strlike(|lexer| ModalLexer::Str(lexer.morph()));
        self.count = 0;
    }

    fn enter_indstr(&mut self, hash_count: usize) {
        self.enter_strlike(|lexer| ModalLexer::MultiStr(lexer.morph()));
        self.count = hash_count;
    }

    fn enter_normal(&mut self) {
        match self.lexer.take() {
            //count must be zero, and we do not push it on the stack
            Some(ModalLexer::Str(lexer)) => {
                self.lexer.replace(ModalLexer::Normal(lexer.morph()));
                self.stack.push(ModeElt::Str);
            }
            Some(ModalLexer::MultiStr(lexer)) => {
                self.lexer.replace(ModalLexer::Normal(lexer.morph()));
                self.stack.push(ModeElt::MultiStr(self.count));
            }
            _ => panic!("lexer::enter_normal"),
        }

        self.count = 0;
    }

    fn leave_str(&mut self) {
        match self.lexer.take() {
            Some(ModalLexer::Str(lexer)) => {
                // We can only enter string mode from normal mode
                self.count = match self.stack.pop() {
                    Some(ModeElt::Normal(count)) => count,
                    mode => panic!("lexer::leave_str (popped mode {:?})", mode),
                };

                self.lexer.replace(ModalLexer::Normal(lexer.morph()));
            }
            _ => panic!("lexer::leave_str"),
        }
    }

    fn leave_indstr(&mut self) {
        match self.lexer.take() {
            Some(ModalLexer::MultiStr(lexer)) => {
                // We can only enter string mode from normal mode
                self.count = match self.stack.pop() {
                    Some(ModeElt::Normal(count)) => count,
                    mode => panic!("lexer::leave_str (popped mode {:?})", mode),
                };

                self.lexer.replace(ModalLexer::Normal(lexer.morph()));
            }
            _ => panic!("lexer::leave_str"),
        }
    }

    fn leave_normal(&mut self) {
        match self.lexer.take() {
            Some(ModalLexer::Normal(lexer)) => {
                // count must be 0
                match self.stack.pop() {
                    Some(ModeElt::Str) => self.lexer.replace(ModalLexer::Str(lexer.morph())),
                    Some(ModeElt::MultiStr(count)) => {
                        self.count = count;
                        self.lexer.replace(ModalLexer::MultiStr(lexer.morph()))
                    }
                    mode => panic!("lexer::leave_normal (popped mode {:?})", mode),
                };
            }
            _ => panic!("lexer::leave_normal"),
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Result<(usize, Token<'input>, usize), LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        use Token::*;

        let lexer = self.lexer.as_mut().unwrap();
        let mut token = lexer.next();
        let span = lexer.span();

        match token.as_ref() {
            Some(Normal(NormalToken::DoubleQuote)) => self.enter_str(),
            Some(Normal(NormalToken::MultiStringStart(hash_count))) => {
                self.enter_indstr(*hash_count)
            }
            Some(Normal(NormalToken::LBrace)) => self.count += 1,
            Some(Normal(NormalToken::RBrace)) => {
                if self.count == 0 {
                    if self.stack.is_empty() {
                        return Some(Err(LexicalError::UnmatchedCloseBrace(span.start)));
                    }

                    self.leave_normal();
                } else {
                    self.count -= 1;
                }
            }
            Some(Str(StringToken::DoubleQuote)) => {
                self.leave_str();
                // To make things simpler on the parser side, we only return one variant for
                // `DoubleQuote`, namely the the normal one.
                token = Some(Normal(NormalToken::DoubleQuote));
            }
            // If we encounter a `CandidateInterp` token with the right number of characters, this is
            // an interpolation sequence.
            Some(MultiStr(MultiStringToken::CandidateInterpolation(s)))
                if s.len() == (self.count - 1) =>
            {
                token = Some(MultiStr(MultiStringToken::Interpolation));
                self.enter_normal();
            }
            // Otherwise, it is just part of the string, so we transform the token into a
            // `FalseInterpolation` one
            Some(MultiStr(MultiStringToken::CandidateInterpolation(s))) => {
                token = Some(MultiStr(MultiStringToken::FalseInterpolation(s)))
            }
            Some(Str(StringToken::HashBrace)) => self.enter_normal(),
            // Convert escape sequences to the corresponding character.
            Some(Str(StringToken::EscapedChar(c))) => {
                if let Some(esc) = escape_char(*c) {
                    token = Some(Str(StringToken::EscapedChar(esc)));
                } else {
                    return Some(Err(LexicalError::InvalidEscapeSequence(span.start + 1)));
                }
            }
            Some(Str(StringToken::EscapedAscii(code))) => {
                if let Some(esc) = escape_ascii(code) {
                    token = Some(Str(StringToken::EscapedChar(esc)));
                } else {
                    return Some(Err(LexicalError::InvalidAsciiEscapeCode(span.start + 2)));
                }
            }
            // If we encounter a `CandidateEnd` token with the right number of characters, this is
            // the end of a multiline string
            Some(MultiStr(MultiStringToken::CandidateEnd(s))) if s.len() == self.count => {
                token = Some(MultiStr(MultiStringToken::End));
                self.leave_indstr()
            }
            // Otherwise, it is just part of the string, so we transform the token into a
            // `FalseEnd` one
            Some(MultiStr(MultiStringToken::CandidateEnd(s))) => {
                token = Some(MultiStr(MultiStringToken::FalseEnd(s)))
            }
            // Early report errors for now. This could change in the future
            Some(Normal(NormalToken::Error))
            | Some(Str(StringToken::Error))
            | Some(MultiStr(MultiStringToken::Error)) => {
                return Some(Err(LexicalError::Generic(span.start, span.end)))
            }
            // Ignore comment
            Some(Normal(NormalToken::LineComment)) => return self.next(),
            _ => (),
        }

        token.map(|t| Ok((span.start, t, span.end)))
    }
}

/// Generate the character corresponding to an escape char.
fn escape_char(chr: char) -> Option<char> {
    match chr {
        '\'' => Some('\''),
        '"' => Some('"'),
        '\\' => Some('\\'),
        '#' => Some('#'),
        'n' => Some('\n'),
        'r' => Some('\r'),
        't' => Some('\t'),
        _ => None,
    }
}

/// Generate the character corresponding to an ASCII escape sequence.
///
/// # Arguments
/// - `code`: a string representation of the ASCII code in hexadecimal
fn escape_ascii(code: &str) -> Option<char> {
    let code = u8::from_str_radix(code, 16).ok()?;
    if code > 0x7F {
        None
    } else {
        Some(code as char)
    }
}
