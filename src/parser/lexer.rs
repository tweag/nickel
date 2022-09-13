//! The lexer, transforming an input string to a stream of tokens.
//!
//! A modal lexer is implemented on top of two standard
//! [logos](https://github.com/maciejhirsz/logos) lexers in order to support arbitrary interpolated
//! expressions, which is not possible using LALRPOP's generated lexer. To see why, consider the
//! following string:
//!
//! ```text
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
use crate::parser::error::{LexicalError, ParseError};
use logos::Logos;
use std::ops::Range;

/// The tokens in normal mode.
#[derive(Logos, Debug, PartialEq, Clone)]
pub enum NormalToken<'input> {
    #[regex("[ \r\t\n]+", logos::skip)]
    #[error]
    Error,

    #[regex("_?[a-zA-Z][_a-zA-Z0-9-']*")]
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
    #[token("Array")]
    Array,

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
    #[token("rec")]
    Rec,
    #[token("switch")]
    Switch,

    #[token("null")]
    Null,
    #[token("true")]
    True,
    #[token("false")]
    False,

    #[token("?")]
    QuestionMark,
    #[token(",")]
    Comma,
    #[token(";")]
    Semicolon,
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
    #[token("\"")]
    DoubleQuote,

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
    #[token("..")]
    Ellipsis,

    #[token("fun")]
    Fun,
    #[token("import")]
    Import,
    #[token("|")]
    Pipe,
    #[token("|>")]
    RightPipe,
    #[token("->")]
    SimpleArrow,
    #[token("=>")]
    DoubleArrow,
    #[token("`")]
    Backtick,
    #[token("_")]
    Underscore,
    #[regex("m(%+)\"", |lex| lex.slice().len())]
    MultiStringStart(usize),

    #[token("%tag%")]
    Tag,
    #[token("%typeof%")]
    Typeof,

    #[token("%assume%")]
    Assume,
    #[token("%array_lazy_assume%")]
    ArrayLazyAssume,
    #[token("%blame%")]
    Blame,
    #[token("%chng_pol%")]
    ChangePol,
    #[token("%polarity%")]
    Polarity,
    #[token("%go_dom%")]
    GoDom,
    #[token("%go_codom%")]
    GoCodom,
    #[token("%go_field%")]
    GoField,
    #[token("%go_array%")]
    GoArray,

    #[token("%seal%")]
    Seal,
    #[token("%unseal%")]
    Unseal,
    #[token("%embed%")]
    Embed,
    #[token("%record_map%")]
    RecordMap,
    #[token("%record_insert%")]
    RecordInsert,
    #[token("%record_remove%")]
    RecordRemove,
    #[token("%seq%")]
    Seq,
    #[token("%deep_seq%")]
    DeepSeq,
    #[token("%force%")]
    OpForce,
    #[token("%head%")]
    Head,
    #[token("%tail%")]
    Tail,
    #[token("%length%")]
    Length,
    #[token("%fields%")]
    FieldsOf,
    #[token("%values%")]
    ValuesOf,
    #[token("%pow%")]
    Pow,

    #[token("%has_field%")]
    HasField,
    #[token("%map%")]
    Map,
    #[token("%elem_at%")]
    ElemAt,
    #[token("%generate%")]
    ArrayGen,
    #[token("default")]
    Default,
    #[token("doc")]
    Doc,
    #[token("optional")]
    Optional,
    #[token("priority")]
    Priority,
    #[token("force")]
    Force,

    #[token("%hash%")]
    OpHash,
    #[token("%serialize%")]
    Serialize,
    #[token("%deserialize%")]
    Deserialize,
    #[token("%str_split%")]
    StrSplit,
    #[token("%str_trim%")]
    StrTrim,
    #[token("%str_chars%")]
    StrChars,
    #[token("%char_code%")]
    CharCode,
    #[token("%char_from_code%")]
    CharFromCode,
    #[token("%str_uppercase%")]
    StrUppercase,
    #[token("%str_lowercase%")]
    StrLowercase,
    #[token("%str_contains%")]
    StrContains,
    #[token("%str_replace%")]
    StrReplace,
    #[token("%str_replace_regex%")]
    StrReplaceRegex,
    #[token("%str_is_match%")]
    StrIsMatch,
    #[token("%str_match%")]
    StrMatch,
    #[token("%str_length%")]
    StrLength,
    #[token("%str_substr%")]
    StrSubstr,
    #[token("%to_str%")]
    ToStr,
    #[token("%num_from_str%")]
    NumFromStr,
    #[token("%enum_from_str%")]
    EnumFromStr,

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
    #[token("[|")]
    EnumOpen,
    #[token("|]")]
    EnumClose,
    #[regex("#[^\n]*")]
    LineComment,
}

/// The tokens in string mode.
#[derive(Logos, Debug, PartialEq, Clone)]
pub enum StringToken<'input> {
    #[error]
    Error,

    #[regex("[^\"%\\\\]+")]
    // Has lower matching priority than `Interpolation` according to Logos' rules.
    #[token("%")]
    Literal(&'input str),

    #[token("\"")]
    DoubleQuote,
    #[token("%{")]
    Interpolation,
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

    #[regex("[^\"%]+")]
    // A token that starts as a multiline end delimiter or an interpolation sequence but is not
    // one.  These ones should have lowest matching priority according to Logos' rules, and
    // CandidateEnd and CandidateInterpolation should be matched first.
    #[token("\"")]
    #[regex("%+")]
    Literal(&'input str),

    /// A candidate end. A multiline string starting delimiter `MultiStringStart` can have a
    /// variable number of `%` character, so the lexer matches candidate end delimiter, compare the
    /// number of characters, and either emit the `End` token above, or turn the `CandidateEnd` to a
    /// `FalseEnd` otherwise
    #[regex("\"%+m")]
    CandidateEnd(&'input str),
    /// Same as `CandidateEnd`, but for interpolation
    #[regex("%+\\{")]
    CandidateInterpolation(&'input str),
    /// Unfortunate consequence of Logos' [issue #200](https://github.com/maciejhirsz/logos/issues/200).
    /// The other rules should be sufficient to match this as a double quote followed by a
    /// `CandidateInterpolation`, but if we omit this token, the lexer can fail unexpectedly on
    /// valid inputs because of #200.
    #[regex("\"%+\\{")]
    QuotesCandidateInterpolation(&'input str),
    /// Token emitted by the modal lexer for the parser once it has decided that a `CandidateEnd` is
    /// an actual end token.
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
    /// A token that has been buffered and must be returned at the next call to `next()`. This is
    /// made necessary by an issue of Logos (<https://github.com/maciejhirsz/logos/issues/200>). See
    /// [`MultiStringToken::QuotesCandidateInterpolation`].
    pub buffer: Option<(Token<'input>, Range<usize>)>,
}

impl<'input> Lexer<'input> {
    pub fn new(s: &'input str) -> Self {
        Lexer {
            lexer: Some(ModalLexer::Normal(NormalToken::lexer(s))),
            stack: Vec::new(),
            count: 0,
            buffer: None,
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
                self.lexer = Some(morph(lexer));
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
                self.lexer = Some(ModalLexer::Normal(lexer.morph()));
                self.stack.push(ModeElt::Str);
            }
            Some(ModalLexer::MultiStr(lexer)) => {
                self.lexer = Some(ModalLexer::Normal(lexer.morph()));
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

                self.lexer = Some(ModalLexer::Normal(lexer.morph()));
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

                self.lexer = Some(ModalLexer::Normal(lexer.morph()));
            }
            _ => panic!("lexer::leave_str"),
        }
    }

    fn leave_normal(&mut self) {
        match self.lexer.take() {
            Some(ModalLexer::Normal(lexer)) => {
                // count must be 0
                match self.stack.pop() {
                    Some(ModeElt::Str) => self.lexer = Some(ModalLexer::Str(lexer.morph())),
                    Some(ModeElt::MultiStr(count)) => {
                        self.count = count;
                        self.lexer = Some(ModalLexer::MultiStr(lexer.morph()))
                    }
                    mode => panic!("lexer::leave_normal (popped mode {:?})", mode),
                };
            }
            _ => panic!("lexer::leave_normal"),
        }
    }

    /// Split a candidate interpolation token into a string literal and an interpolation token. Put
    /// the interpolation token in the buffer to be popped later, and return the literal as the
    /// next token.
    ///
    /// # Precondition
    ///
    /// - this function requires `s.len() >= self.count`, or will panic.
    fn split_candidate_interp(
        &mut self,
        s: &'input str,
        span: Range<usize>,
    ) -> (Option<Token<'input>>, Range<usize>) {
        let split_at = s.len() - self.count + 1;
        let next_token = Token::MultiStr(MultiStringToken::Interpolation);
        let next_span = Range {
            start: span.start + split_at,
            end: span.end,
        };
        self.buffer = Some((next_token, next_span));

        let token = Some(Token::MultiStr(MultiStringToken::Literal(&s[0..split_at])));
        let span = Range {
            start: span.start,
            end: span.start + split_at,
        };

        (token, span)
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Result<(usize, Token<'input>, usize), ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        use Token::*;

        let (mut token, mut span) = if let Some((token, span)) = self.buffer.take() {
            (Some(token), span)
        } else {
            let lexer = self.lexer.as_mut().unwrap();
            let token = lexer.next();
            let span = lexer.span();
            (token, span)
        };

        match token.as_ref() {
            Some(Normal(NormalToken::DoubleQuote)) => self.enter_str(),
            Some(Normal(NormalToken::MultiStringStart(hash_count))) => {
                self.enter_indstr(*hash_count)
            }
            Some(Normal(NormalToken::LBrace)) => self.count += 1,
            Some(Normal(NormalToken::RBrace)) => {
                if self.count == 0 {
                    if self.stack.is_empty() {
                        return Some(Err(ParseError::Lexical(LexicalError::UnmatchedCloseBrace(
                            span.start,
                        ))));
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
            //
            // Note that the number of characters may be greater than `count`: in `m"%
            // %%%{foo} "%m`, the lexer will process `%%%{` as a candidate interpolation with 4
            // characters, while `count` is 1. In that case, we must emit an `%%` literal and put
            // an interpolation token in the buffer.
            Some(MultiStr(MultiStringToken::CandidateInterpolation(s)))
                if s.len() >= (self.count - 1) =>
            {
                if s.len() == (self.count - 1) {
                    token = Some(MultiStr(MultiStringToken::Interpolation));
                    self.enter_normal();
                } else {
                    let (token_fst, span_fst) = self.split_candidate_interp(s, span);
                    token = token_fst;
                    span = span_fst;
                }
            }
            // We never lex something as a `MultiStringToken::Interpolation` directly, but rather
            // generate it in this very function from other tokens. However, such a token could
            // have still been buffered in the previous iteration, and can thus be matched here,
            // which is why we need the case below.
            Some(MultiStr(MultiStringToken::Interpolation)) => self.enter_normal(),
            // If we encouter a `QuotesCandidateInterpolation` token with the right number of
            // characters, we need to split it into two tokens:
            // - a literal starting by a `"` followed by between 0 and k `%`
            // - an interpolation token
            // The interpolation token is put in the buffer such that it will be returned next
            // time.
            //
            // For example, in `m%%""%%%{exp}"%%m`, the `"%%%{` is a `QuotesCandidateInterpolation`
            // which is split as a `"%` literal followed by an interpolation token.
            Some(MultiStr(MultiStringToken::QuotesCandidateInterpolation(s)))
                if s.len() >= self.count =>
            {
                let (token_fst, span_fst) = self.split_candidate_interp(s, span);
                token = token_fst;
                span = span_fst;
            }
            // Otherwise, it is just part of the string, so we transform the token into a
            // `FalseInterpolation` one
            Some(MultiStr(MultiStringToken::CandidateInterpolation(s)))
            | Some(MultiStr(MultiStringToken::QuotesCandidateInterpolation(s))) => {
                token = Some(MultiStr(MultiStringToken::Literal(s)))
            }
            Some(Str(StringToken::Interpolation)) => self.enter_normal(),
            // Convert escape sequences to the corresponding character.
            Some(Str(StringToken::EscapedChar(c))) => {
                if let Some(esc) = escape_char(*c) {
                    token = Some(Str(StringToken::EscapedChar(esc)));
                } else {
                    return Some(Err(ParseError::Lexical(
                        LexicalError::InvalidEscapeSequence(span.start + 1),
                    )));
                }
            }
            Some(Str(StringToken::EscapedAscii(code))) => {
                if let Some(esc) = escape_ascii(code) {
                    token = Some(Str(StringToken::EscapedChar(esc)));
                } else {
                    return Some(Err(ParseError::Lexical(
                        LexicalError::InvalidAsciiEscapeCode(span.start + 2),
                    )));
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
                token = Some(MultiStr(MultiStringToken::Literal(s)))
            }
            // Early report errors for now. This could change in the future
            Some(Normal(NormalToken::Error))
            | Some(Str(StringToken::Error))
            | Some(MultiStr(MultiStringToken::Error)) => {
                return Some(Err(ParseError::Lexical(LexicalError::Generic(
                    span.start, span.end,
                ))))
            }
            // Ignore comment
            Some(Normal(NormalToken::LineComment)) => return self.next(),
            _ => (),
        };

        token.map(|t| Ok((span.start, t, span.end)))
    }
}

/// Generate the character corresponding to an escape char.
fn escape_char(chr: char) -> Option<char> {
    match chr {
        '\'' => Some('\''),
        '"' => Some('"'),
        '\\' => Some('\\'),
        '%' => Some('%'),
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
