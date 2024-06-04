//! The lexer, transforming an input string to a stream of tokens.
//!
//! A modal lexer is implemented on top of two standard
//! [logos](https://github.com/maciejhirsz/logos) lexers in order to support arbitrary interpolated
//! expressions, which is not possible using LALRPOP's generated lexer. To see why, consider the
//! following string:
//!
//! ```text
//! "hello, I have 1 + %{ {a = "40"}.a } + 1 bananas."
//! ```
//!
//! Once the `%{` token is encountered, the lexer has to switch back to lexing expressions as
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
//! When entering a string, the `Str` mode is entered. When a `%{` is encountered in a string,
//! starting an interpolated expression, the normal mode is pushed. At each starting `{` in normal
//! mode, the brace counter is incremented. At each closing '}', it is decremented. When it reaches
//! `0`, this is the end of the current interpolated expressions, and we leave the normal mode and
//! go back to string mode. In our example, this is the second `}`: at this point, the lexer knows
//! that the coming characters must be lexed as string tokens, and not as normal tokens.
use super::{
    error::{LexicalError, ParseError},
    utils::{parse_number_base, parse_number_sci},
};
use crate::term::Number;
use logos::Logos;
use std::ops::Range;

fn symbolic_string_prefix_and_length<'input>(
    lex: &mut logos::Lexer<'input, NormalToken<'input>>,
) -> SymbolicStringStart<'input> {
    let slice = lex.slice();
    let (prefix, postfix) = slice
        .rsplit_once('-')
        .expect("The logos regexp ensures this succeeds");
    SymbolicStringStart {
        prefix,
        length: postfix.len(),
    }
}

// **IMPORTANT**
// When adding or removing tokens that might be parsed as identifiers,
// please update the [KEYWORDS] array
/// The tokens in normal mode.
#[derive(Logos, Debug, PartialEq, Clone)]
pub enum NormalToken<'input> {
    #[regex("((\r\n)+|[ \t\n]+)", logos::skip)]
    // multiline strings cannot be used as enum tags, so we explicitly
    // disallow that pattern.
    #[regex("'m(%)+\"")]
    // We forbid lone carriage returns for sanity
    #[regex("\r[^\n]")]
    #[error]
    Error,

    // **IMPORTANT**
    // This regex should be kept in sync with the one for RawEnumTag below.
    // Also, any change in the lexer regex must also be backported in the LSP's
    // regex for checking identifiers at ../lsp/nls/src/requests/completion.rs
    #[regex("_*[a-zA-Z][_a-zA-Z0-9-']*")]
    Identifier(&'input str),
    #[regex("[0-9]*\\.?[0-9]+([eE][+\\-]?[0-9]+)?", |lex| parse_number_sci(lex.slice()))]
    DecNumLiteral(Number),
    #[regex("0x[A-Fa-f0-9]+", |lex| parse_number_base(16, &lex.slice()[2..]))]
    HexNumLiteral(Number),
    #[regex("0o[0-7]+", |lex| parse_number_base(8, &lex.slice()[2..]))]
    OctNumLiteral(Number),
    #[regex("0b[01]+", |lex| parse_number_base(2, &lex.slice()[2..]))]
    BinNumLiteral(Number),

    // **IMPORTANT**
    // This regex should be kept in sync with the one for Identifier above.
    #[regex("'_*[a-zA-Z][_a-zA-Z0-9-']*", |lex| lex.slice().split_at(1).1)]
    RawEnumTag(&'input str),
    #[token("'\"")]
    StrEnumTagBegin,

    #[token("Dyn")]
    Dyn,
    #[token("Number")]
    Number,
    #[token("Bool")]
    Bool,
    #[token("String")]
    String,
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
    #[token("match")]
    Match,

    #[token("null")]
    Null,
    #[token("true")]
    True,
    #[token("false")]
    False,
    /// Or isn't a reserved keyword. It is a contextual keyword (a keyword that can be used as an
    /// identifier because it's not ambiguous) within patterns.
    #[token("or")]
    Or,

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
    #[token("_")]
    Underscore,
    #[regex("m(%+)\"", |lex| lex.slice().len())]
    MultiStringStart(usize),
    #[regex("[a-zA-Z][_a-zA-Z0-9-']*-s(%+)\"", symbolic_string_prefix_and_length)]
    SymbolicStringStart(SymbolicStringStart<'input>),

    #[token("%typeof%")]
    Typeof,

    #[token("%contract/apply%")]
    ContractApply,
    #[token("%contract/array_lazy_apply%")]
    ContractArrayLazyApp,
    #[token("%contract/record_lazy_apply%")]
    ContractRecordLazyApp,
    #[token("%blame%")]
    Blame,
    #[token("%label/flip_polarity%")]
    LabelFlipPol,
    #[token("%label/polarity%")]
    LabelPol,
    #[token("%label/go_dom%")]
    LabelGoDom,
    #[token("%label/go_codom%")]
    LabelGoCodom,
    #[token("%label/go_field%")]
    LabelGoField,
    #[token("%label/go_array%")]
    LabelGoArray,
    #[token("%label/go_dict%")]
    LabelGoDict,
    #[token("%label/insert_type_variable%")]
    LabelInsertTypeVar,
    #[token("%label/lookup_type_variable%")]
    LabelLookupTypeVar,

    #[token("%seal%")]
    Seal,
    #[token("%unseal%")]
    Unseal,
    #[token("%enum/embed%")]
    EnumEmbed,
    #[token("%record/map%")]
    RecordMap,
    #[token("%record/insert%")]
    RecordInsert,
    #[token("%record/insert_with_opts%")]
    RecordInsertWithOpts,
    #[token("%record/remove%")]
    RecordRemove,
    #[token("%record/remove_with_opts%")]
    RecordRemoveWithOpts,
    #[token("%record/empty_with_tail%")]
    RecordEmptyWithTail,
    #[token("%record/seal_tail%")]
    RecordSealTail,
    #[token("%record/unseal_tail%")]
    RecordUnsealTail,
    #[token("%seq%")]
    Seq,
    #[token("%deep_seq%")]
    DeepSeq,
    #[token("%force%")]
    OpForce,
    #[token("%array/length%")]
    ArrayLength,
    #[token("%record/fields%")]
    RecordFields,
    #[token("%record/fields_with_opts%")]
    RecordFieldsWithOpts,

    #[token("%record/values%")]
    RecordValues,
    #[token("%pow%")]
    Pow,
    #[token("%trace%")]
    Trace,

    #[token("%record/has_field%")]
    RecordHasField,
    #[token("%record/has_field_with_opts%")]
    RecordHasFieldWithOpts,
    #[token("%array/map%")]
    ArrayMap,
    #[token("%array/at%")]
    ArrayAt,
    #[token("%array/generate%")]
    ArrayGen,
    #[token("%rec_force%")]
    OpRecForce,
    #[token("%rec_default%")]
    OpRecDefault,
    #[token("%record/field_is_defined%")]
    RecordFieldIsDefined,
    #[token("%record/field_is_defined_with_opts%")]
    RecordFieldIsDefinedWithOpts,

    #[token("merge")]
    Merge,
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
    #[token("not_exported")]
    NotExported,

    #[token("%hash%")]
    OpHash,
    #[token("%serialize%")]
    Serialize,
    #[token("%deserialize%")]
    Deserialize,
    #[token("%string/split%")]
    StringSplit,
    #[token("%string/trim%")]
    StringTrim,
    #[token("%string/chars%")]
    StringChars,
    #[token("%string/uppercase%")]
    StringUppercase,
    #[token("%string/lowercase%")]
    StringLowercase,
    #[token("%string/contains%")]
    StringContains,
    #[token("%string/replace%")]
    StringReplace,
    #[token("%string/replace_regex%")]
    StringReplaceRegex,
    #[token("%string/is_match%")]
    StringIsMatch,
    #[token("%string/find%")]
    StringFind,
    #[token("%string/find_all%")]
    StringFindAll,
    #[token("%string/length%")]
    StringLength,
    #[token("%string/substr%")]
    StringSubstr,
    #[token("%to_string%")]
    ToString,
    #[token("%number/from_string%")]
    NumberFromString,
    #[token("%enum/from_string%")]
    EnumFromString,
    #[token("%enum/get_arg%")]
    EnumGetArg,
    #[token("%enum/make_variant%")]
    EnumMakeVariant,
    #[token("%enum/is_variant%")]
    EnumIsVariant,
    #[token("%enum/get_tag%")]
    EnumGetTag,

    #[token("%label/with_message%")]
    LabelWithMessage,
    #[token("%label/with_notes%")]
    LabelWithNotes,
    #[token("%label/append_note%")]
    LabelAppendNote,
    #[token("%label/push_diag%")]
    LabelPushDiag,
    #[token("%array/slice%")]
    ArraySlice,
    #[token("%eval_nix%")]
    EvalNix,

    #[token("%pattern_branch%")]
    PatternBranch,

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

pub const KEYWORDS: &[&str] = &[
    "Dyn",
    "Number",
    "Bool",
    "String",
    "Array",
    "if",
    "then",
    "else",
    "forall",
    "in",
    "let",
    "rec",
    "match",
    "null",
    "true",
    "false",
    "fun",
    "import",
    "merge",
    "default",
    "doc",
    "optional",
    "priority",
    "force",
    "not_exported",
];

#[derive(Debug, Clone, PartialEq)]
pub struct SymbolicStringStart<'input> {
    /// The prefix for the symbolic string, e.g. `nix-s%""%` has prefix `"nix"`
    pub prefix: &'input str,
    /// The length of the string delimiter, excluding `prefix` and `-`. E.g. `nix-s%%""%` has
    /// `length` 4, the length of `s%%"`
    pub length: usize,
}

/// The tokens in string mode.
#[derive(Logos, Debug, PartialEq, Eq, Clone)]
pub enum StringToken<'input> {
    // We forbid lone carriage returns for sanity
    #[regex("\r[^\n]")]
    #[error]
    Error,

    #[regex("[^\"%\\\\]+", |lex| normalize_line_endings(lex.slice()))]
    // Has lower matching priority than `Interpolation` according to Logos' rules.
    #[token("%", |lex| String::from(lex.slice()))]
    Literal(String),

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
#[derive(Logos, Debug, PartialEq, Eq, Clone)]
pub enum MultiStringToken<'input> {
    // We forbid lone carriage returns for sanity
    #[regex("\r[^\n]")]
    #[error]
    Error,

    #[regex("[^\"%]+", |lex| normalize_line_endings(lex.slice()))]
    // A token that starts as a multiline end delimiter or an interpolation sequence but is not
    // one.  These ones should have lowest matching priority according to Logos' rules, and
    // CandidateEnd and CandidateInterpolation should be matched first.
    #[token("\"", |lex| String::from(lex.slice()))]
    #[regex("%+", |lex| String::from(lex.slice()))]
    Literal(String),

    /// A candidate end. A multiline string starting delimiter `MultiStringStart` can have a
    /// variable number of `%` character, so the lexer matches candidate end delimiter, compare the
    /// number of characters, and either emit the `End` token above, or turn the `CandidateEnd` to a
    /// `FalseEnd` otherwise
    #[regex("\"%+")]
    CandidateEnd(&'input str),

    /// Same as `CandidateEnd`, but for interpolation
    #[regex("%+\\{")]
    CandidateInterpolation(&'input str),

    /// Unfortunate consequence of Logos'
    /// [issue#200](https://github.com/maciejhirsz/logos/issues/200). The other rules should be
    /// sufficient to match this as a double quote followed by a `CandidateInterpolation`, but if we
    /// omit this token, the lexer can fail unexpectedly on valid inputs because of #200.
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

pub type SpannedToken<'input> = (usize, Token<'input>, usize);
type NormalLexer<'input> = logos::Lexer<'input, NormalToken<'input>>;
type StringLexer<'input> = logos::Lexer<'input, StringToken<'input>>;
type MultiStringLexer<'input> = logos::Lexer<'input, MultiStringToken<'input>>;

pub enum ModalLexer<'input> {
    Normal {
        mode_data: NormalData,
        logos_lexer: NormalLexer<'input>,
    },
    String {
        logos_lexer: StringLexer<'input>,
    },
    MultiString {
        mode_data: MultiStrData,
        /// A token that has been buffered and must be returned at the next call to `next()`.
        /// Related to lexing a possible interpolation sequence, such as `%%%{`, which requires to
        /// split a candidate interpolation token in two. In this case, we need to emit the first
        /// token on the spot, and bufferize the second one, to be emitted on the following call to
        /// `next()`.
        buffer: Option<(MultiStringToken<'input>, Range<usize>)>,
        logos_lexer: MultiStringLexer<'input>,
    },
}

// Wrap the `next()` function of the underlying lexer.
impl<'input> Iterator for ModalLexer<'input> {
    type Item = Token<'input>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            ModalLexer::Normal { logos_lexer, .. } => logos_lexer.next().map(Token::Normal),
            ModalLexer::String { logos_lexer } => logos_lexer.next().map(Token::Str),
            ModalLexer::MultiString { logos_lexer, .. } => logos_lexer.next().map(Token::MultiStr),
        }
    }
}

/// State associated to the lexer in multiline string mode.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct MultiStrData {
    /// The number of characters of the starting delimiter, required to correctly detect the end of
    /// multiline strings.
    percent_count: usize,
    /// The position of the opening delimiter of the current multiline string. Used for error
    /// reporting.
    opening_delimiter: Range<usize>,
}

/// State associated to the lexer in normal mode.
#[derive(Clone, PartialEq, Eq, Debug, Default)]
pub struct NormalData {
    /// The current brace counter to determine if a closing brace is the end of
    /// an interpolated expression.
    brace_count: usize,
}

impl NormalData {
    pub fn new() -> Self {
        Default::default()
    }
}

/// Possible lexer modes together with their associated state. `Mode` values are pushed on a stack
/// when entering a new mode and popped when a mode is exited. The associated mode data are
/// restored when restoring a previous mode.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Mode {
    /// When lexing a normal (double quotes) string.
    String,
    /// When lexing a multiline string.
    MultiString(MultiStrData),
    /// When lexing a normal Nickel expression.
    Normal(NormalData),
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
    /// The mode stack. Whenever a new mode is entered (starting to lex a string for example), the
    /// previous mode together with its associated state is pushed on this stack. It can be then
    /// restored once the current mode is exited (in the string example, when the string ends).
    pub modes: Vec<Mode>,
}

impl<'input> Lexer<'input> {
    pub fn new(s: &'input str) -> Self {
        Lexer {
            lexer: Some(ModalLexer::Normal {
                mode_data: NormalData { brace_count: 0 },
                logos_lexer: NormalToken::lexer(s),
            }),
            modes: Vec::new(),
        }
    }

    fn enter_strlike<F>(&mut self, morph: F)
    where
        F: FnOnce(NormalLexer<'input>) -> ModalLexer<'input>,
    {
        match self.lexer.take() {
            // Cannot transition from a string mode to another one, so the current mode must be
            // `Normal`
            Some(ModalLexer::Normal {
                mode_data,
                logos_lexer,
            }) => {
                self.modes.push(Mode::Normal(mode_data));
                self.lexer = Some(morph(logos_lexer));
            }
            _ => panic!("lexer::enter_strlike"),
        }
    }

    fn enter_str(&mut self) {
        self.enter_strlike(|lexer| ModalLexer::String {
            logos_lexer: lexer.morph(),
        });
    }

    fn enter_indstr(&mut self, percent_count: usize, opening_delimiter: Range<usize>) {
        self.enter_strlike(|lexer| ModalLexer::MultiString {
            mode_data: MultiStrData {
                percent_count,
                opening_delimiter,
            },
            buffer: None,
            logos_lexer: lexer.morph(),
        });
    }

    fn enter_normal(&mut self) {
        match self.lexer.take() {
            Some(ModalLexer::String { logos_lexer }) => {
                self.lexer = Some(ModalLexer::Normal {
                    mode_data: NormalData::new(),
                    logos_lexer: logos_lexer.morph(),
                });

                self.modes.push(Mode::String);
            }
            Some(ModalLexer::MultiString {
                mode_data,
                logos_lexer,
                buffer: _,
            }) => {
                self.lexer = Some(ModalLexer::Normal {
                    mode_data: NormalData::new(),
                    logos_lexer: logos_lexer.morph(),
                });

                self.modes.push(Mode::MultiString(mode_data));
            }
            _ => panic!("lexer::enter_normal"),
        }
    }

    fn leave_str(&mut self) {
        match self.lexer.take() {
            Some(ModalLexer::String { logos_lexer }) => {
                // We can only enter string mode from normal mode
                let Some(Mode::Normal(mode_data)) = self.modes.pop() else {
                    panic!("lexer::leave_str (popped wrong mode)");
                };

                self.lexer = Some(ModalLexer::Normal {
                    mode_data,
                    logos_lexer: logos_lexer.morph(),
                });
            }
            _ => panic!("lexer::leave_str"),
        }
    }

    fn leave_indstr(&mut self) {
        match self.lexer.take() {
            Some(ModalLexer::MultiString { logos_lexer, .. }) => {
                // We can only enter string mode from normal mode
                let Some(Mode::Normal(data)) = self.modes.pop() else {
                    panic!("lexer::leave_str (popped wrong mode)");
                };

                self.lexer = Some(ModalLexer::Normal {
                    mode_data: data,
                    logos_lexer: logos_lexer.morph(),
                });
            }
            _ => panic!("lexer::leave_str"),
        }
    }

    fn leave_normal(&mut self) {
        match self.lexer.take() {
            Some(ModalLexer::Normal { logos_lexer, .. }) => {
                match self.modes.pop() {
                    Some(Mode::String) => {
                        self.lexer = Some(ModalLexer::String {
                            logos_lexer: logos_lexer.morph(),
                        })
                    }
                    Some(Mode::MultiString(data)) => {
                        self.lexer = Some(ModalLexer::MultiString {
                            mode_data: data,
                            buffer: None,
                            logos_lexer: logos_lexer.morph(),
                        })
                    }
                    mode => panic!("lexer::leave_normal (popped mode {mode:?})"),
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
        percent_count: usize,
    ) -> (Token<'input>, Range<usize>) {
        let split_at = s.len() - percent_count;
        let next_token = MultiStringToken::Interpolation;
        let next_span = Range {
            start: span.start + split_at,
            end: span.end,
        };
        self.bufferize(next_token, next_span);

        let token = Token::MultiStr(MultiStringToken::Literal(s[0..split_at].to_owned()));
        let span = Range {
            start: span.start,
            end: span.start + split_at,
        };

        (token, span)
    }

    // Handle a normal token, updating the mode data if necessary.
    fn handle_normal_token(
        &mut self,
        span: Range<usize>,
        token: NormalToken<'input>,
    ) -> Option<Result<SpannedToken<'input>, ParseError>> {
        match token {
            NormalToken::DoubleQuote | NormalToken::StrEnumTagBegin => self.enter_str(),
            NormalToken::MultiStringStart(delim_size)
            | NormalToken::SymbolicStringStart(SymbolicStringStart {
                length: delim_size, ..
            }) => {
                // for interpolation & closing delimiters we only care about
                // the number of `%`s (plus the opening `"` or `{`) so we
                // drop the "kind marker" size here (i.e. the `m` character).
                let size_without_kind_marker = delim_size - 1;
                // unwrap(): the lexer must always
                self.enter_indstr(size_without_kind_marker, span.clone())
            }
            NormalToken::LBrace => {
                self.normal_mode_data_mut().brace_count += 1;
            }
            NormalToken::RBrace => {
                let data = self.normal_mode_data_mut();
                if data.brace_count == 0 {
                    if self.modes.is_empty() {
                        return Some(Err(ParseError::Lexical(LexicalError::UnmatchedCloseBrace(
                            span.start,
                        ))));
                    }

                    self.leave_normal();
                } else {
                    data.brace_count -= 1;
                }
            }
            // Ignore comment
            NormalToken::LineComment => return self.next(),
            NormalToken::Error => {
                return Some(Err(ParseError::Lexical(LexicalError::Generic(span))))
            }
            _ => (),
        };

        Some(Ok((span.start, Token::Normal(token), span.end)))
    }

    // Handle a string token. This method currently doesn't have any side effect, as in string
    // mode, there's no state to update.
    fn handle_string_token(
        &mut self,
        span: Range<usize>,
        token: StringToken<'input>,
    ) -> Option<Result<SpannedToken<'input>, ParseError>> {
        let result = match token {
            StringToken::DoubleQuote => {
                self.leave_str();
                // To make things simpler on the parser side, we only return one variant for
                // `DoubleQuote`, namely the the normal one.
                Token::Normal(NormalToken::DoubleQuote)
            }
            tok @ StringToken::Interpolation => {
                self.enter_normal();
                Token::Str(tok)
            }
            // Convert escape sequences to the corresponding character.
            StringToken::EscapedChar(c) => {
                if let Some(esc) = escape_char(c) {
                    Token::Str(StringToken::EscapedChar(esc))
                } else {
                    return Some(Err(ParseError::Lexical(
                        LexicalError::InvalidEscapeSequence(span.start + 1),
                    )));
                }
            }
            StringToken::EscapedAscii(code) => {
                if let Some(esc) = escape_ascii(code) {
                    Token::Str(StringToken::EscapedChar(esc))
                } else {
                    return Some(Err(ParseError::Lexical(
                        LexicalError::InvalidAsciiEscapeCode(span.start + 2),
                    )));
                }
            }
            StringToken::Error => {
                return Some(Err(ParseError::Lexical(LexicalError::Generic(span))))
            }
            token => Token::Str(token),
        };

        Some(Ok((span.start, result, span.end)))
    }

    // Handle a multistring token. Might push a token inside the buffer.
    fn handle_multistr_token(
        &mut self,
        mut span: Range<usize>,
        token: MultiStringToken<'input>,
    ) -> Option<Result<SpannedToken<'input>, ParseError>> {
        let data = self.multistring_mode_data();

        let result = match token {
            // If we encounter a `CandidateInterp` token with the right number of characters, this
            // is an interpolation sequence.
            //
            // Note that the number of characters may be greater than `count`: in `m%" %%%{foo} "%`,
            // the lexer will process `%%%{` as a candidate interpolation with 4 characters, while
            // `count` is 2. In that case, we must emit a `%%` literal and put an interpolation
            // token in the buffer.
            MultiStringToken::CandidateInterpolation(s) if s.len() >= data.percent_count => {
                if s.len() == data.percent_count {
                    self.enter_normal();
                    Token::MultiStr(MultiStringToken::Interpolation)
                } else {
                    let (token_fst, span_fst) =
                        self.split_candidate_interp(s, span, data.percent_count);
                    span = span_fst;
                    token_fst
                }
            }
            // We never lex something as a `MultiStringToken::Interpolation` directly, but rather
            // generate it in this very function from other tokens. However, such a token could
            // have still been buffered in the previous iteration, and can thus be matched here,
            // which is why we need the case below.
            tok @ MultiStringToken::Interpolation => {
                self.enter_normal();
                Token::MultiStr(tok)
            }
            // If we encounter a `QuotesCandidateInterpolation` token with as many `%` characters
            // as the current count or more, we need to split it into two tokens:
            //
            // - a string literal corresponding to the `"` followed by `(s.len() - self.count)`
            // `%`s
            // - an interpolation token
            //
            // The interpolation token is put in the buffer to returned next time.
            //
            // For example, in `m%""%%{exp}"%`, the `"%%{` is a `QuotesCandidateInterpolation`
            // which is split as a `"%` literal followed by an interpolation token.
            MultiStringToken::QuotesCandidateInterpolation(s) if s.len() > data.percent_count => {
                let (token_fst, span_fst) =
                    self.split_candidate_interp(s, span, data.percent_count);
                span = span_fst;
                token_fst
            }
            // Otherwise, it is just part of the string, so we transform the token into a
            // `Literal` one
            MultiStringToken::CandidateInterpolation(s)
            | MultiStringToken::QuotesCandidateInterpolation(s) => {
                Token::MultiStr(MultiStringToken::Literal(s.to_owned()))
            }
            // Strictly speaking, a candidate end delimiter with more than the required count of
            // `%` should be split between multistring end token, plus a variable number of `%`
            // tokens. This is annoying because we only buffer one token currently. We could use a
            // stack instead of a 1-length buffer, but in practice a string such as `m%" "%%` is
            // almost surely meaningless: there's no meaningful way of interpreting it
            // (although according to the grammar, it might be valid as a string followed by a
            // modulo operator `%` - which will fail anyway at runtime with a type error).
            // Thus, we prefer to emit a proper error right here.
            MultiStringToken::CandidateEnd(s) if s.len() > data.percent_count => {
                return Some(Err(ParseError::Lexical(
                    LexicalError::StringDelimiterMismatch {
                        opening_delimiter: data.opening_delimiter.clone(),
                        closing_delimiter: span,
                    },
                )))
            }
            // If we encounter a `CandidateEnd` token with the same number of `%`s as the
            // starting token then it is the end of a multiline string
            MultiStringToken::CandidateEnd(s) if s.len() == data.percent_count => {
                self.leave_indstr();
                Token::MultiStr(MultiStringToken::End)
            }
            // Otherwise, it is just part of the string, so we transform the token into a
            // `Literal` one
            MultiStringToken::CandidateEnd(s) => {
                Token::MultiStr(MultiStringToken::Literal(s.to_owned()))
            }
            // Early report errors for now. This could change in the future
            MultiStringToken::Error => {
                return Some(Err(ParseError::Lexical(LexicalError::Generic(span))))
            }
            token => Token::MultiStr(token),
        };

        Some(Ok((span.start, result, span.end)))
    }

    // WARNING: this method expects the lexer to be in normal mode. Panics otherwise.
    // Ideally, we wouldn't have to match on `self.lexer` again and have this (hopefully)
    // unreachable `panic!`. In practice, the fact that `handle_normal_token` might both mutate
    // `mode_data` or switch mode (and thus get rid of the current lexer, which holds mode_data)
    // altogether makes it hard to do something that is both ergonomic and satisfies the borrow
    // checker.
    // We initially tried to thread `data` through `handle_normal_token`, but this not only
    // requires to clone the data to avoid multiple mutable borrows to `self`, but also had a
    // subtly wrong behavior because when reaching a comment, we call `self.next()`, and threading
    // data properly becomes non trivial.
    fn normal_mode_data_mut(&mut self) -> &mut NormalData {
        match self.lexer {
            Some(ModalLexer::Normal {
                ref mut mode_data, ..
            }) => mode_data,
            _ => panic!("lexer: normal_mode_data() called while not in normal mode"),
        }
    }

    fn multistring_mode_data(&self) -> &MultiStrData {
        match self.lexer {
            Some(ModalLexer::MultiString { ref mode_data, .. }) => mode_data,
            _ => panic!("lexer: multistring_mode_data() called while not in multistring mode"),
        }
    }

    // WARNING: this method expects the lexer to be in multistring mode. Panics otherwise.
    fn bufferize(&mut self, token: MultiStringToken<'input>, span: Range<usize>) {
        match self.lexer {
            Some(ModalLexer::MultiString { ref mut buffer, .. }) => *buffer = Some((token, span)),
            _ => panic!("lexer: bufferize() called while not in normal mode"),
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Result<SpannedToken<'input>, ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.lexer.as_mut().unwrap() {
            ModalLexer::Normal { logos_lexer, .. } => {
                let normal_token = logos_lexer.next()?;
                let span = logos_lexer.span();
                self.handle_normal_token(span, normal_token)
            }
            ModalLexer::String { logos_lexer } => {
                let string_token = logos_lexer.next()?;
                let span = logos_lexer.span();
                self.handle_string_token(span, string_token)
            }
            ModalLexer::MultiString {
                buffer,
                logos_lexer,
                ..
            } => {
                let (multistr_token, span) = buffer
                    .take()
                    .or_else(|| Some((logos_lexer.next()?, logos_lexer.span())))?;

                self.handle_multistr_token(span, multistr_token)
            }
        }
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

/// Normalize the line endings in `s` to only `\n` and, in debug mode, check
/// for lone `\r` without an accompanying `\n`.
pub fn normalize_line_endings(s: impl AsRef<str>) -> String {
    let normalized = s.as_ref().replace("\r\n", "\n");
    debug_assert!(
        normalized.find('\r').is_none(),
        "The lexer throws an error when it finds a lone carriage return"
    );
    normalized
}
