//! Error types and error reporting.
//!
//! Define error types for different phases of the execution, together with functions to generate a
//! [codespan](https://crates.io/crates/codespan-reporting) diagnostic from them.
use std::fmt::Write;

use codespan::{FileId, Files};
use codespan_reporting::diagnostic::{Diagnostic, Label, LabelStyle};
use lalrpop_util::ErrorRecovery;

use crate::{
    eval::callstack::CallStack,
    identifier::Ident,
    label::{
        self,
        ty_path::{self, PathSpan},
    },
    parser::{
        self,
        error::{LexicalError, ParseError as InternalParseError},
        lexer::Token,
        utils::mk_span,
    },
    position::{RawSpan, TermPos},
    repl,
    serialize::ExportFormat,
    term::{record::FieldMetadata, RichTerm},
    types::{TypeF, Types},
};

use self::blame_error::ExtendWithCallStack;

/// A general error occurring during either parsing or evaluation.
#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    EvalError(EvalError),
    TypecheckError(TypecheckError),
    ParseErrors(ParseErrors),
    ImportError(ImportError),
    SerializationError(SerializationError),
    IOError(IOError),
    ReplError(ReplError),
}

/// An error occurring during evaluation.
#[derive(Debug, Clone, PartialEq)]
pub enum EvalError {
    /// A blame occurred: a contract have been broken somewhere.
    BlameError {
        /// The argument failing the contract. If the argument has been forced by the contract, `evaluated_arg` provides the final value.
        evaluated_arg: Option<RichTerm>,
        /// The label of the corresponding contract.
        label: label::Label,
        /// The callstack when the blame error was raised.
        call_stack: CallStack,
    },
    /// A field required by a record contract is missing a definition.
    MissingFieldDef {
        id: Ident,
        metadata: FieldMetadata,
        pos_record: TermPos,
        pos_access: TermPos,
    },
    /// Mismatch between the expected type and the actual type of an expression.
    TypeError(
        /* expected type */ String,
        /* operation */ String,
        /* position of the original unevaluated expression */ TermPos,
        /* evaluated expression */ RichTerm,
    ),
    /// Tried to evaluate a term which wasn't parsed correctly.
    ParseError(ParseError),
    /// A term which is not a function has been applied to an argument.
    NotAFunc(
        /* term */ RichTerm,
        /* arg */ RichTerm,
        /* app position */ TermPos,
    ),
    /// A field access, or another record operation requiring the existence of a specific field,
    /// has been performed on a record missing that field.
    FieldMissing(
        /* field identifier */ String,
        /* operator */ String,
        RichTerm,
        TermPos,
    ),
    /// Too few arguments were provided to a builtin function.
    NotEnoughArgs(
        /* required arg count */ usize,
        /* primitive */ String,
        TermPos,
    ),
    /// Attempted to merge incompatible values: for example, tried to merge two distinct default
    /// values into one record field.
    MergeIncompatibleArgs(
        /* left operand */ RichTerm,
        /* right operand */ RichTerm,
        /* original merge */ TermPos,
    ),
    /// An unbound identifier was referenced.
    UnboundIdentifier(Ident, TermPos),
    /// An element in the evaluation Cache was entered during its own update.
    InfiniteRecursion(CallStack, TermPos),
    /// A serialization error occurred during a call to the builtin `serialize`.
    SerializationError(SerializationError),
    /// A parse error occurred during a call to the builtin `deserialize`.
    DeserializationError(
        String,  /* format */
        String,  /* error message */
        TermPos, /* position of the call to deserialize */
    ),
    /// A polymorphic record contract was broken somewhere.
    IllegalPolymorphicTailAccess {
        action: IllegalPolymorphicTailAction,
        evaluated_arg: Option<RichTerm>,
        label: label::Label,
        call_stack: CallStack,
    },
    /// A non-equatable term was compared for equality.
    EqError { eq_pos: TermPos, term: RichTerm },
    /// An unexpected internal error.
    InternalError(String, TermPos),
    /// Errors occurring rarely enough to not deserve a dedicated variant.
    Other(String, TermPos),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum IllegalPolymorphicTailAction {
    Map,
    Merge,
}

impl IllegalPolymorphicTailAction {
    fn message(&self) -> &'static str {
        use IllegalPolymorphicTailAction::*;

        match self {
            Map => "cannot map over a record sealed by a polymorphic contract",
            Merge => "cannot merge a record sealed by a polymorphic contract",
        }
    }
}

/// An error occurring during the static typechecking phase.
#[derive(Debug, PartialEq, Clone)]
pub enum TypecheckError {
    /// An unbound identifier was referenced.
    UnboundIdentifier(Ident, TermPos),
    /// A specific row was expected to be in the type of an expression, but was not.
    MissingRow(
        Ident,
        /* the expected type */ Types,
        /* the inferred/annotated type */ Types,
        TermPos,
    ),
    /// A dynamic tail was expected to be in the type of an expression, but was not.
    MissingDynTail(
        /* the expected type */ Types,
        /* the inferred/annotated type */ Types,
        TermPos,
    ),
    /// A specific row was not expected to be in the type of an expression.
    ExtraRow(
        Ident,
        /* the expected type */ Types,
        /* the inferred/annotated type */ Types,
        TermPos,
    ),
    /// A additional dynamic tail was not expected to be in the type of an expression.
    ExtraDynTail(
        /* the expected type */ Types,
        /* the inferred/annotated type */ Types,
        TermPos,
    ),

    /// An unbound type variable was referenced.
    UnboundTypeVariable(Ident, TermPos),
    /// The actual (inferred or annotated) type of an expression is incompatible with its expected
    /// type.
    TypeMismatch(
        /* the expected type */ Types,
        /* the actual type */ Types,
        TermPos,
    ),
    /// Two incompatible kind (enum vs record) have been deduced for the same identifier of a row type.
    RowKindMismatch(
        Ident,
        /* the expected type */ Option<Types>,
        /* the actual type */ Option<Types>,
        TermPos,
    ),
    /// Two incompatible types have been deduced for the same identifier in a row type.
    RowMismatch(
        Ident,
        /* the expected row type (whole) */ Types,
        /* the actual row type (whole) */ Types,
        /* error at the given row */ Box<TypecheckError>,
        TermPos,
    ),
    /// Two incompatible types have been deduced for the same identifier of a row type.
    ///
    /// This is similar to `RowKindMismatch` but occurs in a slightly different situation. Consider a a
    /// unification variable `t`, which is a placeholder to be filled by a concrete type later in
    /// the typechecking phase.  If `t` appears as the tail of a row type, i.e. the type of some
    /// expression is inferred to be `{ field: Type | t}`, then `t` must not be unified later with
    /// a type including a different declaration for field, such as `field: Type2`.
    ///
    /// A [constraint][crate::typecheck::RowConstr] is added accordingly, and if this constraint is
    /// violated (that is if `t` does end up being unified with a type of the form
    /// `{ .., field: Type2, .. }`), `RowConflict` is raised.  We do not have access to the
    /// original `field: Type` declaration, as opposed to `RowKindMismatch`, which corresponds to
    /// the direct failure to unify `{ .. , x: T1, .. }` and `{ .., x: T2, .. }`.
    RowConflict(
        Ident,
        /* the second type assignment which violates the constraint */ Option<Types>,
        /* the expected type of the subexpression */ Types,
        /* the actual type of the subexpression */ Types,
        TermPos,
    ),
    /// Type mismatch on a subtype of an an arrow type.
    ///
    /// The unification of two arrow types requires the unification of the domain and the codomain
    /// (and recursively so, if they are themselves arrow types). When the unification of a subtype
    /// fails, we want to report which part of the arrow types is problematic, and why, rather than
    /// a generic `TypeMismatch`. Indeed, failing to unify two arrow types is a common type error
    /// which deserves a good reporting, that can be caused e.g. by applying a function to an
    /// argument of a wrong type in some cases:
    ///
    /// ```text
    /// let id_mono = fun x => x in let _ign = id_mono true in id_mono 0 : Num
    /// ```
    ///
    /// This specific error stores additionally the [type path][crate::label::ty_path] that
    /// identifies the subtype where unification failed and the corresponding error.
    ArrowTypeMismatch(
        /* the expected arrow type */ Types,
        /* the actual arrow type */ Types,
        /* the path to the incompatible subtypes */ ty_path::Path,
        /* the error on the subtype unification */ Box<TypecheckError>,
        TermPos,
    ),
    /// This is a temporary error due to the current limitation of checking type equality between
    /// contracts. Checking if the unification of two contracts is valid is non-trivial even in
    /// simple cases (see [#724](https://github.com/tweag/nickel/issues/724)). Currently, the
    /// typechecker simply bails out of any attempt of contract unification, represented by this
    /// error.
    IncomparableFlatTypes(
        RichTerm, /* the expected flat type */
        RichTerm, /* the inferred flat type */
        TermPos,
    ),
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

    pub fn from_recoverable(
        errs: Vec<ErrorRecovery<usize, Token<'_>, parser::error::ParseError>>,
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

impl ToDiagnostic<FileId> for ParseErrors {
    fn to_diagnostic(
        &self,
        files: &mut Files<String>,
        contract_id: Option<FileId>,
    ) -> Vec<Diagnostic<FileId>> {
        self.errors
            .iter()
            .flat_map(|e| e.to_diagnostic(files, contract_id))
            .collect()
    }
}

/// An error occurring during parsing.
#[derive(Debug, PartialEq, Eq, Clone)]
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
    /// A closing brace '}' does not match an opening brace '{'. This rather precise error is detected by the because
    /// of how interpolated strings are lexed.
    UnmatchedCloseBrace(RawSpan),
    /// Invalid escape sequence in a string literal.
    InvalidEscapeSequence(RawSpan),
    /// Invalid ASCII escape code in a string literal.
    InvalidAsciiEscapeCode(RawSpan),
    /// Error when parsing an external format such as JSON, YAML, etc.
    ExternalFormatError(
        String, /* format */
        String, /* error message */
        Option<RawSpan>,
    ),
    /// Unbound type variable
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
}

/// An error occurring during the resolution of an import.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ImportError {
    /// An IO error occurred during an import.
    IOError(
        /* imported file */ String,
        /* error message */ String,
        /* import position */ TermPos,
    ),
    /// A parse error occurred during an import.
    ParseErrors(
        /* error */ ParseErrors,
        /* import position */ TermPos,
    ),
}

/// An error occurred during serialization.
#[derive(Debug, PartialEq, Clone)]
pub enum SerializationError {
    /// Encountered a null value for a format that doesn't support them.
    UnsupportedNull(ExportFormat, RichTerm),
    /// Tried exporting something else than a `Str` to raw format.
    NotAString(RichTerm),
    /// A term contains constructs that cannot be serialized.
    NonSerializable(RichTerm),
    Other(String),
}

/// A general I/O error, occurring when reading a source file or writing an export.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IOError(pub String);

/// An error occurring during an REPL session.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ReplError {
    UnknownCommand(String),
    MissingArg {
        cmd: repl::command::CommandType,
        msg_opt: Option<String>,
    },
}

impl From<EvalError> for Error {
    fn from(error: EvalError) -> Error {
        Error::EvalError(error)
    }
}

impl From<ParseError> for Error {
    fn from(error: ParseError) -> Error {
        Error::ParseErrors(ParseErrors {
            errors: vec![error],
        })
    }
}

impl From<ParseErrors> for Error {
    fn from(errors: ParseErrors) -> Error {
        Error::ParseErrors(errors)
    }
}

impl From<TypecheckError> for Error {
    fn from(error: TypecheckError) -> Error {
        Error::TypecheckError(error)
    }
}

impl From<ImportError> for Error {
    fn from(error: ImportError) -> Error {
        Error::ImportError(error)
    }
}

impl From<SerializationError> for Error {
    fn from(error: SerializationError) -> Error {
        Error::SerializationError(error)
    }
}

impl From<IOError> for Error {
    fn from(error: IOError) -> Error {
        Error::IOError(error)
    }
}

impl From<std::io::Error> for IOError {
    fn from(error: std::io::Error) -> IOError {
        IOError(error.to_string())
    }
}

impl From<SerializationError> for EvalError {
    fn from(error: SerializationError) -> EvalError {
        EvalError::SerializationError(error)
    }
}

/// Return an escaped version of a string. Used to sanitize strings before inclusion in error
/// messages, which can contain ASCII code sequences, and in particular ANSI escape codes, that
/// could alter Nickel's error messages.
pub fn escape(s: &str) -> String {
    String::from_utf8(
        s.bytes()
            .flat_map(std::ascii::escape_default)
            .collect::<Vec<u8>>(),
    )
    .expect("escape(): converting from a string should give back a valid UTF8 string")
}

impl From<ReplError> for Error {
    fn from(error: ReplError) -> Error {
        Error::ReplError(error)
    }
}

impl ParseError {
    pub fn from_lalrpop<T>(
        error: lalrpop_util::ParseError<usize, T, InternalParseError>,
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
            lalrpop_util::ParseError::UnrecognizedEOF { expected, .. } => {
                ParseError::UnexpectedEOF(file_id, expected)
            }
            lalrpop_util::ParseError::ExtraToken {
                token: (start, _, end),
            } => ParseError::ExtraToken(mk_span(file_id, start, end)),
            lalrpop_util::ParseError::User { error } => match error {
                InternalParseError::Lexical(LexicalError::Generic(start, end)) => {
                    ParseError::UnexpectedToken(mk_span(file_id, start, end), Vec::new())
                }
                InternalParseError::Lexical(LexicalError::UnmatchedCloseBrace(location)) => {
                    ParseError::UnmatchedCloseBrace(mk_span(file_id, location, location + 1))
                }
                InternalParseError::Lexical(LexicalError::InvalidEscapeSequence(location)) => {
                    ParseError::InvalidEscapeSequence(mk_span(file_id, location, location + 1))
                }
                InternalParseError::Lexical(LexicalError::InvalidAsciiEscapeCode(location)) => {
                    ParseError::InvalidAsciiEscapeCode(mk_span(file_id, location, location + 2))
                }
                InternalParseError::UnboundTypeVariables(idents, span) => {
                    ParseError::UnboundTypeVariables(idents, span)
                }
                InternalParseError::InvalidUniRecord(illegal_pos, tail_pos, pos) => {
                    ParseError::InvalidUniRecord(illegal_pos, tail_pos, pos)
                }
                InternalParseError::RecursiveLetPattern(pos) => {
                    ParseError::RecursiveLetPattern(pos)
                }
                InternalParseError::TypeVariableKindMismatch { ty_var, span } => {
                    ParseError::TypeVariableKindMismatch { ty_var, span }
                }
            },
        }
    }

    pub fn from_serde_json(
        error: serde_json::Error,
        file_id: FileId,
        files: &Files<String>,
    ) -> Self {
        use codespan::ByteOffset;

        // error.line() should start at `1` according to the documentation, but in practice, it may
        // be 0 for the error `json parse error: data did not match any variant of untagged enum
        // Term`. Although this error should not happen, if it does, it's better to get a message
        // than a panic message `subtract with overflow`.
        let line_span = if error.line() == 0 {
            None
        } else {
            files.line_span(file_id, (error.line() - 1) as u32).ok()
        };

        let start = line_span.map(|ls| ls.start() + ByteOffset::from(error.column() as i64 - 1));
        ParseError::ExternalFormatError(
            String::from("json"),
            error.to_string(),
            start.map(|start| RawSpan {
                src_id: file_id,
                start,
                end: start + ByteOffset::from(1),
            }),
        )
    }

    pub fn from_serde_yaml(error: serde_yaml::Error, file_id: FileId) -> Self {
        use codespan::{ByteIndex, ByteOffset};

        let start = error
            .location()
            .map(|loc| loc.index() as u32)
            .map(ByteIndex::from);
        ParseError::ExternalFormatError(
            String::from("yaml"),
            error.to_string(),
            start.map(|start| RawSpan {
                src_id: file_id,
                start,
                end: start + ByteOffset::from(1),
            }),
        )
    }

    pub fn from_toml(error: toml::de::Error, file_id: FileId, files: &Files<String>) -> Self {
        use codespan::ByteOffset;

        let start = error.line_col().and_then(|(line, col)| {
            Some(files.line_span(file_id, line as u32).ok()?.start() + ByteOffset::from(col as i64))
        });
        ParseError::ExternalFormatError(
            String::from("toml"),
            error.to_string(),
            start.map(|start| RawSpan {
                src_id: file_id,
                start,
                end: start + ByteOffset::from(1),
            }),
        )
    }
}

pub const INTERNAL_ERROR_MSG: &str =
    "This error should not happen. This is likely a bug in the Nickel interpreter. Please consider\
 reporting it at https://github.com/tweag/nickel/issues with the above error message.";

/// A trait for converting an error to a diagnostic.
pub trait ToDiagnostic<FileId> {
    /// Convert an error to a list of printable formatted diagnostic.
    ///
    /// # Arguments
    ///
    /// - `files`: to know why it takes a mutable reference to `Files<String>`, see
    ///   `label_alt`.
    /// - `contract_id` is required to format the callstack when reporting blame errors. For some
    ///   errors (such as [`ParseError`])), contracts may not have been loaded yet, hence the
    ///   optional. See also [`crate::eval::callstack::CallStack::group_by_calls`].
    ///
    /// # Return
    ///
    /// Return a list of diagnostics. Most errors generate only one, but showing the callstack
    /// ordered requires to sidestep a limitation of codespan. The current solution is to generate
    /// one diagnostic per callstack element. See issue
    /// [#285](https://github.com/brendanzab/codespan/issues/285).
    fn to_diagnostic(
        &self,
        files: &mut Files<String>,
        contract_id: Option<FileId>,
    ) -> Vec<Diagnostic<FileId>>;
}

// Helpers for the creation of codespan `Label`s

/// Create a primary label from a span.
fn primary(span: &RawSpan) -> Label<FileId> {
    Label::primary(span.src_id, span.start.to_usize()..span.end.to_usize())
}

/// Create a secondary label from a span.
fn secondary(span: &RawSpan) -> Label<FileId> {
    Label::secondary(span.src_id, span.start.to_usize()..span.end.to_usize())
}

/// Create a label from an optional span, or fallback to annotating the alternative snippet
/// `alt_term` if the span is `None`.
///
/// When `span_opt` is `None`, the code snippet `alt_term` is added to `files` under a special
/// name and is referred to instead.
///
/// This is useful because during evaluation, some terms are the results of computations. They
/// correspond to nothing in the original source, and thus have a position set to `None`(e.g. the
/// result of `let x = 1 + 1 in x`).  In such cases it may still be valuable to print the term (or
/// a terse representation) in the error diagnostic rather than nothing, because if you have let `x
/// = 1 + 1 in` and then 100 lines later, `x arg` - causing a `NotAFunc` error - it may be helpful
/// to know that `x` holds the value `2`.
///
/// For example, if one wants to report an error on a record, `alt_term` may be defined to `{ ...  }`.
/// Then, if this record has no position (`span_opt` is `None`), the error will be reported as:
///
/// ```text
/// error: some error
///   -- <unknown> (generated by evaluation):1:2
///   |
/// 1 | { ... }
///     ^^^^^^^ some annotation
/// ```
///
/// The reason for the mutable reference to `files` is that codespan do no let you annotate
/// something that is not in `files`: you can't provide a raw snippet, you need to provide a
/// `FileId` referring to a file. This leaves the following possibilities:
///
/// 1. Do nothing: just elude annotations which refer to the term
/// 2. Print the term and the annotation as a note together with the diagnostic. Notes are
///    additional text placed at the end of diagnostic. What you lose:
///     - pretty formatting of annotations for such snippets
///     - style consistency: the style of the error now depends on the term being from the source
///     or a byproduct of evaluation
/// 3. Add the term to files, take 1: pass a reference to files so that the code building the
///    diagnostic can itself add arbitrary snippets if necessary, and get back their `FileId`. This
///    is what is done here.
/// 4. Add the term to files, take 2: make a wrapper around the `Files` and `FileId` structures of
///    codespan which handle source mapping. `FileId` could be something like
///    `Either<codespan::FileId, CustomId = u32>` so that `to_diagnostic` could construct and use these
///    separate ids, and return the corresponding snippets to be added together with the
///    diagnostic without modifying external state. Or even have `FileId = Either<codespan::FileId`,
///    `LoneCode = String or (Id, String)>` so we don't have to return the additional list of
///    snippets. This adds some boilerplate, that we wanted to avoid, but this stays on the
///    reasonable side of being an alternative.
fn label_alt(
    span_opt: Option<RawSpan>,
    alt_term: String,
    style: LabelStyle,
    files: &mut Files<String>,
) -> Label<FileId> {
    match span_opt {
        Some(span) => Label::new(
            style,
            span.src_id,
            span.start.to_usize()..span.end.to_usize(),
        ),
        None => {
            let range = 0..alt_term.len();
            Label::new(
                style,
                files.add("<unknown> (generated by evaluation)", alt_term),
                range,
            )
        }
    }
}

/// Create a secondary label from an optional span, or fallback to annotating the alternative snippet
/// `alt_term` if the span is `None`.
///
/// See [`label_alt`].
fn primary_alt(
    span_opt: Option<RawSpan>,
    alt_term: String,
    files: &mut Files<String>,
) -> Label<FileId> {
    label_alt(span_opt, alt_term, LabelStyle::Primary, files)
}

/// Create a primary label from a term, or fallback to annotating the shallow representation of this term
/// if its span is `None`.
///
/// See [`label_alt`].
fn primary_term(term: &RichTerm, files: &mut Files<String>) -> Label<FileId> {
    primary_alt(term.pos.into_opt(), term.as_ref().shallow_repr(), files)
}

/// Create a secondary label from an optional span, or fallback to annotating the alternative snippet
/// `alt_term` if the span is `None`.
///
/// See [`label_alt`].
fn secondary_alt(span_opt: TermPos, alt_term: String, files: &mut Files<String>) -> Label<FileId> {
    label_alt(span_opt.into_opt(), alt_term, LabelStyle::Secondary, files)
}

/// Create a secondary label from a term, or fallback to annotating the shallow representation of this term
/// if its span is `None`.
///
/// See [`label_alt`].
fn secondary_term(term: &RichTerm, files: &mut Files<String>) -> Label<FileId> {
    secondary_alt(term.pos, term.as_ref().shallow_repr(), files)
}

impl ToDiagnostic<FileId> for Error {
    fn to_diagnostic(
        &self,
        files: &mut Files<String>,
        contract_id: Option<FileId>,
    ) -> Vec<Diagnostic<FileId>> {
        match self {
            Error::ParseErrors(errs) => errs
                .errors
                .iter()
                .flat_map(|e| e.to_diagnostic(files, contract_id))
                .collect(),
            Error::TypecheckError(err) => err.to_diagnostic(files, contract_id),
            Error::EvalError(err) => err.to_diagnostic(files, contract_id),
            Error::ImportError(err) => err.to_diagnostic(files, contract_id),
            Error::SerializationError(err) => err.to_diagnostic(files, contract_id),
            Error::IOError(err) => err.to_diagnostic(files, contract_id),
            Error::ReplError(err) => err.to_diagnostic(files, contract_id),
        }
    }
}

impl ToDiagnostic<FileId> for EvalError {
    fn to_diagnostic(
        &self,
        files: &mut Files<String>,
        contract_id: Option<FileId>,
    ) -> Vec<Diagnostic<FileId>> {
        match self {
            EvalError::BlameError {
                evaluated_arg,
                label,
                call_stack,
            } => {
                let mut msg = String::new();

                // Writing in a string should not raise an error, hence the fearless `unwrap()`
                write!(&mut msg, "{}", blame_error::title(label)).unwrap();

                if !label.tag.is_empty() {
                    write!(&mut msg, ": {}", &escape(&label.tag)).unwrap();
                }

                let (path_label, notes) = blame_error::report_ty_path(label, files);
                let labels = blame_error::build_diagnostic_labels(
                    evaluated_arg.clone(),
                    label,
                    path_label,
                    files,
                    contract_id,
                );

                let mut diagnostics = vec![Diagnostic::error()
                    .with_message(msg)
                    .with_labels(labels)
                    .with_notes(notes)];

                diagnostics.push(blame_error::note(label));

                if ty_path::has_no_dom(&label.path) {
                } else if let Some(id) = contract_id {
                    diagnostics.extend_with_call_stack(id, call_stack);
                }

                diagnostics
            }
            EvalError::MissingFieldDef {
                id,
                metadata,
                pos_record,
                pos_access,
            } => {
                let mut labels = vec![];

                if let Some(span) = id.pos.into_opt() {
                    labels.push(primary(&span).with_message("defined here"));
                }

                if let Some(span) = pos_record.into_opt() {
                    labels.push(primary(&span).with_message("in this record"));
                }

                if let Some(span) = pos_access.into_opt() {
                    labels.push(secondary(&span).with_message("accessed here"));
                }

                let mut diags = vec![Diagnostic::error()
                    .with_message(format!("missing definition for `{id}`",))
                    .with_labels(labels)
                    .with_notes(vec![])];

                // Is it really useful to include the label if we show the position of the ident?
                // We have to see in practice if it can be the case that `id.pos` is
                // `TermPos::None`, but the label is defined.
                if let Some(label) = metadata
                    .annotation
                    .first()
                    .map(|labeled_ty| labeled_ty.label.clone())
                {
                    diags.push(blame_error::note(&label));
                }

                diags
            }
            EvalError::TypeError(expd, msg, orig_pos_opt, t) => {
                let label = format!(
                    "this expression has type {}, but {} was expected",
                    t.term
                        .type_of()
                        .unwrap_or_else(|| String::from("<unevaluated>")),
                    expd,
                );

                let labels = match orig_pos_opt {
                    TermPos::Original(pos) | TermPos::Inherited(pos) if orig_pos_opt != &t.pos => {
                        vec![
                            primary(pos).with_message(label),
                            secondary_term(t, files).with_message("evaluated to this"),
                        ]
                    }
                    _ => vec![primary_term(t, files).with_message(label)],
                };

                vec![Diagnostic::error()
                    .with_message("type error")
                    .with_labels(labels)
                    .with_notes(vec![msg.clone()])]
            }
            EvalError::ParseError(parse_error) => parse_error.to_diagnostic(files, contract_id),
            EvalError::NotAFunc(t, arg, pos_opt) => vec![Diagnostic::error()
                .with_message("not a function")
                .with_labels(vec![
                    primary_term(t, files)
                        .with_message("this term is applied, but it is not a function"),
                    secondary_alt(
                        *pos_opt,
                        format!(
                            "({}) ({})",
                            (*t.term).shallow_repr(),
                            (*arg.term).shallow_repr()
                        ),
                        files,
                    )
                    .with_message("applied here"),
                ])],
            EvalError::FieldMissing(field, op, t, span_opt) => {
                let mut labels = Vec::new();
                let mut notes = Vec::new();
                let field = escape(field);

                if let Some(span) = span_opt.into_opt() {
                    labels.push(
                        Label::primary(span.src_id, span.start.to_usize()..span.end.to_usize())
                            .with_message(format!("this requires field {field} to exist")),
                    );
                } else {
                    notes.push(format!("field {field} was required by the operator {op}"));
                }

                if let Some(span) = t.pos.as_opt_ref() {
                    labels.push(
                        secondary(span).with_message(format!("field {field} is missing here")),
                    );
                }

                vec![Diagnostic::error()
                    .with_message("missing field")
                    .with_labels(labels)]
            }
            EvalError::NotEnoughArgs(count, op, span_opt) => {
                let mut labels = Vec::new();
                let mut notes = Vec::new();
                let msg = format!("{op} expects {count} arguments, but not enough were provided");

                if let Some(span) = span_opt.into_opt() {
                    labels.push(
                        Label::primary(span.src_id, span.start.to_usize()..span.end.to_usize())
                            .with_message(msg),
                    );
                } else {
                    notes.push(msg);
                }

                vec![Diagnostic::error()
                    .with_message("not enough arguments")
                    .with_labels(labels)
                    .with_notes(notes)]
            }
            EvalError::MergeIncompatibleArgs(t1, t2, span_opt) => {
                let mut labels = vec![
                    primary_term(t1, files).with_message("cannot merge this expression"),
                    primary_term(t2, files).with_message("with this expression"),
                ];

                if let TermPos::Original(span) | TermPos::Inherited(span) = span_opt {
                    labels.push(secondary(span).with_message("merged here"));
                }

                vec![Diagnostic::error()
                    .with_message("non mergeable terms")
                    .with_labels(labels)
                    .with_notes(vec![String::from(
                        "Both values have the same merge priority but they can't be combined",
                    )])]
            }
            EvalError::UnboundIdentifier(ident, span_opt) => vec![Diagnostic::error()
                .with_message("unbound identifier")
                .with_labels(vec![primary_alt(
                    span_opt.into_opt(),
                    ident.to_string(),
                    files,
                )
                .with_message("this identifier is unbound")])],
            EvalError::InfiniteRecursion(_call_stack, span_opt) => {
                let labels = span_opt
                    .as_opt_ref()
                    .map(|span| vec![primary(span).with_message("recursive reference")])
                    .unwrap_or_default();

                vec![Diagnostic::error()
                    .with_message("infinite recursion")
                    .with_labels(labels)]
            }
            EvalError::Other(msg, span_opt) => {
                let labels = span_opt
                    .as_opt_ref()
                    .map(|span| vec![primary(span).with_message("here")])
                    .unwrap_or_default();

                vec![Diagnostic::error().with_message(msg).with_labels(labels)]
            }
            EvalError::InternalError(msg, span_opt) => {
                let labels = span_opt
                    .as_opt_ref()
                    .map(|span| vec![primary(span).with_message("here")])
                    .unwrap_or_default();

                vec![Diagnostic::error()
                    .with_message(format!("internal error: {msg}"))
                    .with_labels(labels)
                    .with_notes(vec![String::from(INTERNAL_ERROR_MSG)])]
            }
            EvalError::SerializationError(err) => err.to_diagnostic(files, contract_id),
            EvalError::DeserializationError(format, msg, span_opt) => {
                let labels = span_opt
                    .as_opt_ref()
                    .map(|span| vec![primary(span).with_message("here")])
                    .unwrap_or_default();

                vec![Diagnostic::error()
                    .with_message(format!("{format} parse error: {msg}"))
                    .with_labels(labels)
                    .with_notes(vec![String::from(INTERNAL_ERROR_MSG)])]
            }
            EvalError::EqError { eq_pos, term: t } => {
                let label = format!(
                    "an argument has type {}, which cannot be compared for equality",
                    t.term
                        .type_of()
                        .unwrap_or_else(|| String::from("<unevaluated>")),
                );

                let labels = match eq_pos {
                    TermPos::Original(pos) | TermPos::Inherited(pos) if eq_pos != &t.pos => {
                        vec![
                            primary(pos).with_message(label),
                            secondary_term(t, files)
                                .with_message("problematic argument evaluated to this"),
                        ]
                    }
                    _ => vec![primary_term(t, files).with_message(label)],
                };

                vec![Diagnostic::error()
                    .with_message("cannot compare values for equality")
                    .with_labels(labels)]
            }
            EvalError::IllegalPolymorphicTailAccess {
                action,
                label: l,
                evaluated_arg,
                call_stack,
            } => {
                let mut msg = String::new();

                write!(&mut msg, "{}", blame_error::title(l)).unwrap();
                write!(&mut msg, " - {}", action.message()).unwrap();
                if !l.tag.is_empty() {
                    write!(&mut msg, ": {}", &escape(&l.tag)).unwrap();
                }

                let (path_label, notes) = blame_error::report_ty_path(l, files);
                let labels = blame_error::build_diagnostic_labels(
                    evaluated_arg.clone(),
                    l,
                    path_label,
                    files,
                    contract_id,
                );

                let mut diagnostics = vec![Diagnostic::error()
                    .with_message(msg)
                    .with_labels(labels)
                    .with_notes(notes)];

                diagnostics.push(blame_error::note(l));

                if ty_path::has_no_dom(&l.path) {
                } else if let Some(id) = contract_id {
                    diagnostics.extend_with_call_stack(id, call_stack);
                }

                diagnostics
            }
        }
    }
}

/// Common functionality for formatting blame errors.
mod blame_error {
    use codespan::{FileId, Files};
    use codespan_reporting::diagnostic::{Diagnostic, Label, LabelStyle};

    use crate::{
        eval::callstack::CallStack,
        label::{
            self,
            ty_path::{self, PathSpan},
        },
        position::TermPos,
        term::RichTerm,
    };

    use super::{primary, secondary, secondary_term};

    /// Returns a title to be used by blame errors based on the `path` and `polarity`
    /// of the label.
    pub fn title(l: &label::Label) -> &str {
        if ty_path::has_no_arrow(&l.path) {
            // An empty path or a path that contains only fields necessarily corresponds to
            // a positive blame
            assert!(l.polarity);
            "contract broken by a value"
        } else if l.polarity {
            "contract broken by a function"
        } else {
            "contract broken by the caller"
        }
    }

    /// Constructs the diagnostic labels used when raising a blame error.
    pub fn build_diagnostic_labels(
        evaluated_arg: Option<RichTerm>,
        blame_label: &label::Label,
        path_label: Label<FileId>,
        files: &mut Files<String>,
        contract_id: Option<FileId>,
    ) -> Vec<Label<FileId>> {
        let mut labels = vec![path_label];

        if let Some(ref arg_pos) = blame_label.arg_pos.into_opt() {
            // In some cases, if the blame error is located in an argument or return value
            // of an higher order functions for example, the original argument position can
            // point to the builtin implementation contract like `func` or `record`, so
            // there's no good reason to show it. Note than even in that case, the
            // information contained at the argument index can still be useful.
            if contract_id
                .map(|ctrs_id| arg_pos.src_id != ctrs_id)
                .unwrap_or(true)
            {
                labels.push(primary(arg_pos).with_message("applied to this expression"));
            }
        }

        // If we have a reference to the element in the cache that was being tested,
        // we can try to show more information about the final, evaluated value that is
        // responsible for the blame.
        if let Some(mut evaluated_arg) = evaluated_arg {
            match (
                evaluated_arg.pos,
                blame_label.arg_pos.as_opt_ref(),
                contract_id,
            ) {
                // Avoid showing a position inside builtin contracts, it's rarely
                // informative.
                (TermPos::Original(val_pos), _, Some(c_id)) if val_pos.src_id == c_id => {
                    evaluated_arg.pos = TermPos::None;
                    labels.push(
                        secondary_term(&evaluated_arg, files)
                            .with_message("evaluated to this value"),
                    );
                }
                // Do not show the same thing twice: if arg_pos and val_pos are the same,
                // the first label "applied to this value" is sufficient.
                (TermPos::Original(ref val_pos), Some(arg_pos), _) if val_pos == arg_pos => {}
                (TermPos::Original(ref val_pos), ..) => {
                    labels.push(secondary(val_pos).with_message("evaluated to this expression"))
                }
                // If the final element is a direct reduct of the original value, rather
                // print the actual value than referring to the same position as
                // before.
                (TermPos::Inherited(ref val_pos), Some(arg_pos), _) if val_pos == arg_pos => {
                    evaluated_arg.pos = TermPos::None;
                    labels.push(
                        secondary_term(&evaluated_arg, files)
                            .with_message("evaluated to this value"),
                    );
                }
                // Finally, if the parameter reduced to a value which originates from a
                // different expression, show both the expression and the value.
                (TermPos::Inherited(ref val_pos), ..) => {
                    labels.push(secondary(val_pos).with_message("evaluated to this expression"));
                    evaluated_arg.pos = TermPos::None;
                    labels.push(
                        secondary_term(&evaluated_arg, files)
                            .with_message("evaluated to this value"),
                    );
                }
                (TermPos::None, ..) => labels.push(
                    secondary_term(&evaluated_arg, files).with_message("evaluated to this value"),
                ),
            }
        }

        labels
    }

    pub trait ExtendWithCallStack {
        fn extend_with_call_stack(&mut self, contract_id: FileId, call_stack: &CallStack);
    }

    impl ExtendWithCallStack for Vec<Diagnostic<FileId>> {
        fn extend_with_call_stack(&mut self, contract_id: FileId, call_stack: &CallStack) {
            let (calls, curr_call) = call_stack.group_by_calls(contract_id);
            let diag_curr_call = curr_call.map(|cdescr| {
                let name = cdescr
                    .head
                    .map(|ident| ident.to_string())
                    .unwrap_or_else(|| String::from("<func>"));
                Diagnostic::note().with_labels(vec![
                    primary(&cdescr.span).with_message(format!("While calling to {name}"))
                ])
            });
            let diags =
                calls.into_iter().enumerate().map(|(i, cdescr)| {
                    let name = cdescr
                        .head
                        .map(|ident| ident.to_string())
                        .unwrap_or_else(|| String::from("<func>"));
                    Diagnostic::note().with_labels(vec![secondary(&cdescr.span)
                        .with_message(format!("({}) calling {}", i + 1, name))])
                });

            self.extend(diag_curr_call);
            self.extend(diags);
        }
    }

    /// Generate a codespan label that describes the [type path][crate::label::ty_path::Path] of a
    /// (Nickel) label, and notes to hint at the situation that may have caused the corresponding
    /// error.
    pub fn report_ty_path(
        l: &label::Label,
        files: &mut Files<String>,
    ) -> (Label<FileId>, Vec<String>) {
        let end_note = String::from("Note: this is an illustrative example. The actual error may involve deeper nested functions calls.");

        let PathSpan {
            start,
            end,
            last,
            last_arrow_elem,
        } = ty_path::span(l.path.iter().peekable(), &l.types);

        let (msg, notes) = match (last, last_arrow_elem) {
            // The type path doesn't contain any arrow, and the failing subcontract is the
            // contract for the elements of an array
            (Some(ty_path::Elem::Array), None) => (String::from("expected array element type"), Vec::new()),
            // The type path doesn't contain any arrow, and the failing subcontract is the contract
            // for the field of a record
            (Some(ty_path::Elem::Field(_)), None) => (String::from("expected field type"), Vec::new()),
            // The original contract contains an arrow, and the path is only composed of codomains.
            // Then polarity is necessarily true and the cause of the blame is the return value of
            // the function
            (Some(_), Some(ty_path::Elem::Codomain)) if ty_path::has_no_dom(&l.path) => {
                (
                    String::from("expected return type"),
                    vec![
                        String::from(
                            "This error may happen in the following situation:
    1. A function `f` is bound by a contract: e.g. `Bool -> Num`.
    2. `f` returns a value of the wrong type: e.g. `f = fun c => \"string\"` while `Num` is expected.",
                        ),
                        String::from(
                            "Either change the contract accordingly, or change the return value of `f`",
                        ),
                    ],
                )
            }
            // The original contract contains an arrow, the subcontract is the domain of an
            // arrow, and the polarity is positive. The function is to be blamed for calling an
            // argument on a value of the wrong type.
            (Some(_), Some(ty_path::Elem::Domain)) if l.polarity => {
                (
                    String::from("expected type of an argument of an inner call"),
                    vec![
                        String::from("This error may happen in the following situation:
    1. A function `f` is bound by a contract: e.g. `(Str -> Str) -> Str)`.
    2. `f` takes another function `g` as an argument: e.g. `f = fun g => g 0`.
    3. `f` calls `g` with an argument that does not respect the contract: e.g. `g 0` while `Str -> Str` is expected."),
                        String::from("Either change the contract accordingly, or call `g` with a `Str` argument."),
                        end_note,
                    ],
                )
            }
            // The original contract contains an arrow, the subcontract is the codomain of an
            // arrow, and the polarity is positive. The function is to be blamed for calling a
            // higher-order function argument on a function which returns a value of the wrong
            // type.
            (Some(_), Some(ty_path::Elem::Codomain)) if l.polarity => {
                (
                    String::from("expected return type of a sub-function passed as an argument of an inner call"),
                    vec![
                        String::from("This error may happen in the following situation:
    1. A function `f` is bound by a contract: e.g. `((Num -> Num) -> Num) -> Num)`.
    2. `f` take another function `g` as an argument: e.g. `f = fun g => g (fun x => true)`.
    3. `g` itself takes a function as an argument.
    4. `f` passes a function that does not respect the contract to `g`: e.g. `g (fun x => true)` (expected to be of type `Num -> Num`)."),
                        String::from("Either change the contract accordingly, or call `g` with a function that returns a value of type `Num`."),
                        end_note,
                    ],
                )
            }
            // The original contract contains an arrow, the subcontract is the domain of an arrow,
            // and the polarity is negative. The caller is to be blamed for providing an argument
            // of the wrong type.
            (Some(_), Some(ty_path::Elem::Domain)) => {
                (
                    String::from("expected type of the argument provided by the caller"),
                    vec![
                        String::from("This error may happen in the following situation:
    1. A function `f` is bound by a contract: e.g. `Num -> Num`.
    2. `f` is called with an argument of the wrong type: e.g. `f false`."),
                        String::from("Either change the contract accordingly, or call `f` with an argument of the right type."),
                        end_note,
                    ],
                )
            }
            // The original contract contains an arrow, the subcontract is the codomain of an
            // arrow, and the polarity is negative. The caller is to be blamed for providing a
            // higher-order function argument which returns a value of the wrong type.
            (Some(_), Some(ty_path::Elem::Codomain)) => {
                (
                    String::from("expected return type of a function provided by the caller"),
                    vec![
                        String::from("This error may happen in the following situation:
    1. A function `f` is bound by a contract: e.g. `(Num -> Num) -> Num`.
    2. `f` takes another function `g` as an argument: e.g. `f = fun g => g 0`.
    3. `f` is called by with an argument `g` that does not respect the contract: e.g. `f (fun x => false)`."),
                        String::from("Either change the contract accordingly, or call `f` with a function that returns a value of the right type."),
                        end_note,
                    ],
                )
            }
            // If there is a last arrow element, then there must be last element
            (None, Some(_)) => panic!("blame error reporting: inconsistent path analysis, last_elem\
is None but last_arrow_elem is Some"),
            _ => (String::from("expected type"), Vec::new()),
        };

        let label = Label::new(
            LabelStyle::Secondary,
            files.add("", format!("{}", l.types)),
            start..end,
        )
        .with_message(msg);
        (label, notes)
    }

    /// Return a note diagnostic showing where a contract was bound.
    pub fn note(l: &label::Label) -> Diagnostic<FileId> {
        Diagnostic::note().with_labels(vec![Label::primary(
            l.span.src_id,
            l.span.start.to_usize()..l.span.end.to_usize(),
        )
        .with_message("bound here")])
    }
}

impl ToDiagnostic<FileId> for ParseError {
    fn to_diagnostic(
        &self,
        files: &mut Files<String>,
        _contract_id: Option<FileId>,
    ) -> Vec<Diagnostic<FileId>> {
        let diagnostic = match self {
            ParseError::UnexpectedEOF(file_id, _expected) => {
                let end = files.source_span(*file_id).end();
                Diagnostic::error()
                    .with_message(format!(
                        "unexpected end of file when parsing {}",
                        files.name(*file_id).to_string_lossy()
                    ))
                    .with_labels(vec![primary(&RawSpan {
                        start: end,
                        end,
                        src_id: *file_id,
                    })])
            }
            ParseError::UnexpectedToken(span, _expected) => Diagnostic::error()
                .with_message("unexpected token")
                .with_labels(vec![primary(span)]),
            ParseError::ExtraToken(span) => Diagnostic::error()
                .with_message("superfluous unexpected token")
                .with_labels(vec![primary(span)]),
            ParseError::UnmatchedCloseBrace(span) => Diagnostic::error()
                .with_message("unmatched closing brace \'}\'")
                .with_labels(vec![primary(span)]),
            ParseError::InvalidEscapeSequence(span) => Diagnostic::error()
                .with_message("invalid escape sequence")
                .with_labels(vec![primary(span)]),
            ParseError::InvalidAsciiEscapeCode(span) => Diagnostic::error()
                .with_message("invalid ascii escape code")
                .with_labels(vec![primary(span)]),
            ParseError::ExternalFormatError(format, msg, span_opt) => {
                let labels = span_opt
                    .as_ref()
                    .map(|span| vec![primary(span)])
                    .unwrap_or_default();

                Diagnostic::error()
                    .with_message(format!("{format} parse error: {msg}"))
                    .with_labels(labels)
            }
            ParseError::UnboundTypeVariables(idents, span) => Diagnostic::error()
                .with_message(format!(
                    "unbound type variable(s): {}",
                    idents
                        .iter()
                        .map(|x| format!("`{x}`"))
                        .collect::<Vec<_>>()
                        .join(",")
                ))
                .with_labels(vec![primary(span)]),
            ParseError::InvalidUniRecord(illegal_span, tail_span, span) => Diagnostic::error()
                .with_message("invalid record literal")
                .with_labels(vec![
                    primary(span),
                    secondary(illegal_span).with_message("can't use this record construct"),
                    secondary(tail_span).with_message("in presence of a tail"),
                ])
                .with_notes(vec![
                    String::from("Using a polymorphic tail in a record `{ ..; a}` requires the rest of the record to be only composed of type annotations, of the form `<field>: <type>`."),
                    String::from("Value assignments, such as `<field> = <expr>`, metadata, etc. are forbidden."),
                ]),
            ParseError::RecursiveLetPattern(span) => Diagnostic::error()
                .with_message("recursive destructuring is not supported")
                .with_labels(vec![
                    primary(span),
                ])
                .with_notes(vec![
                    String::from("A destructuring let-binding can't be recursive. Try removing the `rec` from `let rec`."),
                    String::from("Note: you can reference other fields of a record recursively from within a field, so you might not need the recursive let."),
                ]),
            ParseError::TypeVariableKindMismatch { ty_var, span } => Diagnostic::error()
                .with_message(format!("the type variable {ty_var} is used in conflicting ways"))
                .with_labels(vec![
                    primary(span),
                ])
                .with_notes(vec![
                    String::from("Type variables may be used either as types, polymorphic record tails, or polymorphic enum tails."),
                    String::from("Using the same variable as more than one of these is not permitted.")
                ]),
        };

        vec![diagnostic]
    }
}

impl ToDiagnostic<FileId> for TypecheckError {
    fn to_diagnostic(
        &self,
        files: &mut Files<String>,
        contract_id: Option<FileId>,
    ) -> Vec<Diagnostic<FileId>> {
        fn mk_expr_label(span_opt: &TermPos) -> Vec<Label<FileId>> {
            span_opt
                .as_opt_ref()
                .map(|span| vec![primary(span).with_message("this expression")])
                .unwrap_or_default()
        }

        match self {
            TypecheckError::UnboundIdentifier(ident, pos_opt) =>
            // Use the same diagnostic as `EvalError::UnboundIdentifier` for consistency.
                {
                    EvalError::UnboundIdentifier(*ident, *pos_opt)
                        .to_diagnostic(files, contract_id)
                }
            TypecheckError::MissingRow(ident, expd, actual, span_opt) =>
                vec![Diagnostic::error()
                    .with_message(format!("type error: missing row `{ident}`"))
                    .with_labels(mk_expr_label(span_opt))
                    .with_notes(vec![
                        format!("The type of the expression was expected to be `{expd}` which contains the field `{ident}`"),
                        format!("The type of the expression was inferred to be `{actual}`, which does not contain the field `{ident}`"),
                    ])]
            ,
            TypecheckError::MissingDynTail(expd, actual, span_opt) =>
                vec![Diagnostic::error()
                    .with_message(String::from("type error: missing dynamic tail `| Dyn`"))
                    .with_labels(mk_expr_label(span_opt))
                    .with_notes(vec![
                        format!("The type of the expression was expected to be `{expd}` which contains the tail `| Dyn`"),
                        format!("The type of the expression was inferred to be `{actual}`, which does not contain the tail `| Dyn`"),
                    ])]
            ,

            TypecheckError::ExtraRow(ident, expd, actual, span_opt) =>
                vec![Diagnostic::error()
                    .with_message(format!("type error: extra row `{ident}`"))
                    .with_labels(mk_expr_label(span_opt))
                    .with_notes(vec![
                        format!("The type of the expression was expected to be `{expd}`, which does not contain the field `{ident}`"),
                        format!("The type of the expression was inferred to be `{actual}`, which contains the extra field `{ident}`"),
                    ])]
            ,
            TypecheckError::ExtraDynTail(expd, actual, span_opt) =>
                vec![Diagnostic::error()
                    .with_message(String::from("type error: extra dynamic tail `| Dyn`"))
                    .with_labels(mk_expr_label(span_opt))
                    .with_notes(vec![
                        format!("The type of the expression was expected to be `{expd}`, which does not contain the tail `| Dyn`"),
                        format!("The type of the expression was inferred to be `{actual}`, which contains the extra tail `| Dyn`"),
                    ])]
            ,

            TypecheckError::UnboundTypeVariable(ident, span_opt) =>
                vec![Diagnostic::error()
                    .with_message(String::from("unbound type variable"))
                    .with_labels(vec![primary_alt(span_opt.into_opt(), ident.to_string(), files).with_message("this type variable is unbound")])
                    .with_notes(vec![
                        format!("Maybe you forgot to put a `forall {ident}.` somewhere in the enclosing type ?"),
                    ])]
            ,
            TypecheckError::TypeMismatch(expd, actual, span_opt) => {
                fn addendum(ty: &Types) -> &str {
                    if ty.0.is_flat() {
                        " (a contract)"
                    } else {
                        ""
                    }
                }
                let last_note = if expd.0.is_flat() ^ actual.0.is_flat() {
                    "Static types and contracts are not compatible"
                } else {
                    "These types are not compatible"
                };

                vec![
                    Diagnostic::error()
                        .with_message("incompatible types")
                        .with_labels(mk_expr_label(span_opt))
                        .with_notes(vec![
                            format!("The type of the expression was expected to be `{}`{}", expd, addendum(expd)),
                            format!("The type of the expression was inferred to be `{}`{}", actual, addendum(actual)),
                            String::from(last_note),
                        ])]
            }
            TypecheckError::RowKindMismatch(ident, expd, actual, span_opt) => {
                let (expd_str, actual_str) = match (expd, actual) {
                    (Some(_), None) => ("an enum type", "a record type"),
                    (None, Some(_)) => ("a record type", "an enum type"),
                    _ => panic!("error::to_diagnostic()::RowKindMismatch: unexpected configuration for `expd` and `actual`"),
                };

                vec![
                    Diagnostic::error()
                        .with_message("incompatible row kinds")
                        .with_labels(mk_expr_label(span_opt))
                        .with_notes(vec![
                            format!("The row type of `{ident}` was expected to be `{expd_str}`, but was inferred to be `{actual_str}`"),
                            String::from("Enum row types and record row types are not compatible"),
                        ])]
            }
            TypecheckError::RowMismatch(ident, expd, actual, err_, span_opt) => {
                // If the unification error is on a nested field, we will have a succession of
                // `RowMismatch` errors wrapping the underlying error. In this case, instead of
                // showing a cascade of similar error messages, we determine the full path of the
                // nested field (e.g. `pkg.subpkg1.meta.url`) and only show once the row mismatch
                // error followed by the underlying error.
                let mut err = (*err_).clone();
                let mut path = vec![*ident];

                while let TypecheckError::RowMismatch(id_next, _, _, next, _) = *err {
                    path.push(id_next);
                    err = next;
                }

                let path_str: Vec<String> = path.clone().into_iter().map(|ident| format!("{ident}")).collect();
                let field = path_str.join(".");

                //TODO: we should rather have RowMismatch hold a rows, instead of a general type,
                //than doing this match.
                let row_msg = |word, field, ty| format!("The type of the expression was {word} to have the row `{field}: {ty}`");
                let default_msg = |word, ty| format!("The type of the expression was {word} to be `{ty}`");

                let note1 = if let TypeF::Record(rrows) = &expd.0 {
                    match rrows.row_find_path(path.as_slice()) {
                        Some(ty) => row_msg("expected", &field, ty),
                        None => default_msg("expected", &expd),
                    }
                } else {
                    default_msg("expected", &expd)
                };

                let note2 = if let TypeF::Record(rrows) = &actual.0 {
                    match rrows.row_find_path(path.as_slice()) {
                        Some(ty) => row_msg("inferred", &field, ty),
                        None => default_msg("inferred", &expd),
                    }
                } else {
                    default_msg("inferred", &expd)
                };

                let mut diags = vec![Diagnostic::error()
                    .with_message("incompatible rows declaration")
                    .with_labels(mk_expr_label(span_opt))
                    .with_notes(vec![
                        note1,
                        note2,
                        format!("Could not match the two declaration of `{field}`"),
                    ])
                ];

                // We generate a diagnostic for the underlying error, but append a prefix to the
                // error message to make it clear that this is not a separated error but a more
                // precise description of why the unification of a row failed.
                diags.extend((*err).to_diagnostic(files, contract_id).into_iter()
                    .map(|mut diag| {
                        diag.message = format!("While typing field `{}`: {}", field, diag.message);
                        diag
                    }));
                diags
            }
            TypecheckError::RowConflict(ident, conflict, _expd, _actual, span_opt) => {
                vec![
                    Diagnostic::error()
                        .with_message("multiple rows declaration")
                        .with_labels(mk_expr_label(span_opt))
                        .with_notes(vec![
                            format!("The type of the expression was inferred to have the row `{}: {}`", ident, conflict.as_ref().cloned().unwrap()),
                            format!("But this type appears inside another row type, which already has a declaration for the field `{ident}`"),
                            String::from("A type cannot have two conflicting declaration for the same row"),
                        ])]
            }
            TypecheckError::ArrowTypeMismatch(expd, actual, path, err, span_opt) => {
                let PathSpan {start: expd_start, end: expd_end, ..} = ty_path::span(path.iter().peekable(), expd);
                let PathSpan {start: actual_start, end: actual_end, ..} = ty_path::span(path.iter().peekable(), actual);

                let mut labels = vec![
                    Label::secondary(
                        files.add("", format!("{expd}")),
                        expd_start..expd_end,
                    )
                        .with_message("this part of the expected type"),
                    Label::secondary(
                        files.add("", format!("{actual}")),
                        actual_start..actual_end,
                    )
                        .with_message("does not match this part of the inferred type"),
                ];
                labels.extend(mk_expr_label(span_opt));

                let mut diags = vec![Diagnostic::error()
                    .with_message("function types mismatch")
                    .with_labels(labels)
                    .with_notes(vec![
                        format!("The type of the expression was expected to be `{expd}`"),
                        format!("The type of the expression was inferred to be `{actual}`"),
                        String::from("Could not match the two function types"),
                    ])
                ];

                // We generate a diagnostic for the underlying error, but append a prefix to the
                // error message to make it clear that this is not a separated error but a more
                // precise description of why the unification of the row failed.
                match err.as_ref() {
                    // If the underlying error is a type mismatch, printing won't add any useful
                    // information, so we just ignore it.
                    TypecheckError::TypeMismatch(_, _, _) => (),
                    err => {
                        diags.extend(err.to_diagnostic(files, contract_id).into_iter()
                            .map(|mut diag| {
                                diag.message = format!("While matching function types: {}", diag.message);
                                diag
                            }));
                    }
                }

                diags
            }
            TypecheckError::IncomparableFlatTypes(expd, actual, span_opt) => {
                vec![Diagnostic::error()
                    .with_message("can't compare contract types")
                    .with_labels(mk_expr_label(span_opt))
                    .with_notes(vec![
                        format!("The type of the expression was expected to be `{}`", expd.as_ref().shallow_repr()),
                        format!("The type of the expression was inferred to be `{}`", actual.as_ref().shallow_repr()),
                        String::from("Nickel can't compare contracts during typechecking"),
                    ]),
                    Diagnostic::note()
                    .with_notes(vec![
                        String::from("Due to a temporary limitation, contracts don't mix well with static types (see https://github.com/tweag/nickel/issues/724). This error may happen when using a contract as a type annotation or when calling to a function whose type contain contracts."),
                        String::from("As a temporary fix, please annotate the offending expression with its expected type using the pipe operator `|`. This disables static typing for the given expression."),
                        String::from("For example: if `foo` has type `MyContract -> Num`, rewrite `foo value + 1` as `(foo value | Num) + 1`."),
                    ])
                ]
            }
        }
    }
}

impl ToDiagnostic<FileId> for ImportError {
    fn to_diagnostic(
        &self,
        files: &mut Files<String>,
        contract_id: Option<FileId>,
    ) -> Vec<Diagnostic<FileId>> {
        match self {
            ImportError::IOError(path, error, span_opt) => {
                let labels = span_opt
                    .as_opt_ref()
                    .map(|span| vec![secondary(span).with_message("imported here")])
                    .unwrap_or_default();

                vec![Diagnostic::error()
                    .with_message(format!("import of {path} failed: {error}"))
                    .with_labels(labels)]
            }
            ImportError::ParseErrors(error, span_opt) => {
                let mut diagnostic: Vec<Diagnostic<FileId>> = error
                    .errors
                    .iter()
                    .flat_map(|e| e.to_diagnostic(files, contract_id))
                    .collect();

                if let Some(span) = span_opt.as_opt_ref() {
                    diagnostic[0]
                        .labels
                        .push(secondary(span).with_message("imported here"));
                }

                diagnostic
            }
        }
    }
}

impl ToDiagnostic<FileId> for SerializationError {
    fn to_diagnostic(
        &self,
        files: &mut Files<String>,
        _contract_id: Option<FileId>,
    ) -> Vec<Diagnostic<FileId>> {
        match self {
            SerializationError::NotAString(rt) => vec![Diagnostic::error()
                .with_message(format!(
                    "raw export only supports `Str`, got {}",
                    rt.as_ref()
                        .type_of()
                        .unwrap_or_else(|| String::from("<unevaluated>"))
                ))
                .with_labels(vec![primary_term(rt, files)])],
            SerializationError::UnsupportedNull(format, rt) => vec![Diagnostic::error()
                .with_message(format!("{format} doesn't support null values"))
                .with_labels(vec![primary_term(rt, files)])],
            SerializationError::NonSerializable(rt) => vec![Diagnostic::error()
                .with_message("non serializable term")
                .with_labels(vec![primary_term(rt, files)])],
            SerializationError::Other(msg) => vec![Diagnostic::error()
                .with_message("error during serialization")
                .with_notes(vec![msg.clone()])],
        }
    }
}

impl ToDiagnostic<FileId> for IOError {
    fn to_diagnostic(
        &self,
        _files: &mut Files<String>,
        _contract_id: Option<FileId>,
    ) -> Vec<Diagnostic<FileId>> {
        match self {
            IOError(msg) => vec![Diagnostic::error().with_message(msg.clone())],
        }
    }
}

impl ToDiagnostic<FileId> for ReplError {
    fn to_diagnostic(
        &self,
        _files: &mut Files<String>,
        _contract_id: Option<FileId>,
    ) -> Vec<Diagnostic<FileId>> {
        match self {
            ReplError::UnknownCommand(s) => vec![Diagnostic::error()
                .with_message(format!("unknown command `{s}`"))
                .with_notes(vec![String::from(
                    "type `:?` or `:help` for a list of available commands.",
                )])],
            ReplError::MissingArg { cmd, msg_opt } => {
                let mut notes = msg_opt
                    .as_ref()
                    .map(|msg| vec![msg.clone()])
                    .unwrap_or_default();
                notes.push(format!(
                    "type `:? {cmd}` or `:help {cmd}` for more information."
                ));

                vec![Diagnostic::error()
                    .with_message(format!("{cmd}: missing argument"))
                    .with_notes(notes)]
            }
        }
    }
}
