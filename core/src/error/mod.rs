//! Error types and error reporting.
//!
//! Define error types for different phases of the execution, together with functions to generate a
//! [codespan](https://crates.io/crates/codespan-reporting) diagnostic from them.
pub use codespan::{FileId, Files};
pub use codespan_reporting::diagnostic::{Diagnostic, Label, LabelStyle};

use codespan_reporting::term::termcolor::{ColorChoice, StandardStream, WriteColor};
use lalrpop_util::ErrorRecovery;
use malachite::num::conversion::traits::ToSci;

use crate::{
    cache::Cache,
    eval::callstack::CallStack,
    identifier::LocIdent,
    label::{
        self,
        ty_path::{self, PathSpan},
        MergeKind, MergeLabel,
    },
    parser::{
        self,
        error::{InvalidRecordTypeError, LexicalError, ParseError as InternalParseError},
        lexer::Token,
        utils::mk_span,
    },
    position::{RawSpan, TermPos},
    repl,
    serialize::{ExportFormat, NickelPointer},
    term::{record::FieldMetadata, Number, RichTerm, Term},
    typ::{EnumRow, RecordRow, Type, TypeF, VarKindDiscriminant},
};

pub mod report;
pub mod suggest;

/// A general error occurring during either parsing or evaluation.
#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    EvalError(EvalError),
    TypecheckError(TypecheckError),
    ParseErrors(ParseErrors),
    ImportError(ImportError),
    ExportError(ExportError),
    IOError(IOError),
    ReplError(ReplError),
}

/// An error occurring during evaluation.
#[derive(Debug, Clone, PartialEq)]
pub enum EvalError {
    /// A blame occurred: a contract has been broken somewhere.
    BlameError {
        /// The argument failing the contract. If the argument has been forced by the contract,
        /// `evaluated_arg` provides the final value.
        evaluated_arg: Option<RichTerm>,
        /// The label of the corresponding contract.
        label: label::Label,
        /// The callstack when the blame error was raised.
        call_stack: CallStack,
    },
    /// A field required by a record contract is missing a definition.
    MissingFieldDef {
        id: LocIdent,
        metadata: FieldMetadata,
        pos_record: TermPos,
        pos_access: TermPos,
    },
    /// Mismatch between the expected type and the actual type of an expression.
    TypeError(
        /* expected type */ String,
        /* free form message */ String,
        /* position of the original unevaluated expression */ TermPos,
        /* evaluated expression */ RichTerm,
    ),
    /// `TypeError` when evaluating a unary primop
    UnaryPrimopTypeError {
        primop: String,
        expected: String,
        arg_pos: TermPos,
        arg_evaluated: RichTerm,
    },
    /// `TypeError` when evaluating a binary primop
    NAryPrimopTypeError {
        primop: String,
        expected: String,
        arg_number: usize,
        arg_pos: TermPos,
        arg_evaluated: RichTerm,
    },
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
    FieldMissing {
        // The name of the missing field.
        id: LocIdent,
        // The actual fields of the record used to suggest similar fields.
        field_names: Vec<LocIdent>,
        // The primitive operation that required the field to exist.
        operator: String,
        // The position of the record value which is missing the field.
        pos_record: TermPos,
        // The position of the primitive operation application.
        pos_op: TermPos,
    },
    /// Too few arguments were provided to a builtin function.
    NotEnoughArgs(
        /* required arg count */ usize,
        /* primitive */ String,
        TermPos,
    ),
    /// Attempted to merge incompatible values: for example, tried to merge two distinct default
    /// values into one record field.
    MergeIncompatibleArgs {
        /// The left operand of the merge.
        left_arg: RichTerm,
        /// The right operand of the merge.
        right_arg: RichTerm,
        /// Additional error-reporting data.
        merge_label: MergeLabel,
    },
    /// An unbound identifier was referenced.
    UnboundIdentifier(LocIdent, TermPos),
    /// An element in the evaluation Cache was entered during its own update.
    InfiniteRecursion(CallStack, TermPos),
    /// A serialization error occurred during a call to the builtin `serialize`.
    SerializationError(ExportError),
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
    /// A value didn't match any branch of a `match` expression at runtime. This is a specialized
    /// version of [Self::NonExhaustiveMatch] when all branches are enum patterns. In this case,
    /// the error message is more informative than the generic one.
    NonExhaustiveEnumMatch {
        /// The list of expected patterns. Currently, those are just enum tags.
        expected: Vec<LocIdent>,
        /// The original term matched
        found: RichTerm,
        /// The position of the `match` expression
        pos: TermPos,
    },
    NonExhaustiveMatch {
        /// The original term matched.
        value: RichTerm,
        /// The position of the `match` expression
        pos: TermPos,
    },
    /// Tried to query a field of something that wasn't a record.
    QueryNonRecord {
        /// Position of the original unevaluated expression.
        pos: TermPos,
        /// The identifier that we tried to query.
        id: LocIdent,
        /// Evaluated expression
        value: RichTerm,
    },
    /// An unexpected internal error.
    InternalError(String, TermPos),
    /// Errors occurring rarely enough to not deserve a dedicated variant.
    Other(String, TermPos),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum IllegalPolymorphicTailAction {
    FieldAccess { field: String },
    Map,
    Merge,
    RecordRemove { field: String },
}

impl IllegalPolymorphicTailAction {
    fn message(&self) -> String {
        use IllegalPolymorphicTailAction::*;

        match self {
            FieldAccess { field } => {
                format!("cannot access field `{field}` sealed by a polymorphic contract")
            }
            Map => "cannot map over a record sealed by a polymorphic contract".to_owned(),
            Merge => "cannot merge a record sealed by a polymorphic contract".to_owned(),
            RecordRemove { field } => {
                format!("cannot remove field `{field}` sealed by a polymorphic contract")
            }
        }
    }
}

pub const UNKNOWN_SOURCE_NAME: &str = "<unknown> (generated by evaluation)";

/// An error occurring during the static typechecking phase.
#[derive(Debug, PartialEq, Clone)]
pub enum TypecheckError {
    /// An unbound identifier was referenced.
    UnboundIdentifier { id: LocIdent, pos: TermPos },
    /// A specific row was expected to be in the type of an expression, but was not.
    MissingRow {
        id: LocIdent,
        expected: Type,
        inferred: Type,
        pos: TermPos,
    },
    /// A dynamic tail was expected to be in the type of an expression, but was not.
    MissingDynTail {
        expected: Type,
        inferred: Type,
        pos: TermPos,
    },
    /// A specific row was not expected to be in the type of an expression.
    ExtraRow {
        id: LocIdent,
        expected: Type,
        inferred: Type,
        pos: TermPos,
    },
    /// A additional dynamic tail was not expected to be in the type of an expression.
    ExtraDynTail {
        expected: Type,
        inferred: Type,
        pos: TermPos,
    },
    /// A parametricity violation involving a row-kinded type variable.
    ///
    /// For example, in a function like this:
    ///
    /// ```nickel
    /// let f : forall a. { x: String, y: String } -> { x: String; a } =
    ///   fun r => r
    /// in ...
    /// ```
    ///
    /// this error would be raised with `{ ; a }` as the `tail` type and
    /// `{ y : String }` as the `violating_type`.
    ForallParametricityViolation {
        kind: VarKindDiscriminant,
        tail: Type,
        violating_type: Type,
        pos: TermPos,
    },
    /// An unbound type variable was referenced.
    UnboundTypeVariable(LocIdent),
    /// The actual (inferred or annotated) type of an expression is incompatible with its expected
    /// type.
    TypeMismatch {
        expected: Type,
        inferred: Type,
        pos: TermPos,
    },
    /// The actual (inferred or annotated) record row type of an expression is incompatible with
    /// its expected record row type. Specialized version of [Self::TypeMismatch] with additional
    /// row-specific information.
    RecordRowMismatch {
        id: LocIdent,
        expected: Type,
        inferred: Type,
        cause: Box<TypecheckError>,
        pos: TermPos,
    },
    /// Same as [Self::RecordRowMismatch] but for enum types.
    EnumRowMismatch {
        id: LocIdent,
        expected: Type,
        inferred: Type,
        cause: Option<Box<TypecheckError>>,
        pos: TermPos,
    },
    /// Two incompatible types have been deduced for the same identifier of a row type.
    ///
    /// This is similar to [Self::RecordRowMismatch] but occurs in a slightly different situation.
    /// Consider a unification variable `t`, which is a placeholder to be filled by a concrete type
    /// later in the typechecking phase.  If `t` appears as the tail of a row type, i.e. the type
    /// of some expression is inferred to be `{ field: Type; t}`, then `t` must not be unified
    /// later with a type including a different declaration for field, such as `field: Type2`.
    ///
    /// A [constraint][crate::typecheck::unif::RowConstrs] is added accordingly, and if this
    /// constraint is violated (that is if `t` does end up being unified with a type of the form `{
    /// .., field: Type2, .. }`), [Self::RecordRowConflict] is raised.  We do not necessarily have
    /// access to the original `field: Type` declaration, as opposed to [Self::RecordRowMismatch],
    /// which corresponds to the direct failure to unify `{ .. , x: T1, .. }` and `{ .., x: T2, ..
    /// }`.
    RecordRowConflict {
        /// The row that couldn't be added to the record type, because it already existed with a
        /// different type assignement.
        row: RecordRow,
        expected: Type,
        inferred: Type,
        pos: TermPos,
    },
    /// Same as [Self::RecordRowConflict] but for enum types.
    EnumRowConflict {
        /// The row that couldn't be added to the record type, because it already existed with a
        /// different type assignement.
        row: EnumRow,
        expected: Type,
        inferred: Type,
        pos: TermPos,
    },
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
    /// let id_mono = fun x => x in let _ign = id_mono true in id_mono 0 : Number
    /// ```
    ///
    /// This specific error stores additionally the [type path][crate::label::ty_path] that
    /// identifies the subtype where unification failed and the corresponding error.
    ArrowTypeMismatch {
        expected: Type,
        inferred: Type,
        /// The path to the incompatible type components
        type_path: ty_path::Path,
        cause: Box<TypecheckError>,
        pos: TermPos,
    },
    /// This error should mostly not happen: contracts (flat types) are now properly checked for
    /// equality. This error is raised when flat types are encountered during unification, but flat
    /// types should all have been converted to `typecheck::UnifType::Contract` at this point.
    /// Consider this as an internal, unexpected error.
    IncomparableFlatTypes {
        expected: RichTerm,
        inferred: RichTerm,
        pos: TermPos,
    },
    /// Within statically typed code, the typechecker must reject terms containing nonsensical
    /// contracts such as `let C = { foo : (4 + 1) } in ({ foo = 5 } | C)`, which will fail at
    /// runtime.
    ///
    /// The typechecker is currently quite conservative and simply forbids to store any custom
    /// contract (flat type) in a type that appears in term position. Note that this restriction
    /// doesn't apply to annotations, which aren't considered part of the statically typed block.
    /// For example, `{foo = 5} | {foo : (4 + 1)}` is accepted by the typechecker.
    FlatTypeInTermPosition {
        /// The term that was in a flat type (the `(4 + 1)` in the example above).
        flat: RichTerm,
        /// The position of the entire type (the `{foo : 5}` in the example above).
        pos: TermPos,
    },
    /// Unsound generalization.
    ///
    /// When typechecking polymorphic expressions, polymorphic variables introduced by a `forall`
    /// are substituted with rigid type variables, which can only unify with a free unification
    /// variable. However, the condition that the unification variable is free isn't enough.
    ///
    /// Consider the following example:
    ///
    /// ```nickel
    /// (fun x => let y : forall a. a = x in (y : Number)) : _
    /// ```
    ///
    /// This example must be rejected, as it is an identity function that casts any value to
    /// something of type `Number`. It will typically fail with a contract error if applied to a
    /// string, for example.
    ///
    /// But when `let y : forall a. a = x` is typechecked, `x` is affected to a free unification
    /// variable `_a`, which isn't determined yet. The unsoundess comes from the fact that `_a` was
    /// introduced **before** the block with the `forall a. a` annotation, and thus shouldn't be
    /// allowed to be generalized (unified with a rigid type variable) at this point.
    ///
    /// Nickel uses an algorithm coming from the OCaml implementation, recognizing that the
    /// discipline needed to reject those case is similar to region-based memory management. See
    /// [crate::typecheck] for more details. This error indicates that a case similar to the above
    /// example happened.
    VarLevelMismatch {
        /// The user-defined type variable (the rigid type variable during unification) that
        /// couldn't be unified.
        type_var: LocIdent,
        /// The position of the expression that was being typechecked as `type_var`.
        pos: TermPos,
    },
    /// Invalid or-pattern.
    ///
    /// This error is raised when the patterns composing an or-pattern don't have the precise
    /// same set of free variables. For example, `'Foo x or 'Bar y`.
    OrPatternVarsMismatch {
        /// A variable which isn't present in all the other patterns (there might be more of them,
        /// this is just a sample).
        var: LocIdent,
        /// The position of the whole or-pattern.
        pos: TermPos,
    },
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

impl IntoDiagnostics<FileId> for ParseErrors {
    fn into_diagnostics(
        self,
        files: &mut Files<String>,
        stdlib_ids: Option<&Vec<FileId>>,
    ) -> Vec<Diagnostic<FileId>> {
        self.errors
            .into_iter()
            .flat_map(|e| e.into_diagnostics(files, stdlib_ids))
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
    /// There was an attempt to use a feature that hasn't been enabled.
    DisabledFeature { feature: String, span: RawSpan },
    /// A term was used as a contract in type position, but this term has no chance to make any
    /// sense as a contract. What terms make sense might evolve with time, but any given point in
    /// time, there are a set of expressions that can be excluded syntactically. Currently, it's
    /// mostly constants.
    InvalidContract(RawSpan),
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

#[derive(Debug, PartialEq, Clone)]
pub struct ExportError {
    /// The path to the field that contains a non-serializable value. This might be empty if the
    /// error occurred before entering any record.
    pub path: NickelPointer,
    /// The cause of the error.
    pub data: ExportErrorData,
}

/// The type of error occurring during serialization.
#[derive(Debug, PartialEq, Clone)]
pub enum ExportErrorData {
    /// Encountered a null value for a format that doesn't support them.
    UnsupportedNull(ExportFormat, RichTerm),
    /// Tried exporting something else than a `String` to raw format.
    NotAString(RichTerm),
    /// A term contains constructs that cannot be serialized.
    NonSerializable(RichTerm),
    /// No exportable documentation was found when requested.
    NoDocumentation(RichTerm),
    /// A number was too large (in absolute value) to be serialized as `f64`
    NumberOutOfRange {
        term: RichTerm,
        value: Number,
    },
    Other(String),
}

impl From<ExportErrorData> for ExportError {
    fn from(data: ExportErrorData) -> ExportError {
        ExportError {
            path: NickelPointer::new(),
            data,
        }
    }
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
    InvalidQueryPath(ParseError),
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

impl From<ExportError> for Error {
    fn from(error: ExportError) -> Error {
        Error::ExportError(error)
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

impl From<ExportError> for EvalError {
    fn from(error: ExportError) -> EvalError {
        EvalError::SerializationError(error)
    }
}

/// Return an escaped version of a string. Used to sanitize strings before inclusion in error
/// messages, which can contain ASCII code sequences, and in particular ANSI escape codes, that
/// could alter Nickel's error messages.
pub fn escape(s: &str) -> String {
    String::from_utf8(strip_ansi_escapes::strip(s))
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
            lalrpop_util::ParseError::UnrecognizedEof { expected, .. } => {
                ParseError::UnexpectedEOF(file_id, expected)
            }
            lalrpop_util::ParseError::ExtraToken {
                token: (start, _, end),
            } => ParseError::ExtraToken(mk_span(file_id, start, end)),
            lalrpop_util::ParseError::User { error } => match error {
                InternalParseError::Lexical(LexicalError::Generic(range)) => {
                    ParseError::UnexpectedToken(
                        mk_span(file_id, range.start, range.end),
                        Vec::new(),
                    )
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
                InternalParseError::Lexical(LexicalError::StringDelimiterMismatch {
                    opening_delimiter,
                    closing_delimiter,
                }) => ParseError::StringDelimiterMismatch {
                    opening_delimiter: mk_span(
                        file_id,
                        opening_delimiter.start,
                        opening_delimiter.end,
                    ),
                    closing_delimiter: mk_span(
                        file_id,
                        closing_delimiter.start,
                        closing_delimiter.end,
                    ),
                },
                InternalParseError::UnboundTypeVariables(idents) => {
                    ParseError::UnboundTypeVariables(idents)
                }
                InternalParseError::InvalidRecordType {
                    record_span,
                    tail_span,
                    cause,
                } => ParseError::InvalidRecordType {
                    record_span,
                    tail_span,
                    cause,
                },
                InternalParseError::RecursiveLetPattern(pos) => {
                    ParseError::RecursiveLetPattern(pos)
                }
                InternalParseError::TypeVariableKindMismatch { ty_var, span } => {
                    ParseError::TypeVariableKindMismatch { ty_var, span }
                }
                InternalParseError::TypedFieldWithoutDefinition {
                    field_span,
                    annot_span,
                } => ParseError::TypedFieldWithoutDefinition {
                    field_span,
                    annot_span,
                },
                InternalParseError::DuplicateIdentInRecordPattern { ident, prev_ident } => {
                    ParseError::DuplicateIdentInRecordPattern { ident, prev_ident }
                }
                InternalParseError::DisabledFeature { feature, span } => {
                    ParseError::DisabledFeature { feature, span }
                }
                InternalParseError::InterpolationInStaticPath { path_elem_span } => {
                    ParseError::InterpolationInStaticPath {
                        input: String::new(),
                        path_elem_span,
                    }
                }
                InternalParseError::InvalidContract(span) => ParseError::InvalidContract(span),
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

    pub fn from_toml(error: toml::de::Error, file_id: FileId) -> Self {
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

pub const INTERNAL_ERROR_MSG: &str =
    "This error should not happen. This is likely a bug in the Nickel interpreter. Please consider\
 reporting it at https://github.com/tweag/nickel/issues with the above error message.";

/// A trait for converting an error to a diagnostic.
pub trait IntoDiagnostics<FileId> {
    /// Convert an error to a list of printable formatted diagnostic.
    ///
    /// # Arguments
    ///
    /// - `files`: to know why it takes a mutable reference to `Files<String>`, see
    ///   `label_alt`.
    /// - `stdlib_ids` is required to format the callstack when reporting blame errors. For some
    ///   errors (such as [`ParseError`])), contracts may not have been loaded yet, hence the
    ///   optional. See also [`crate::eval::callstack::CallStack::group_by_calls`].
    ///
    /// # Return
    ///
    /// Return a list of diagnostics. Most errors generate only one, but showing the callstack
    /// ordered requires to sidestep a limitation of codespan. The current solution is to generate
    /// one diagnostic per callstack element. See issue
    /// [#285](https://github.com/brendanzab/codespan/issues/285).
    fn into_diagnostics(
        self,
        files: &mut Files<String>,
        stdlib_ids: Option<&Vec<FileId>>,
    ) -> Vec<Diagnostic<FileId>>;
}

// Allow the use of a single `Diagnostic` directly as an error that can be reported by Nickel.
impl<FileId> IntoDiagnostics<FileId> for Diagnostic<FileId> {
    fn into_diagnostics(
        self,
        _files: &mut Files<String>,
        _stdlib_ids: Option<&Vec<FileId>>,
    ) -> Vec<Diagnostic<FileId>> {
        vec![self]
    }
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
/// When `span_opt` is `None`, the code snippet `alt_term` is added to `files` under a special name
/// and is referred to instead.
///
/// This is useful because during evaluation, some terms are the results of computations. They
/// correspond to nothing in the original source, and thus have a position set to `None`(e.g. the
/// result of `let x = 1 + 1 in x`).  In such cases it may still be valuable to print the term (or a
/// terse representation) in the error diagnostic rather than nothing, because if you have let `x =
/// 1 + 1 in` and then 100 lines later, `x arg` - causing a `NotAFunc` error - it may be helpful to
/// know that `x` holds the value `2`.
///
/// For example, if one wants to report an error on a record, `alt_term` may be defined as
/// `{ ... }`. Then, if this record has no position (`span_opt` is `None`), the error will be
/// reported as:
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
///     - style consistency: the style of the error now depends on the term being from the source or
///     a byproduct of evaluation
/// 3. Add the term to files, take 1: pass a reference to files so that the code building the
///    diagnostic can itself add arbitrary snippets if necessary, and get back their `FileId`. This
///    is what is done here.
/// 4. Add the term to files, take 2: make a wrapper around the `Files` and `FileId` structures of
///    codespan which handle source mapping. `FileId` could be something like
///    `Either<codespan::FileId, CustomId = u32>` so that `to_diagnostic` could construct and use
///    these separate ids, and return the corresponding snippets to be added together with the
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
            Label::new(style, files.add(UNKNOWN_SOURCE_NAME, alt_term), range)
        }
    }
}

/// Create a secondary label from an optional span, or fallback to annotating the alternative
/// snippet `alt_term` if the span is `None`.
///
/// See [`label_alt`].
fn primary_alt(
    span_opt: Option<RawSpan>,
    alt_term: String,
    files: &mut Files<String>,
) -> Label<FileId> {
    label_alt(span_opt, alt_term, LabelStyle::Primary, files)
}

/// Create a primary label from a term, or fallback to annotating the shallow representation of this
/// term if its span is `None`.
///
/// See [`label_alt`].
fn primary_term(term: &RichTerm, files: &mut Files<String>) -> Label<FileId> {
    primary_alt(term.pos.into_opt(), term.to_string(), files)
}

/// Create a secondary label from an optional span, or fallback to annotating the alternative
/// snippet `alt_term` if the span is `None`.
///
/// See [`label_alt`].
fn secondary_alt(span_opt: TermPos, alt_term: String, files: &mut Files<String>) -> Label<FileId> {
    label_alt(span_opt.into_opt(), alt_term, LabelStyle::Secondary, files)
}

/// Create a secondary label from a term, or fallback to annotating the shallow representation of
/// this term if its span is `None`.
///
/// See [`label_alt`].
fn secondary_term(term: &RichTerm, files: &mut Files<String>) -> Label<FileId> {
    secondary_alt(term.pos, term.to_string(), files)
}

fn cardinal(number: usize) -> String {
    let suffix = if number % 10 == 1 {
        "st"
    } else if number % 10 == 2 {
        "nd"
    } else if number % 10 == 3 {
        "rd"
    } else {
        "th"
    };
    format!("{number}{suffix}")
}

impl IntoDiagnostics<FileId> for Error {
    fn into_diagnostics(
        self,
        files: &mut Files<String>,
        stdlib_ids: Option<&Vec<FileId>>,
    ) -> Vec<Diagnostic<FileId>> {
        match self {
            Error::ParseErrors(errs) => errs
                .errors
                .into_iter()
                .flat_map(|e| e.into_diagnostics(files, stdlib_ids))
                .collect(),
            Error::TypecheckError(err) => err.into_diagnostics(files, stdlib_ids),
            Error::EvalError(err) => err.into_diagnostics(files, stdlib_ids),
            Error::ImportError(err) => err.into_diagnostics(files, stdlib_ids),
            Error::ExportError(err) => err.into_diagnostics(files, stdlib_ids),
            Error::IOError(err) => err.into_diagnostics(files, stdlib_ids),
            Error::ReplError(err) => err.into_diagnostics(files, stdlib_ids),
        }
    }
}

impl IntoDiagnostics<FileId> for EvalError {
    fn into_diagnostics(
        self,
        files: &mut Files<String>,
        stdlib_ids: Option<&Vec<FileId>>,
    ) -> Vec<Diagnostic<FileId>> {
        match self {
            EvalError::BlameError {
                evaluated_arg,
                label,
                call_stack,
            } => blame_error::blame_diagnostics(
                files,
                stdlib_ids,
                label,
                evaluated_arg,
                &call_stack,
                "",
            ),
            EvalError::MissingFieldDef {
                id,
                metadata,
                pos_record,
                pos_access,
            } => {
                let mut labels = vec![];

                if let Some(span) = id.pos.into_opt() {
                    labels.push(primary(&span).with_message("required here"));
                }

                if let Some(span) = pos_record.into_opt() {
                    labels.push(secondary(&span).with_message("in this record"));
                }

                if let Some(span) = pos_access.into_opt() {
                    labels.push(secondary(&span).with_message("accessed here"));
                }

                let mut diags = vec![Diagnostic::error()
                    .with_message(format!("missing definition for `{id}`",))
                    .with_labels(labels)];

                // Is it really useful to include the label if we show the position of the ident?
                // We have to see in practice if it can be the case that `id.pos` is
                // `TermPos::None`, but the label is defined.
                if let Some(label) = metadata
                    .annotation
                    .first()
                    .map(|labeled_ty| labeled_ty.label.clone())
                {
                    diags.push(blame_error::contract_bind_loc(&label));
                }

                diags
            }
            EvalError::TypeError(expd, msg, pos_orig, t) => {
                let label = format!(
                    "this expression has type {}, but {} was expected",
                    t.term
                        .type_of()
                        .unwrap_or_else(|| String::from("<unevaluated>")),
                    expd,
                );

                let labels = match (pos_orig.into_opt(), t.pos.into_opt()) {
                    (Some(span_orig), Some(span_t)) if span_orig == span_t => {
                        vec![primary(&span_orig).with_message(label)]
                    }
                    (Some(span_orig), Some(_)) => {
                        vec![
                            primary(&span_orig).with_message(label),
                            secondary_term(&t, files).with_message("evaluated to this"),
                        ]
                    }
                    (Some(span), None) => {
                        vec![primary(&span).with_message(label)]
                    }
                    (None, Some(span)) => {
                        vec![primary(&span).with_message(label)]
                    }
                    (None, None) => {
                        vec![primary_term(&t, files).with_message(label)]
                    }
                };

                vec![Diagnostic::error()
                    .with_message("dynamic type error")
                    .with_labels(labels)
                    .with_notes(vec![msg])]
            }
            EvalError::ParseError(parse_error) => parse_error.into_diagnostics(files, stdlib_ids),
            EvalError::NotAFunc(t, arg, pos_opt) => vec![Diagnostic::error()
                .with_message("not a function")
                .with_labels(vec![
                    primary_term(&t, files)
                        .with_message("this term is applied, but it is not a function"),
                    secondary_alt(pos_opt, format!("({}) ({})", t, arg), files)
                        .with_message("applied here"),
                ])],
            EvalError::FieldMissing {
                id: name,
                field_names,
                operator,
                pos_record,
                pos_op,
            } => {
                let mut labels = Vec::new();
                let mut notes = Vec::new();
                let field = escape(name.as_ref());

                if let Some(span) = pos_op.into_opt() {
                    labels.push(
                        Label::primary(span.src_id, span.start.to_usize()..span.end.to_usize())
                            .with_message(format!("this requires the field `{field}` to exist")),
                    );
                } else {
                    notes.push(format!(
                        "The field `{field}` was required by the operator {operator}"
                    ));
                }

                if let Some(span) = pos_record.as_opt_ref() {
                    labels.push(
                        secondary(span)
                            .with_message(format!("this record lacks the field `{field}`")),
                    );
                }

                suggest::add_suggestion(&mut notes, &field_names, &name);

                vec![Diagnostic::error()
                    .with_message(format!("missing field `{field}`"))
                    .with_labels(labels)
                    .with_notes(notes)]
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
            EvalError::MergeIncompatibleArgs {
                left_arg,
                right_arg,
                merge_label,
            } => {
                let mut labels = vec![
                    primary_term(&left_arg, files).with_message("cannot merge this expression"),
                    primary_term(&right_arg, files).with_message("with this expression"),
                ];

                let span_label = match merge_label.kind {
                    // For a standard merge, the span of the label indicates the position of the
                    // original merge expression
                    MergeKind::Standard => "originally merged here",
                    // For a piecewise definition, there isn't such merge expression (the merge has
                    // been generated by the parser). The spans thus point to the corresponding
                    // field identifier
                    MergeKind::PiecewiseDef => "when combining the definitions of this field",
                };

                labels.push(secondary(&merge_label.span).with_message(span_label));

                fn push_merge_note(notes: &mut Vec<String>, typ: &str) {
                    notes.push(format!(
                        "Both values are of type {typ} but they aren't equal."
                    ));
                    notes.push(format!("{typ} values can only be merged if they are equal"));
                }

                let mut notes = vec![
                    "Merge operands have the same merge priority but they can't \
                    be combined."
                        .to_owned(),
                ];

                if let (Some(left_ty), Some(right_ty)) =
                    (right_arg.as_ref().type_of(), left_arg.as_ref().type_of())
                {
                    match left_ty.as_str() {
                        _ if left_ty != right_ty => {
                            notes.push(format!(
                                "One value is of type {left_ty} \
                                while the other is of type {right_ty}"
                            ));
                            notes.push("Values of different types can't be merged".to_owned());
                        }
                        "String" | "Number" | "Bool" | "Array" | "EnumTag" => {
                            push_merge_note(&mut notes, &left_ty);
                        }
                        "Function" | "MatchExpression" => {
                            notes.push(
                                "Both values are functions (or match expressions)".to_owned(),
                            );
                            notes.push(
                                "Functions can never be merged with anything else, \
                                even another function."
                                    .to_owned(),
                            );
                        }
                        "EnumVariant" => {
                            if let (
                                Term::EnumVariant { tag: tag1, .. },
                                Term::EnumVariant { tag: tag2, .. },
                            ) = (right_arg.as_ref(), left_arg.as_ref())
                            {
                                // The only possible cause of failure of merging two enum variants is a
                                // different tag (the arguments could fail to merge as well, but then
                                // the error would have them as the operands, not the enclosing enums).
                                notes.push(format!(
                                    "Both values are enum variants, \
                                    but their tags differ (`'{tag1}` vs `'{tag2}`)"
                                ));
                                notes.push(
                                    "Enum variants can only be \
                                    merged if they have the same tag"
                                        .to_owned(),
                                );
                            } else {
                                // This should not happen, but it's recoverable, so let's not fail
                                // in release mode.
                                debug_assert!(false);

                                notes.push(
                                    "Primitive values (Number, String, EnumTag and Bool) \
                                    and arrays can only be merged if they are equal"
                                        .to_owned(),
                                );
                                notes.push("Enum variants must have the same tag.".to_owned());
                                notes.push("Functions can never be merged.".to_owned());
                            }
                        }
                        _ => {
                            // In other cases, we print a generic message
                            notes.push(
                                "Primitive values (Number, String, EnumTag and Bool) \
                                    and arrays can only be merged if they are equal"
                                    .to_owned(),
                            );
                            notes.push("Enum variants must have the same tag.".to_owned());
                            notes.push("Functions can never be merged.".to_owned());
                        }
                    }
                }

                vec![Diagnostic::error()
                    .with_message("non mergeable terms")
                    .with_labels(labels)
                    .with_notes(notes)]
            }
            EvalError::UnboundIdentifier(ident, span_opt) => vec![Diagnostic::error()
                .with_message(format!("unbound identifier `{ident}`"))
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
            EvalError::SerializationError(err) => err.into_diagnostics(files, stdlib_ids),
            EvalError::DeserializationError(format, msg, span_opt) => {
                let labels = span_opt
                    .as_opt_ref()
                    .map(|span| vec![primary(span).with_message("here")])
                    .unwrap_or_default();

                vec![Diagnostic::error()
                    .with_message(format!("{format} parse error: {msg}"))
                    .with_labels(labels)]
            }
            EvalError::EqError { eq_pos, term: t } => {
                let label = format!(
                    "an argument has type {}, which cannot be compared for equality",
                    t.term
                        .type_of()
                        .unwrap_or_else(|| String::from("<unevaluated>")),
                );

                let labels = match eq_pos {
                    TermPos::Original(pos) | TermPos::Inherited(pos) if eq_pos != t.pos => {
                        vec![
                            primary(&pos).with_message(label),
                            secondary_term(&t, files)
                                .with_message("problematic argument evaluated to this"),
                        ]
                    }
                    _ => vec![primary_term(&t, files).with_message(label)],
                };

                vec![Diagnostic::error()
                    .with_message("cannot compare values for equality")
                    .with_labels(labels)]
            }
            EvalError::NonExhaustiveEnumMatch {
                expected,
                found,
                pos,
            } => {
                let tag_list = expected
                    .into_iter()
                    .map(|tag| {
                        // We let the pretty printer handle proper formatting
                        RichTerm::from(Term::Enum(tag)).to_string()
                    })
                    .collect::<Vec<_>>()
                    .join(", ");

                let mut labels = Vec::new();

                if let Some(span) = pos.into_opt() {
                    labels.push(primary(&span).with_message("in this match expression"));
                }

                labels.push(
                    secondary_term(&found, files)
                        .with_message("this value doesn't match any branch"),
                );

                vec![Diagnostic::error()
                    .with_message("unmatched pattern")
                    .with_labels(labels)
                    .with_notes(vec![
                        format!("This match expression isn't exhaustive, matching only the following pattern(s): `{tag_list}`"),
                        "But it has been applied to an argument which doesn't match any of those patterns".to_owned(),
                    ])]
            }
            EvalError::NonExhaustiveMatch { value, pos } => {
                let mut labels = Vec::new();

                if let Some(span) = pos.into_opt() {
                    labels.push(primary(&span).with_message("in this match expression"));
                }

                labels.push(
                    secondary_term(&value, files)
                        .with_message("this value doesn't match any branch"),
                );

                vec![Diagnostic::error()
                    .with_message("unmatched pattern")
                    .with_labels(labels)]
            }
            EvalError::IllegalPolymorphicTailAccess {
                action,
                label: contract_label,
                evaluated_arg,
                call_stack,
            } => blame_error::blame_diagnostics(
                files,
                stdlib_ids,
                contract_label,
                evaluated_arg,
                &call_stack,
                &format!(": {}", &action.message()),
            ),
            EvalError::UnaryPrimopTypeError {
                primop,
                ref expected,
                arg_pos,
                arg_evaluated,
            } => EvalError::TypeError(
                expected.clone(),
                format!("{primop} expects its argument to be a {expected}"),
                arg_pos,
                arg_evaluated,
            )
            .into_diagnostics(files, stdlib_ids),
            EvalError::NAryPrimopTypeError {
                primop,
                expected,
                arg_number,
                arg_pos,
                arg_evaluated,
            } => EvalError::TypeError(
                expected.clone(),
                format!(
                    "{primop} expects its {} argument to be a {expected}",
                    cardinal(arg_number)
                ),
                arg_pos,
                arg_evaluated,
            )
            .into_diagnostics(files, stdlib_ids),
            EvalError::QueryNonRecord { pos, id, value } => {
                let label = format!(
                    "tried to query field `{}`, but the expression has type {}",
                    id,
                    value
                        .term
                        .type_of()
                        .unwrap_or_else(|| String::from("<unevaluated>")),
                );

                let label = if let Some(span) = pos.into_opt() {
                    primary(&span).with_message(label)
                } else {
                    primary_term(&value, files).with_message(label)
                };

                vec![Diagnostic::error()
                    .with_message("tried to query field of a non-record")
                    .with_labels(vec![label])]
            }
        }
    }
}

/// Common functionality for formatting blame errors.
mod blame_error {
    use codespan::{FileId, Files};
    use codespan_reporting::diagnostic::{Diagnostic, Label};

    use crate::{
        eval::callstack::CallStack,
        label::{
            self,
            ty_path::{self, PathSpan},
            Polarity,
        },
        position::TermPos,
        term::RichTerm,
        typ::Type,
    };

    use super::{primary, secondary, secondary_term};

    /// Returns a title to be used by blame errors based on the `path` and `polarity`
    /// of the label.
    pub fn title(l: &label::Label) -> String {
        if ty_path::has_no_arrow(&l.path) {
            // An empty path or a path that contains only fields necessarily corresponds to
            // a positive blame
            assert_eq!(l.polarity, Polarity::Positive);
            match l.field_name {
                Some(ident) => format!("contract broken by the value of `{ident}`"),
                None => "contract broken by a value".to_owned(),
            }
        } else if l.polarity == Polarity::Positive {
            match l.field_name {
                Some(ident) => format!("contract broken by the function `{ident}`"),
                None => "contract broken by a function".to_owned(),
            }
        } else {
            match l.field_name {
                Some(ident) => format!("contract broken by the caller of `{ident}`"),
                None => "contract broken by the caller".to_owned(),
            }
        }
    }

    /// Constructs the diagnostic labels used when raising a blame error.
    pub fn build_diagnostic_labels(
        evaluated_arg: Option<RichTerm>,
        blame_label: &label::Label,
        path_label: Label<FileId>,
        files: &mut Files<String>,
        stdlib_ids: Option<&Vec<FileId>>,
    ) -> Vec<Label<FileId>> {
        let mut labels = vec![path_label];

        if let Some(ref arg_pos) = blame_label.arg_pos.into_opt() {
            // In some cases, if the blame error is located in an argument or return value
            // of an higher order functions for example, the original argument position can
            // point to the builtin implementation contract like `func` or `record`, so
            // there's no good reason to show it. Note than even in that case, the
            // information contained at the argument index can still be useful.
            if stdlib_ids
                .as_ref()
                .map(|ctrs_id| !ctrs_id.contains(&arg_pos.src_id))
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
                stdlib_ids,
            ) {
                // Avoid showing a position inside builtin contracts, it's rarely
                // informative.
                (TermPos::Original(val_pos), _, Some(c_id)) if c_id.contains(&val_pos.src_id) => {
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
                (TermPos::Inherited(ref val_pos), _, ids) => {
                    if ids
                        .as_ref()
                        .map(|cids| !cids.contains(&val_pos.src_id))
                        .unwrap_or(true)
                    {
                        labels
                            .push(secondary(val_pos).with_message("evaluated to this expression"));
                    }

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
        fn extend_with_call_stack(&mut self, stdlib_ids: &[FileId], call_stack: &CallStack);
    }

    impl ExtendWithCallStack for Vec<Diagnostic<FileId>> {
        fn extend_with_call_stack(&mut self, stdlib_ids: &[FileId], call_stack: &CallStack) {
            let (calls, curr_call) = call_stack.group_by_calls(stdlib_ids);
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

    /// Calls [`crate::label::ty_path::span`], but if the call returns `None` (the position of the
    /// subtype isn't defined), [path_span] pretty-prints the type inside a new source, parses it,
    /// and calls `ty_path::span`. This new type is guaranteed to have all of its positions set,
    /// providing a definite `PathSpan`. This is similar to the behavior of [`super::primary_alt`].
    pub fn path_span(files: &mut Files<String>, path: &[ty_path::Elem], ty: &Type) -> PathSpan {
        use crate::parser::{grammar::FixedTypeParser, lexer::Lexer, ErrorTolerantParser};

        ty_path::span(path.iter().peekable(), ty)
            .or_else(|| {
                let type_pprinted = format!("{ty}");
                let file_id = files.add(super::UNKNOWN_SOURCE_NAME, type_pprinted.clone());

                let ty_with_pos = FixedTypeParser::new()
                    .parse_strict(file_id, Lexer::new(&type_pprinted))
                    .unwrap();

                ty_path::span(path.iter().peekable(), &ty_with_pos)
            })
            .expect(
                "path_span: we pretty-printed and parsed again the type of a label, \
                so it must have all of its position defined, but `ty_path::span` returned `None`",
            )
    }

    /// Generate a codespan label that describes the [type path][crate::label::ty_path::Path] of a
    /// (Nickel) label.
    pub fn report_ty_path(files: &mut Files<String>, l: &label::Label) -> Label<FileId> {
        let PathSpan {
            span,
            last,
            last_arrow_elem,
        } = path_span(files, &l.path, &l.typ);

        let msg = match (last, last_arrow_elem) {
            // The type path doesn't contain any arrow, and the failing subcontract is the
            // contract for the elements of an array
            (Some(ty_path::Elem::Array), None) => "expected array element type",
            // The type path doesn't contain any arrow, and the failing subcontract is the contract
            // for the fields of a dictionary
            (Some(ty_path::Elem::Dict), None) => "expected dictionary field type",
            // The type path doesn't contain any arrow, and the failing subcontract is the contract
            // for the field of a record
            (Some(ty_path::Elem::Field(_)), None) => "expected field type",
            // The original contract contains an arrow, and the path is only composed of codomains.
            // Then polarity is necessarily true and the cause of the blame is the return value of
            // the function
            (Some(_), Some(ty_path::Elem::Codomain)) if ty_path::has_no_dom(&l.path) => {
                "expected return type"
            }
            // The original contract contains an arrow, the subcontract is the domain of an
            // arrow, and the polarity is positive. The function is to be blamed for calling an
            // argument on a value of the wrong type.
            (Some(_), Some(ty_path::Elem::Domain)) if l.polarity == Polarity::Positive => {
                "expected type of an argument of an inner call"
            }
            // The original contract contains an arrow, the subcontract is the codomain of an
            // arrow, and the polarity is positive. The function is to be blamed for calling a
            // higher-order function argument on a function which returns a value of the wrong
            // type.
            (Some(_), Some(ty_path::Elem::Codomain)) if l.polarity == Polarity::Positive => {
                "expected return type of a sub-function passed as an argument of an inner call"
            }
            // The original contract contains an arrow, the subcontract is the domain of an arrow,
            // and the polarity is negative. The caller is to be blamed for providing an argument
            // of the wrong type.
            (Some(_), Some(ty_path::Elem::Domain)) => {
                "expected type of the argument provided by the caller"
            }
            // The original contract contains an arrow, the subcontract is the codomain of an
            // arrow, and the polarity is negative. The caller is to be blamed for providing a
            // higher-order function argument which returns a value of the wrong type.
            (Some(_), Some(ty_path::Elem::Codomain)) => {
                "expected return type of a function provided by the caller"
            }
            // If there is a last arrow element, then there must be last element
            (None, Some(_)) => panic!(
                "blame error reporting: inconsistent path analysis, last_elem\
is None but last_arrow_elem is Some"
            ),
            _ => "expected type",
        };

        secondary(&span).with_message(msg.to_owned())
    }

    /// Return a note diagnostic showing where a contract was bound.
    pub fn contract_bind_loc(l: &label::Label) -> Diagnostic<FileId> {
        Diagnostic::note().with_labels(vec![Label::primary(
            l.span.src_id,
            l.span.start.to_usize()..l.span.end.to_usize(),
        )
        .with_message("bound here")])
    }

    /// Generate codespan diagnostics from blame data. Mostly used by `into_diagnostics`
    /// implementations.
    ///
    /// # Parameters
    ///
    /// The `msg_addendum` is used to customize the main error message. It's inserted between the
    /// leading "contract broken by .." and the custom contract diagnostic message in tail
    /// position.
    pub fn blame_diagnostics(
        files: &mut Files<String>,
        stdlib_ids: Option<&Vec<FileId>>,
        mut label: label::Label,
        evaluated_arg: Option<RichTerm>,
        call_stack: &CallStack,
        msg_addendum: &str,
    ) -> Vec<Diagnostic<FileId>> {
        use std::fmt::Write;

        let mut diagnostics = Vec::new();

        // Contract diagnostics are stacked up in order, which means the last one is
        // usually the latest/most precise/most relevant. We ignore empty diagnostics and
        // iterate in reverse order, to show the most relevant diagnostics first.
        let mut contract_diagnostics = std::mem::take(&mut label.diagnostics)
            .into_iter()
            .rev()
            .filter(|diag| !label::ContractDiagnostic::is_empty(diag));
        let head_contract_diagnostic = contract_diagnostics.next();

        // The addendum and the custom contract diagnostic are important, so we want to display
        // them as part of the main error message. However, they can make the message quite long.
        // To avoid clutter, we display each component on a new line, indented with respect to the
        // initial "error: "
        let new_msg_block = "\n       ";
        let mut msg = title(&label);

        if !msg_addendum.is_empty() {
            // unwrap(): write shouldn't fail on a String
            write!(&mut msg, "{new_msg_block}{msg_addendum}").unwrap();
        }

        if let Some(contract_msg) = head_contract_diagnostic
            .as_ref()
            .and_then(|diag| diag.message.as_ref())
        {
            // unwrap(): write shouldn't fail on a String
            write!(&mut msg, "{new_msg_block}{}", &super::escape(contract_msg)).unwrap();
        }

        let contract_notes = head_contract_diagnostic
            .map(|diag| diag.notes)
            .unwrap_or_default();
        let path_label = report_ty_path(files, &label);

        let labels = build_diagnostic_labels(evaluated_arg, &label, path_label, files, stdlib_ids);

        // If there are notes in the head contract diagnostic, we build the first
        // diagnostic using them and will put potential generated notes on higher-order
        // contracts in a following diagnostic.
        if !contract_notes.is_empty() {
            diagnostics.push(
                Diagnostic::error()
                    .with_message(msg)
                    .with_labels(labels)
                    .with_notes(contract_notes),
            );
        } else {
            diagnostics.push(Diagnostic::error().with_message(msg).with_labels(labels));
        }

        for ctr_diag in contract_diagnostics {
            let mut msg = String::from("from a parent contract violation");

            if let Some(msg_contract) = ctr_diag.message {
                msg.push_str(": ");
                msg.push_str(&super::escape(&msg_contract));
            }

            diagnostics.push(
                Diagnostic::note()
                    .with_message(msg)
                    .with_notes(ctr_diag.notes),
            );
        }

        match stdlib_ids {
            Some(id) if !ty_path::has_no_dom(&label.path) => {
                diagnostics.extend_with_call_stack(id, call_stack)
            }
            _ => (),
        };

        diagnostics
    }
}

impl IntoDiagnostics<FileId> for ParseError {
    fn into_diagnostics(
        self,
        files: &mut Files<String>,
        _stdlib_ids: Option<&Vec<FileId>>,
    ) -> Vec<Diagnostic<FileId>> {
        let diagnostic = match self {
            ParseError::UnexpectedEOF(file_id, _expected) => {
                let end = files.source_span(file_id).end();
                Diagnostic::error()
                    .with_message(format!(
                        "unexpected end of file when parsing {}",
                        files.name(file_id).to_string_lossy()
                    ))
                    .with_labels(vec![primary(&RawSpan {
                        start: end,
                        end,
                        src_id: file_id,
                    })])
            }
            ParseError::UnexpectedToken(span, _expected) => Diagnostic::error()
                .with_message("unexpected token")
                .with_labels(vec![primary(&span)]),
            ParseError::ExtraToken(span) => Diagnostic::error()
                .with_message("superfluous unexpected token")
                .with_labels(vec![primary(&span)]),
            ParseError::UnmatchedCloseBrace(span) => Diagnostic::error()
                .with_message("unmatched closing brace \'}\'")
                .with_labels(vec![primary(&span)]),
            ParseError::InvalidEscapeSequence(span) => Diagnostic::error()
                .with_message("invalid escape sequence")
                .with_labels(vec![primary(&span)]),
            ParseError::InvalidAsciiEscapeCode(span) => Diagnostic::error()
                .with_message("invalid ascii escape code")
                .with_labels(vec![primary(&span)]),
            ParseError::StringDelimiterMismatch {
                opening_delimiter,
                closing_delimiter,
            } => Diagnostic::error()
                .with_message("string closing delimiter has too many `%`")
                .with_labels(vec![
                    primary(&closing_delimiter).with_message("the closing delimiter"),
                    secondary(&opening_delimiter).with_message("the opening delimiter"),
                ])
                .with_notes(vec![
                    "A special string must be opened and closed with the same number of `%` \
                    in the corresponding delimiters."
                        .into(),
                    "Try removing the superflous `%` in the closing delimiter".into(),
                ]),
            ParseError::ExternalFormatError(format, msg, span_opt) => {
                let labels = span_opt
                    .as_ref()
                    .map(|span| vec![primary(span)])
                    .unwrap_or_default();

                Diagnostic::error()
                    .with_message(format!("{format} parse error: {msg}"))
                    .with_labels(labels)
            }
            ParseError::UnboundTypeVariables(idents) => Diagnostic::error()
                .with_message(format!(
                    "unbound type variable(s): {}",
                    idents
                        .iter()
                        .map(|x| format!("`{x}`"))
                        .collect::<Vec<_>>()
                        .join(",")
                ))
                .with_labels(
                    idents
                        .into_iter()
                        .filter_map(|id| id.pos.into_opt())
                        .map(|span| primary(&span).with_message("this identifier is unbound"))
                        .collect(),
                ),
            ParseError::InvalidRecordType {
                record_span,
                tail_span,
                cause,
            } => {
                let mut labels: Vec<_> = std::iter::once(primary(&record_span))
                    .chain(cause.labels())
                    .collect();
                let mut notes: Vec<_> = std::iter::once(
                    "A record type is a literal composed only of type annotations, of the \
                        form `<field>: <type>`."
                        .into(),
                )
                .chain(cause.notes())
                .collect();

                if let Some(tail_span) = tail_span {
                    labels.push(secondary(&tail_span).with_message("tail"));
                    notes.push(
                        "This literal was interpreted as a record type because it has a \
                        polymorphic tail; record values cannot have tails."
                            .into(),
                    );
                } else {
                    notes.push(
                        "This literal was interpreted as a record type because it has \
                        fields with type annotations but no value definitions; to make \
                        this a record value, assign values to its fields."
                            .into(),
                    );
                };
                Diagnostic::error()
                    .with_message("invalid record literal")
                    .with_labels(labels)
                    .with_notes(notes)
            }
            ParseError::RecursiveLetPattern(span) => Diagnostic::error()
                .with_message("recursive destructuring is not supported")
                .with_labels(vec![primary(&span)])
                .with_notes(vec![
                    "A destructuring let-binding can't be recursive. Try removing the `rec` \
                        from `let rec`."
                        .into(),
                    "You can reference other fields of a record recursively \
                        from within a field, so you might not need the recursive let."
                        .into(),
                ]),
            ParseError::TypeVariableKindMismatch { ty_var, span } => Diagnostic::error()
                .with_message(format!(
                    "the type variable `{ty_var}` is used in conflicting ways"
                ))
                .with_labels(vec![primary(&span)])
                .with_notes(vec![
                    "Type variables may be used either as types, polymorphic record tails, \
                    or polymorphic enum tails."
                        .into(),
                    "Using the same type variable as more than one category at the same time \
                    is forbidden."
                        .into(),
                ]),
            ParseError::TypedFieldWithoutDefinition {
                field_span,
                annot_span,
            } => Diagnostic::error()
                .with_message("statically typed field without a definition")
                .with_labels(vec![
                    primary(&field_span).with_message("this field doesn't have a definition"),
                    secondary(&annot_span).with_message("but it has a type annotation"),
                ])
                .with_notes(vec![
                    "A static type annotation must be attached to an expression but \
                    this field doesn't have a definition."
                        .into(),
                    "Did you mean to use `|` instead of `:`, for example when defining a \
                    record contract?"
                        .into(),
                    "Typed fields without definitions are only allowed inside \
                    record types, but the enclosing record literal doesn't qualify as a \
                    record type. Please refer to the manual for the defining conditions of a \
                    record type."
                        .into(),
                ]),
            ParseError::InterpolationInStaticPath {
                input: _,
                path_elem_span,
            } => Diagnostic::error()
                .with_message("string interpolation is forbidden within a query")
                .with_labels(vec![primary(&path_elem_span)])
                .with_notes(vec![
                    "Field paths don't support string interpolation when querying \
                        metadata."
                        .into(),
                    "Only identifiers and simple string literals are allowed.".into(),
                ]),
            ParseError::DuplicateIdentInRecordPattern { ident, prev_ident } => Diagnostic::error()
                .with_message(format!(
                    "duplicated binding `{}` in record pattern",
                    ident.label()
                ))
                .with_labels(vec![
                    secondary(&prev_ident.pos.unwrap()).with_message("previous binding here"),
                    primary(&ident.pos.unwrap()).with_message("duplicated binding here"),
                ]),
            ParseError::DisabledFeature { feature, span } => Diagnostic::error()
                .with_message("interpreter compiled without required features")
                .with_labels(vec![primary(&span).with_message(format!(
                    "this syntax is only supported with the `{}` feature enabled",
                    feature
                ))])
                .with_notes(vec![format!(
                    "Recompile nickel with `--features {}`",
                    feature
                )]),
            ParseError::InvalidContract(span) => Diagnostic::error()
                .with_message("invalid contract expression")
                .with_labels(vec![primary(&span).with_message("this can't be used as a contract")])
                .with_notes(vec![
                    "This expression is used as a contract as part of an annotation or a type expression."
                        .to_owned(),
                    "Only functions and records might be valid contracts".to_owned(),
                ]),
        };

        vec![diagnostic]
    }
}

impl IntoDiagnostics<FileId> for TypecheckError {
    fn into_diagnostics(
        self,
        files: &mut Files<String>,
        stdlib_ids: Option<&Vec<FileId>>,
    ) -> Vec<Diagnostic<FileId>> {
        fn mk_expr_label(span_opt: &TermPos) -> Vec<Label<FileId>> {
            span_opt
                .as_opt_ref()
                .map(|span| vec![primary(span).with_message("this expression")])
                .unwrap_or_default()
        }

        fn mk_expected_msg<T: std::fmt::Display>(expected: &T) -> String {
            format!("Expected an expression of type `{expected}`")
        }

        fn mk_inferred_msg<T: std::fmt::Display>(inferred: &T) -> String {
            format!("Found an expression of type `{inferred}`")
        }

        match self {
            TypecheckError::UnboundIdentifier { id, pos } =>
            // Use the same diagnostic as `EvalError::UnboundIdentifier` for consistency.
            {
                EvalError::UnboundIdentifier(id, pos).into_diagnostics(files, stdlib_ids)
            }
            TypecheckError::MissingRow {
                id,
                expected,
                inferred,
                pos,
            } => vec![Diagnostic::error()
                .with_message(format!("type error: missing row `{id}`"))
                .with_labels(mk_expr_label(&pos))
                .with_notes(vec![
                    format!(
                        "{}, which contains the field `{id}`",
                        mk_expected_msg(&expected)
                    ),
                    format!(
                        "{}, which does not contain the field `{id}`",
                        mk_inferred_msg(&inferred)
                    ),
                ])],
            TypecheckError::MissingDynTail {
                expected,
                inferred,
                pos,
            } => vec![Diagnostic::error()
                .with_message(String::from("type error: missing dynamic tail `; Dyn`"))
                .with_labels(mk_expr_label(&pos))
                .with_notes(vec![
                    format!(
                        "{}, which contains the tail `; Dyn`",
                        mk_expected_msg(&expected)
                    ),
                    format!(
                        "{}, which does not contain the tail `; Dyn`",
                        mk_inferred_msg(&inferred)
                    ),
                ])],
            TypecheckError::ExtraRow {
                id,
                expected,
                inferred,
                pos,
            } => vec![Diagnostic::error()
                .with_message(format!("type error: extra row `{id}`"))
                .with_labels(mk_expr_label(&pos))
                .with_notes(vec![
                    format!(
                        "{}, which does not contain the field `{id}`",
                        mk_expected_msg(&expected)
                    ),
                    format!(
                        "{}, which contains the extra field `{id}`",
                        mk_inferred_msg(&inferred)
                    ),
                ])],
            TypecheckError::ExtraDynTail {
                expected,
                inferred,
                pos,
            } => vec![Diagnostic::error()
                .with_message(String::from("type error: extra dynamic tail `; Dyn`"))
                .with_labels(mk_expr_label(&pos))
                .with_notes(vec![
                    format!(
                        "{}, which does not contain the tail `; Dyn`",
                        mk_expected_msg(&expected)
                    ),
                    format!(
                        "{}, which contains the extra tail `; Dyn`",
                        mk_inferred_msg(&inferred)
                    ),
                ])],
            TypecheckError::UnboundTypeVariable(ident) => vec![Diagnostic::error()
                .with_message(format!("unbound type variable `{ident}`"))
                .with_labels(vec![primary_alt(
                    ident.pos.into_opt(),
                    ident.to_string(),
                    files,
                )
                .with_message("this type variable is unbound")])
                .with_notes(vec![format!(
                    "Did you forget to put a `forall {ident}.` somewhere in the enclosing type?"
                )])],
            TypecheckError::TypeMismatch {
                expected,
                inferred,
                pos,
            } => {
                fn addendum(ty: &Type) -> &str {
                    if ty.typ.is_flat() {
                        " (a contract)"
                    } else {
                        ""
                    }
                }
                let last_note = if expected.typ.is_flat() ^ inferred.typ.is_flat() {
                    "Static types and contracts are not compatible"
                } else {
                    "These types are not compatible"
                };

                vec![Diagnostic::error()
                    .with_message("incompatible types")
                    .with_labels(mk_expr_label(&pos))
                    .with_notes(vec![
                        format!("{}{}", mk_expected_msg(&expected), addendum(&expected),),
                        format!("{}{}", mk_inferred_msg(&inferred), addendum(&inferred),),
                        String::from(last_note),
                    ])]
            }
            TypecheckError::RecordRowMismatch {
                id,
                expected,
                inferred,
                cause: mut err,
                pos,
            } => {
                // If the unification error is on a nested field, we will have a succession of
                // `RowMismatch` errors wrapping the underlying error. In this case, instead of
                // showing a cascade of similar error messages, we determine the full path of the
                // nested field (e.g. `pkg.subpkg1.meta.url`) and only show once the row mismatch
                // error followed by the underlying error.
                let mut path = vec![id.ident()];

                while let TypecheckError::RecordRowMismatch {
                    id: id_next,
                    cause: next,
                    ..
                } = *err
                {
                    path.push(id_next.ident());
                    err = next;
                }

                let path_str: Vec<String> = path
                    .clone()
                    .into_iter()
                    .map(|ident| format!("{ident}"))
                    .collect();
                let field = path_str.join(".");

                let mk_expected_row_msg = |field, ty| {
                    format!("Expected an expression of a record type with the row `{field}: {ty}`")
                };
                let mk_inferred_row_msg = |field, ty| {
                    format!("Found an expression of a record type with the row `{field}: {ty}`")
                };

                //TODO: we should rather have RowMismatch hold a rows, instead of a general type,
                //than doing this match.
                let note1 = if let TypeF::Record(rrows) = &expected.typ {
                    match rrows.find_path(path.as_slice()) {
                        Some(row) => mk_expected_row_msg(&field, row.typ),
                        None => mk_expected_msg(&expected),
                    }
                } else {
                    mk_expected_msg(&expected)
                };

                let note2 = if let TypeF::Record(rrows) = &inferred.typ {
                    match rrows.find_path(path.as_slice()) {
                        Some(row) => mk_inferred_row_msg(&field, row.typ),
                        None => mk_inferred_msg(&inferred),
                    }
                } else {
                    mk_inferred_msg(&inferred)
                };

                let mut diags = vec![Diagnostic::error()
                    .with_message("incompatible record rows declaration")
                    .with_labels(mk_expr_label(&pos))
                    .with_notes(vec![
                        note1,
                        note2,
                        format!("Could not match the two declarations of `{field}`"),
                    ])];

                // We generate a diagnostic for the underlying error, but append a prefix to the
                // error message to make it clear that this is not a separate error but a more
                // precise description of why the unification of a row failed.
                diags.extend((*err).into_diagnostics(files, stdlib_ids).into_iter().map(
                    |mut diag| {
                        diag.message = format!("while typing field `{}`: {}", field, diag.message);
                        diag
                    },
                ));
                diags
            }
            TypecheckError::EnumRowMismatch {
                id,
                expected,
                inferred,
                cause,
                pos,
            } => {
                let mk_expected_row_msg = |row| {
                    format!("Expected an expression of an enum type with the enum row `{row}`")
                };
                let mk_inferred_row_msg =
                    |row| format!("Found an expression of an enum type with the enum row `{row}`");

                //TODO: we should rather have RowMismatch hold enum rows, instead of a general
                //type, to avoid doing this match.
                let note1 = if let TypeF::Enum(erows) = &expected.typ {
                    if let Some(row) = erows.find_row(id.ident()) {
                        mk_expected_row_msg(row)
                    } else {
                        mk_expected_msg(&expected)
                    }
                } else {
                    mk_expected_msg(&expected)
                };

                let note2 = if let TypeF::Enum(erows) = &inferred.typ {
                    if let Some(row) = erows.find_row(id.ident()) {
                        mk_inferred_row_msg(row)
                    } else {
                        mk_inferred_msg(&expected)
                    }
                } else {
                    mk_inferred_msg(&inferred)
                };

                let mut diags = vec![Diagnostic::error()
                    .with_message("incompatible enum rows declaration")
                    .with_labels(mk_expr_label(&pos))
                    .with_notes(vec![
                        note1,
                        note2,
                        format!("Could not match the two declarations of `{id}`"),
                    ])];

                // We generate a diagnostic for the underlying error if any, but append a prefix to
                // the error message to make it clear that this is not a separate error but a more
                // precise description of why the unification of a row failed.
                if let Some(err) = cause {
                    diags.extend((*err).into_diagnostics(files, stdlib_ids).into_iter().map(
                        |mut diag| {
                            diag.message =
                                format!("while typing enum row `{id}`: {}", diag.message);
                            diag
                        },
                    ));
                }

                diags
            }
            TypecheckError::RecordRowConflict {
                row,
                expected,
                inferred,
                pos,
            } => {
                let mut diags = Vec::new();

                diags.push(
                    Diagnostic::error()
                        .with_message("multiple record row declarations")
                        .with_labels(mk_expr_label(&pos))
                        .with_notes(vec![
                            format!("Found an expression with the row `{row}`"),
                            format!(
                                "But this row appears inside another record type, \
                                which already has a diffent declaration for the field `{}`",
                                row.id
                            ),
                            String::from(
                                "A type cannot have two conflicting declarations for the same row",
                            ),
                        ]),
                );

                diags.push(
                    Diagnostic::note()
                        .with_message("while matching types")
                        .with_notes(vec![
                            format!("Expected type {expected}"),
                            format!("With inferred type {inferred}"),
                        ]),
                );

                diags
            }
            TypecheckError::EnumRowConflict {
                row,
                expected,
                inferred,
                pos,
            } => {
                let mut diags = Vec::new();

                diags.push(
                    Diagnostic::error()
                        .with_message("multiple enum row declarations")
                        .with_labels(mk_expr_label(&pos))
                        .with_notes(vec![
                            format!("Found an expression with the row `{row}`"),
                            format!(
                                "But this row appears inside another enum type, \
                                which already has a diffent declaration for the tag `{}`",
                                row.id
                            ),
                            String::from(
                                "A type cannot have two conflicting declarations for the same row",
                            ),
                        ]),
                );

                diags.push(
                    Diagnostic::note()
                        .with_message("while matching types")
                        .with_notes(vec![
                            format!("Expected type {expected}"),
                            format!("With inferred type {inferred}"),
                        ]),
                );

                diags
            }
            TypecheckError::ArrowTypeMismatch {
                expected,
                inferred,
                type_path,
                cause,
                pos,
            } => {
                let PathSpan {
                    span: expd_span, ..
                } = blame_error::path_span(files, &type_path, &expected);
                let PathSpan {
                    span: actual_span, ..
                } = blame_error::path_span(files, &type_path, &inferred);

                let mut labels = vec![
                    secondary(&expd_span).with_message("this part of the expected type"),
                    secondary(&actual_span)
                        .with_message("does not match this part of the inferred type"),
                ];
                labels.extend(mk_expr_label(&pos));

                let mut diags = vec![Diagnostic::error()
                    .with_message("function types mismatch")
                    .with_labels(labels)
                    .with_notes(vec![
                        mk_expected_msg(&expected),
                        mk_inferred_msg(&inferred),
                        String::from("Could not match the two function types"),
                    ])];

                // We generate a diagnostic for the underlying error, but append a prefix to the
                // error message to make it clear that this is not a separated error but a more
                // precise description of why the unification of the row failed.
                match *cause {
                    // If the underlying error is a type mismatch, printing won't add any useful
                    // information, so we just ignore it.
                    TypecheckError::TypeMismatch { .. } => (),
                    err => {
                        diags.extend(err.into_diagnostics(files, stdlib_ids).into_iter().map(
                            |mut diag| {
                                diag.message =
                                    format!("while matching function types: {}", diag.message);
                                diag
                            },
                        ));
                    }
                }

                diags
            }
            TypecheckError::IncomparableFlatTypes {
                expected,
                inferred,
                pos,
            } => {
                vec![Diagnostic::error()
                    .with_message("internal error: can't compare unconverted flat types")
                    .with_labels(mk_expr_label(&pos))
                    .with_notes(vec![
                        format!("{} (contract)", mk_expected_msg(&expected.to_string()),),
                        format!("{} (contract)", mk_inferred_msg(&inferred.to_string()),),
                        String::from(INTERNAL_ERROR_MSG),
                    ])]
            }
            TypecheckError::ForallParametricityViolation {
                kind,
                tail,
                violating_type,
                pos,
            } => {
                let tail_kind = match kind {
                    VarKindDiscriminant::Type => "type",
                    VarKindDiscriminant::EnumRows => "enum tail",
                    VarKindDiscriminant::RecordRows => "record tail",
                };
                vec![Diagnostic::error()
                    .with_message(format!(
                        "values of type `{violating_type}` are not guaranteed to be compatible \
                        with polymorphic {tail_kind} `{tail}`"
                    ))
                    .with_labels(mk_expr_label(&pos))
                    .with_notes(vec![
                        "Type variables introduced in a `forall` range over all possible types."
                            .to_owned(),
                    ])]
            }
            TypecheckError::FlatTypeInTermPosition { flat, pos } => {
                vec![Diagnostic::error()
                    .with_message(
                        "types containing user-defined contracts cannot be converted into contracts"
                    )
                    .with_labels(
                        pos.into_opt()
                            .map(|span| {
                                primary(&span).with_message("This type (in contract position)")
                            })
                            .into_iter()
                            .chain(flat.pos.into_opt().map(|span| {
                                secondary(&span).with_message("contains this user-defined contract")
                            }))
                            .collect(),
                    )]
            }
            TypecheckError::VarLevelMismatch {
                type_var: constant,
                pos,
            } => {
                let mut labels = mk_expr_label(&pos);

                if let Some(span) = constant.pos.into_opt() {
                    labels.push(secondary(&span).with_message("this polymorphic type variable"));
                }

                vec![Diagnostic::error()
                    .with_message("invalid polymorphic generalization".to_string())
                    .with_labels(labels)
                    .with_notes(vec![
                        "While the type of this expression is still undetermined, it appears \
                            indirectly in the type of another expression introduced before \
                            the `forall` block."
                            .into(),
                        format!(
                            "The type of this expression escapes the scope of the \
                                corresponding `forall` and can't be generalized to the \
                                polymorphic type variable `{constant}`"
                        ),
                    ])]
            }
            TypecheckError::OrPatternVarsMismatch { var, pos } => {
                let mut labels = vec![primary_alt(var.pos.into_opt(), var.into_label(), files)
                    .with_message("this variable must occur in all branches")];

                if let Some(span) = pos.into_opt() {
                    labels.push(secondary(&span).with_message("in this or-pattern"));
                }

                vec![Diagnostic::error()
                    .with_message("or-pattern variable mismatch".to_string())
                    .with_labels(labels)
                    .with_notes(vec![
                        "All branches of an or-pattern must bind exactly the same set of variables"
                            .into(),
                    ])]
            }
        }
    }
}

impl IntoDiagnostics<FileId> for ImportError {
    fn into_diagnostics(
        self,
        files: &mut Files<String>,
        stdlib_ids: Option<&Vec<FileId>>,
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
                    .into_iter()
                    .flat_map(|e| e.into_diagnostics(files, stdlib_ids))
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

impl IntoDiagnostics<FileId> for ExportError {
    fn into_diagnostics(
        self,
        files: &mut Files<String>,
        _stdlib_ids: Option<&Vec<FileId>>,
    ) -> Vec<Diagnostic<FileId>> {
        let mut notes = if !self.path.0.is_empty() {
            vec![format!("When exporting field `{}`", self.path)]
        } else {
            vec![]
        };

        match self.data {
            ExportErrorData::NotAString(rt) => vec![Diagnostic::error()
                .with_message(format!(
                    "raw export expects a String value, but got {}",
                    rt.as_ref()
                        .type_of()
                        .unwrap_or_else(|| String::from("<unevaluated>"))
                ))
                .with_labels(vec![primary_term(&rt, files)])
                .with_notes(notes)],
            ExportErrorData::UnsupportedNull(format, rt) => vec![Diagnostic::error()
                .with_message(format!("{format} format doesn't support null values"))
                .with_labels(vec![primary_term(&rt, files)])
                .with_notes(notes)],
            ExportErrorData::NonSerializable(rt) => {
                notes.extend([
                    "Nickel only supports serializing to and from strings, booleans, numbers, \
                    enum tags, `null` (depending on the format), as well as records and arrays \
                    of serializable values."
                        .into(),
                    "Functions and special values (such as contract labels) aren't \
                    serializable."
                        .into(),
                    "If you want serialization to ignore a specific value, please use the \
                    `not_exported` metadata."
                        .into(),
                ]);

                vec![Diagnostic::error()
                    .with_message("non serializable term")
                    .with_labels(vec![primary_term(&rt, files)])
                    .with_notes(notes)]
            }
            ExportErrorData::NoDocumentation(rt) => {
                notes.push("documentation can only be collected from a record.".to_owned());

                vec![Diagnostic::error()
                    .with_message("no documentation found")
                    .with_labels(vec![primary_term(&rt, files)])
                    .with_notes(notes)]
            }
            ExportErrorData::NumberOutOfRange { term, value } => {
                notes.push(format!(
                    "Only numbers in the range {:e} to {:e} can be portably serialized",
                    f64::MIN,
                    f64::MAX
                ));

                vec![Diagnostic::error()
                    .with_message(format!(
                        "The number {} is too large (in absolute value) to be serialized.",
                        value.to_sci()
                    ))
                    .with_labels(vec![primary_term(&term, files)])
                    .with_notes(notes)]
            }
            ExportErrorData::Other(msg) => {
                notes.push(msg);

                vec![Diagnostic::error()
                    .with_message("serialization failed")
                    .with_notes(notes)]
            }
        }
    }
}

impl IntoDiagnostics<FileId> for IOError {
    fn into_diagnostics(
        self,
        _files: &mut Files<String>,
        _stdlib_ids: Option<&Vec<FileId>>,
    ) -> Vec<Diagnostic<FileId>> {
        match self {
            IOError(msg) => vec![Diagnostic::error().with_message(msg)],
        }
    }
}

impl IntoDiagnostics<FileId> for ReplError {
    fn into_diagnostics(
        self,
        files: &mut Files<String>,
        stdlib_ids: Option<&Vec<FileId>>,
    ) -> Vec<Diagnostic<FileId>> {
        match self {
            ReplError::UnknownCommand(s) => vec![Diagnostic::error()
                .with_message(format!("unknown command `{s}`"))
                .with_notes(vec![String::from(
                    "type `:?` or `:help` for a list of available commands.",
                )])],
            ReplError::InvalidQueryPath(err) => err.into_diagnostics(files, stdlib_ids),
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
