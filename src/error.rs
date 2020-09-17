//! Error types and error reporting.
//!
//! Define error types for different phases of the execution, together with functions to generate a
//! [codespan](https://crates.io/crates/codespan-reporting) diagnostic from them.
use crate::eval::{CallStack, StackElem};
use crate::identifier::Ident;
use crate::label;
use crate::label::ty_path;
use crate::parser::lexer::LexicalError;
use crate::parser::utils::mk_span;
use crate::position::RawSpan;
use crate::term::RichTerm;
use crate::types::Types;
use codespan::{FileId, Files};
use codespan_reporting::diagnostic::{Diagnostic, Label, LabelStyle};
use std::fmt::Write;

/// A general error occurring during either parsing or evaluation.
#[derive(Debug, PartialEq)]
pub enum Error {
    EvalError(EvalError),
    TypecheckError(TypecheckError),
    ParseError(ParseError),
    ImportError(ImportError),
}

/// An error occurring during evaluation.
#[derive(Debug, PartialEq)]
pub enum EvalError {
    /// A blame occurred: a contract have been broken somewhere.
    BlameError(label::Label, Option<CallStack>),
    /// Mismatch between the expected type and the actual type of an expression.
    TypeError(
        /* expected type */ String,
        /* operation */ String,
        /* position of the original unevaluated expression */ Option<RawSpan>,
        /* evaluated expression */ RichTerm,
    ),
    /// A term which is not a function has been applied to an argument.
    NotAFunc(
        /* term */ RichTerm,
        /* arg */ RichTerm,
        /* app position */ Option<RawSpan>,
    ),
    /// A field access, or another record operation requiring the existence of a specific field,
    /// has been performed on a record missing that field.
    FieldMissing(
        /* field identifier */ String,
        /* operator */ String,
        RichTerm,
        Option<RawSpan>,
    ),
    /// Too few arguments were provided to a builtin function.
    NotEnoughArgs(
        /* required arg count */ usize,
        /* primitive */ String,
        Option<RawSpan>,
    ),
    /// Attempted to merge incompatible values: for example, tried to merge two distinct default
    /// values into one record field.
    MergeIncompatibleArgs(
        /* left operand */ RichTerm,
        /* right operand */ RichTerm,
        /* original merge */ Option<RawSpan>,
    ),
    /// An unbound identifier was referenced.
    UnboundIdentifier(Ident, Option<RawSpan>),
    /// An unexpected internal error.
    InternalError(String, Option<RawSpan>),
    /// Errors occurring rarely enough to not deserve a dedicated variant.
    Other(String, Option<RawSpan>),
}

/// An error occurring during the static typechecking phase.
#[derive(Debug, PartialEq)]
pub enum TypecheckError {
    /// An unbound identifier was referenced.
    UnboundIdentifier(Ident, Option<RawSpan>),
    /// An ill-formed type, such as a non-row type appearing in a row.
    IllformedType(Types),
    /// A specific row was expected to be in the type of an expression, but was not.
    MissingRow(
        Ident,
        /* the expected type */ Types,
        /* the inferred/annotated type */ Types,
        Option<RawSpan>,
    ),
    /// An unbound type variable was referenced.
    UnboundTypeVariable(Ident, Option<RawSpan>),
    /// The actual (inferred or annotated) type of an expression is incompatible with its expected
    /// type.
    TypeMismatch(
        /* the expected type */ Types,
        /* the inferred or annotated type */ Types,
        Option<RawSpan>,
    ),
    /// Two incompatible types have been deduced for the same identifier of a row type.
    RowMismatch(
        Ident,
        /* expected */ Option<Types>,
        /* actual/inferred/annotated */ Option<Types>,
        Option<RawSpan>,
    ),
    /// Two incompatible types have been deduced for the same identifier of a row type.
    ///
    /// This is similar to `RowMismatch` but occurs in a slightly different situation. Consider a a
    /// unification variable `t`, which is a placeholder to be filled by a concrete type later in
    /// the typechecking phase.  If `t` appears as the tail of a row type, i.e. the type of some
    /// expression is inferred to be `{| field: Type | t}`, then `t` must not be unified later with
    /// a type including a different declaration for field, such as `field: Type2`.
    ///
    /// A [constraint](../typecheck/type.RowConstr.html) is added accordingly, and if this
    /// constraint is violated (that is if `t` does end up being unified with a type of the form
    /// `{| .., field: Type2, .. }`), `RowConflict` is raised.  We do not have access to the
    /// original `field: Type` declaration, as opposed to `RowMismatch`, which corresponds to the
    /// direct failure to unify `{| .. , x: T1, .. }` and `{| .., x: T2, .. }`.
    RowConflict(
        Ident,
        /* the second type assignment which violates the constraint */ Option<Types>,
        /* the expected type of the subexpression */ Types,
        /* the actual type of the subexpression */ Types,
        Option<RawSpan>,
    ),
}

/// An error occurring during parsing.
#[derive(Debug, PartialEq)]
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
    /// An alphanumeric character directly follows a number literal.
    NumThenIdent(RawSpan),
    /// Invalid escape sequence in a string literal.
    InvalidEscapeSequence(RawSpan),
}

/// An error occuring during the resolution of an import.
#[derive(Debug, PartialEq)]
pub enum ImportError {
    /// An IO error occurred during an import.
    IOError(
        /* imported file */ String,
        /* error message */ String,
        /* import position */ Option<RawSpan>,
    ),
    /// A parse error occured during an import.
    ParseError(
        /* error */ ParseError,
        /* import position */ Option<RawSpan>,
    ),
}

impl From<EvalError> for Error {
    fn from(error: EvalError) -> Error {
        Error::EvalError(error)
    }
}

impl From<ParseError> for Error {
    fn from(error: ParseError) -> Error {
        Error::ParseError(error)
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

impl ParseError {
    pub fn from_lalrpop<T>(
        error: lalrpop_util::ParseError<usize, T, LexicalError>,
        file_id: FileId,
    ) -> ParseError {
        match error {
            lalrpop_util::ParseError::InvalidToken { location } => {
                ParseError::UnexpectedToken(mk_span(file_id, location, location + 1), Vec::new())
            }
            lalrpop_util::ParseError::UnrecognizedToken {
                token: Some((start, _, end)),
                expected,
            } => ParseError::UnexpectedToken(mk_span(file_id, start, end), expected),
            lalrpop_util::ParseError::UnrecognizedToken {
                token: None,
                expected,
            }
            | lalrpop_util::ParseError::User {
                error: LexicalError::UnexpectedEOF(expected),
            } => ParseError::UnexpectedEOF(file_id, expected),
            lalrpop_util::ParseError::ExtraToken {
                token: (start, _, end),
            } => ParseError::ExtraToken(mk_span(file_id, start, end)),
            lalrpop_util::ParseError::User {
                error: LexicalError::UnmatchedCloseBrace(location),
            } => ParseError::UnmatchedCloseBrace(mk_span(file_id, location, location + 1)),
            lalrpop_util::ParseError::User {
                error: LexicalError::UnexpectedChar(location),
            } => ParseError::UnexpectedToken(mk_span(file_id, location, location + 1), Vec::new()),
            lalrpop_util::ParseError::User {
                error: LexicalError::NumThenIdent(location),
            } => ParseError::NumThenIdent(mk_span(file_id, location, location + 1)),
            lalrpop_util::ParseError::User {
                error: LexicalError::InvalidEscapeSequence(location),
            } => ParseError::InvalidEscapeSequence(mk_span(file_id, location, location + 1)),
        }
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
    ///   [`label_alt`](fn.label_alt.html).
    /// - `contract_id` is required to format the callstack when reporting blame errors. For some
    ///   errors (such as [`ParseError`](./enum.ParseError.html)), contracts may not have been loaded
    ///   yet, hence the optional. See also [`process_callstack`](fn.process_callstack.html).
    ///
    /// # Return
    ///
    /// Return a list of diagnostics. Most errors generate only one, but showing the callstack
    /// ordered requires to sidestep a limitation of codespan. The current solution is to generate
    /// one diagnostic per callstack element. See [this
    /// issue](https://github.com/brendanzab/codespan/issues/285).
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
/// = 1 + 1 in` and then 100 lines later, `x arg` - causing an `NotAFunc` error - it may be helpful
/// to know that `x` holds the value `2`.
///
/// For example, if one wants to report an error on a record, `alt_term` may be defined to `{ ...  }`.
/// Then, if this record has no position (`span_opt` is `None`), the error will be reported as:
///
/// ```
/// error: some error
///   -- <unkown> (generated by evaluation):1:2
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
    span_opt: &Option<RawSpan>,
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
                files.add("<unkown> (generated by evaluation)", alt_term),
                range,
            )
        }
    }
}

/// Create a secondary label from an optional span, or fallback to annotating the alternative snippet
/// `alt_term` if the span is `None`.
///
/// See [`label_alt`](fn.label_alt.html).
fn primary_alt(
    span_opt: &Option<RawSpan>,
    alt_term: String,
    files: &mut Files<String>,
) -> Label<FileId> {
    label_alt(span_opt, alt_term, LabelStyle::Primary, files)
}

/// Create a primary label from a term, or fallback to annotating the shallow representation of this term
/// if its span is `None`.
///
/// See [`label_alt`](fn.label_alt.html).
fn primary_term(term: &RichTerm, files: &mut Files<String>) -> Label<FileId> {
    primary_alt(&term.pos, term.as_ref().shallow_repr(), files)
}

/// Create a secondary label from an optional span, or fallback to annotating the alternative snippet
/// `alt_term` if the span is `None`.
///
/// See [`label_alt`](fn.label_alt.html).
fn secondary_alt(
    span_opt: &Option<RawSpan>,
    alt_term: String,
    files: &mut Files<String>,
) -> Label<FileId> {
    label_alt(span_opt, alt_term, LabelStyle::Secondary, files)
}

/// Create a secondary label from a term, or fallback to annotating the shallow representation of this term
/// if its span is `None`.
///
/// See [`label_alt`](fn.label_alt.html).
fn secondary_term(term: &RichTerm, files: &mut Files<String>) -> Label<FileId> {
    secondary_alt(&term.pos, term.as_ref().shallow_repr(), files)
}

/// Generate a codespan label that describes the [type path](../label/enum.TyPath.html) of a
/// (Nickel) label, and notes to hint at the situation that may have caused the corresponding
/// error.
fn report_ty_path(l: &label::Label, files: &mut Files<String>) -> (Label<FileId>, Vec<String>) {
    let end_note = String::from("Note: this is an illustrative example. The actual error may involve deeper nested functions calls.");

    let (msg, notes) = if l.path.is_empty() {
        (String::from("expected type"), Vec::new())
    }
    // If the path is only composed of codomains, polarity is necessarily true and the cause of the
    // blame is the return value of the function
    else if ty_path::is_only_codom(&l.path) {
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
    } else {
        match l.path.last().unwrap() {
                ty_path::Elem::Domain if l.polarity => {
                    (String::from("expected type of an argument of an inner call"),
                    vec![
                        String::from("This error may happen in the following situation:
1. A function `f` is bound by a contract: e.g. `(Str -> Str) -> Str)`.
2. `f` takes another function `g` as an argument: e.g. `f = fun g => g 0`.
3. `f` calls `g` with an argument that does not respect the contract: e.g. `g 0` while `Str -> Str` is expected."),
                        String::from("Either change the contract accordingly, or call `g` with a `Str` argument."),
                        end_note,
                    ])
                }
                ty_path::Elem::Codomain if l.polarity => {
                    (String::from("expected return type of a sub-function passed as an argument of an inner call"),
                    vec![
                        String::from("This error may happen in the following situation:
1. A function `f` is bound by a contract: e.g. `((Num -> Num) -> Num) -> Num)`.
2. `f` take another function `g` as an argument: e.g. `f = fun g => g (fun x => true)`.
3. `g` itself takes a function as an argument.
4. `f` passes a function that does not respect the contract to `g`: e.g. `g (fun x => true)` (expected to be of type `Num -> Num`)."),
                        String::from("Either change the contract accordingly, or call `g` with a function that returns a value of type `Num`."),
                        end_note,
                    ])
                }
                ty_path::Elem::Domain => {
                    (String::from("expected type of the argument provided by the caller"),
                    vec![
                        String::from("This error may happen in the following situation:
1. A function `f` is bound by a contract: e.g. `Num -> Num`.
2. `f` is called with an argument of the wrong type: e.g. `f false`."),
                        String::from("Either change the contract accordingly, or call `f` with an argument of the right type."),
                        end_note,
                    ])
                }
                ty_path::Elem::Codomain => {
                    (String::from("expected return type of a function provided by the caller"),
                    vec![
                        String::from("This error may happen in the following situation:
1. A function `f` is bound by a contract: e.g. `(Num -> Num) -> Num`.
2. `f` takes another function `g` as an argument: e.g. `f = fun g => g 0`.
3. `f` is called by with an argument `g` that does not respect the contract: e.g. `f (fun x => false)`."),
                        String::from("Either change the contract accordingly, or call `f` with a function that returns a value of the right type."),
                        end_note,
                    ])
                }
            }
    };

    let (start, end) = ty_path::span(l.path.iter().peekable(), &l.types);
    let label = Label::new(
        LabelStyle::Secondary,
        files.add("", format!("{}", l.types)),
        start..end,
    )
    .with_message(msg);
    (label, notes)
}

/// Process a raw callstack by grouping elements belonging to the same call and getting rid of
/// elements that are not associated to a call.
///
/// Recall that when a call `f arg` is evaluated, the following events happen:
/// 1. `arg` is pushed on the evaluation stack.
/// 2. `f` is evaluated.
/// 3. Hopefully, the result of this evaluation is a function `Func(id, body)`. `arg` is popped
///    from the stack, bound to `id` in the environment, and `body is entered`.
///
/// For error reporting purpose, we want to be able to determine the chain of nested calls leading
/// to the current code path at any moment. To do so, the Nickel abstract machine maintains a
/// callstack via this basic mechanism:
/// 1. When a function body is entered, push the position of the original application on the
///    callstack.
/// 2. When a variable is evaluated, put its name and position on the callstack. The goal is to
///    have an easy access to the name of a called function for calls of the form `f arg1
///    .. argn`.
///
/// The resulting stack is not suited to be reported to the user for the following reasons:
///
/// 1. Some variable evaluations do not correspond to a call. The typical example is `let x = exp
///    in x`, which pushes an orphan `x` on the callstack. We want to get rid of these.
/// 2. Because of currying, multiple arguments applications span several objects on the callstack.
///    Typically, `(fun x y => x + y) arg1 arg2` spans two `App` elements, where the position span
///    of the latter includes the position span of the former. We want to group them as one call.
/// 3. The callstack includes calls to builtin contracts. These calls are inserted implicitely by
///    the abstract machine and are not written explicitly by the user. Showing them is confusing
///    and clutters the call chain, so we get rid of them too.
///
/// This is the role of `process_callstack`, which filters out unwanted elements and groups
/// callstack elements into atomic call elements represented as `(Option<Ident>, RawSpan)` tuples:
///
///  - The first element is the name of the function called, if there is any (anonymous functions don't have one).
///  - The second is the position span of the whole application.
///
/// The callstack is also reversed such that the most nested calls, which are usually the most
/// relevant to understand the error, are printed first.
///
/// # Arguments
///
/// - `cs`: the raw callstack to process.
/// - `contract_id`: the `FileId` of the source containing standard contracts, to filter their
///   calls out.
pub fn process_callstack(cs: &CallStack, contract_id: FileId) -> Vec<(Option<Ident>, RawSpan)> {
    // Create a call element from a callstack element.
    fn from_elem(elem: &StackElem) -> (Option<Ident>, RawSpan) {
        match elem {
            StackElem::Var(_, id, Some(pos)) => (Some(id.clone()), pos.clone()),
            StackElem::App(Some(pos)) => (None, pos.clone()),
            _ => panic!(),
        }
    }

    let it = cs.iter().filter(|elem| match elem {
        StackElem::Var(_, _, Some(RawSpan { src_id, .. }))
        | StackElem::App(Some(RawSpan { src_id, .. }))
            if *src_id != contract_id =>
        {
            true
        }
        _ => false,
    });

    // To decide how to fuse calls, we need to see two successive elements of the callstack at each
    // iteration. To do so, we create a zipper of the original iterator with a copy of the iterator
    // shifted by one element, which returns options to be able to iter until the very last element
    // (it is padded with an ending `None`).
    let shifted = it.clone().skip(1).map(|elem| Some(elem)).chain(Some(None));
    let mut it = it.peekable();

    // The call element being currently built.
    let mut pending = if let Some(first) = it.peek() {
        from_elem(first)
    } else {
        return Vec::new();
    };

    let mut acc = Vec::new();

    for (prev, next) in it.zip(shifted) {
        match (prev, next) {
            // If a `Var` is immediately followed by another `Var`, it does not correspond to a
            // call: we just drop `pending` and replace it with a fresh call element. This
            // corresponds to the case 1 mentioned in the description of this function.
            (StackElem::Var(_, _, _), Some(StackElem::Var(_, id, Some(pos)))) => {
                pending = (Some(id.clone()), pos.clone())
            }
            // If an `App` is followed by a `Var`, then `Var` necessarily belongs to a different
            // call (or to no call at all): we push the pending call and replace it with a fresh
            // call element.
            (StackElem::App(Some(_)), Some(StackElem::Var(_, id, Some(pos)))) => {
                let old = std::mem::replace(&mut pending, (Some(id.clone()), pos.clone()));
                acc.push(old);
            }
            // If a `Var` is followed by an `App`, they belong to the same call iff the position
            // span of the `Var` is included in the position span of the following `App`. In this
            // case, we fuse them. Otherwise, `Var` again does not correspond to any call, and we
            // just drop the old `pending`.
            (StackElem::Var(_, _, _), Some(StackElem::App(Some(pos_app)))) => {
                let id_opt = match &pending {
                    (id_opt, pos) if *pos <= *pos_app => id_opt.as_ref().cloned(),
                    _ => None,
                };
                pending = (id_opt, pos_app.clone());
            }
            // Same thing with two `App`: we test for the containment of position spans. If this
            // fails, however, we still push `pending`, since as opposed to `Var`, an `App` always
            // corresponds to an actual call.
            (StackElem::App(_), Some(StackElem::App(Some(pos_app)))) => {
                let (contained, id_opt) = match &pending {
                    (id_opt, pos) if *pos <= *pos_app => (true, id_opt.as_ref().cloned()),
                    _ => (false, None),
                };
                if contained {
                    pending = (id_opt, pos_app.clone());
                } else {
                    let old = std::mem::replace(&mut pending, (None, pos_app.clone()));
                    acc.push(old);
                };
            }
            // We save the last `pending` element at the end of the iterator if the last stack
            // element was an `App`.
            (StackElem::App(_), None) => {
                acc.push(pending);
                // The `break` is useless at first sight, but if omit it the compiler complains
                // that we will use a moved `pending`
                break;
            }
            // Otherwise it is again an orphan `Var` that can be ignored.
            (StackElem::Var(_, _, _), None) => (),
            // We should have tested all possible legal configurations of a callstack.
            _ => panic!(
                "error::process_callstack(): unexpected consecutive elements of the\
callstack ({:?}, {:?})",
                prev, next
            ),
        };
    }

    acc.reverse();
    acc
}

impl ToDiagnostic<FileId> for Error {
    fn to_diagnostic(
        &self,
        files: &mut Files<String>,
        contract_id: Option<FileId>,
    ) -> Vec<Diagnostic<FileId>> {
        match self {
            Error::ParseError(err) => err.to_diagnostic(files, contract_id),
            Error::TypecheckError(err) => err.to_diagnostic(files, contract_id),
            Error::EvalError(err) => err.to_diagnostic(files, contract_id),
            Error::ImportError(err) => err.to_diagnostic(files, contract_id),
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
            EvalError::BlameError(l, cs_opt) => {
                let mut msg = String::from("Blame error: ");

                // Writing in a string should not raise an error, whence the fearless `unwrap()`
                if l.path.is_empty() {
                    // An empty path necessarily corresponds to a positive blame
                    assert!(l.polarity);
                    write!(&mut msg, "contract broken by a value").unwrap();
                } else {
                    if l.polarity {
                        write!(&mut msg, "contract broken by a function").unwrap();
                    } else {
                        write!(&mut msg, "contract broken by the caller").unwrap();
                    }
                }

                if !l.tag.is_empty() {
                    write!(&mut msg, " [{}].", l.tag).unwrap();
                } else {
                    write!(&mut msg, ".").unwrap();
                }

                let (path_label, notes) = report_ty_path(&l, files);
                let labels = vec![
                    path_label,
                    Label::primary(
                        l.span.src_id,
                        l.span.start.to_usize()..l.span.end.to_usize(),
                    )
                    .with_message("bound here"),
                ];

                let mut diagnostics = vec![Diagnostic::error()
                    .with_message(msg)
                    .with_labels(labels)
                    .with_notes(notes)];

                match contract_id {
                    Some(id) if !ty_path::is_only_codom(&l.path) => {
                        let diags_opt =
                            cs_opt
                                .as_ref()
                                .map(|cs| process_callstack(cs, id))
                                .map(|calls| {
                                    calls.into_iter().enumerate().map(|(i, (id_opt, pos))| {
                                        let name = id_opt
                                            .map(|Ident(id)| id.clone())
                                            .unwrap_or(String::from("<func>"));
                                        Diagnostic::note().with_labels(vec![secondary(&pos)
                                            .with_message(format!("({}) calling {}", i + 1, name))])
                                    })
                                });

                        if let Some(diags) = diags_opt {
                            diagnostics.extend(diags);
                        }
                    }
                    _ => (),
                }

                diagnostics
            }
            EvalError::TypeError(expd, msg, orig_pos_opt, t) => {
                let label = format!(
                    "This expression has type {}, but {} was expected",
                    t.term.type_of().unwrap_or(String::from("<unevaluated>")),
                    expd,
                );

                let labels = match orig_pos_opt {
                    Some(pos) if orig_pos_opt != &t.pos => vec![
                        primary(pos).with_message(label),
                        secondary_term(&t, files).with_message("evaluated to this"),
                    ],
                    _ => vec![primary_term(&t, files).with_message(label)],
                };

                vec![Diagnostic::error()
                    .with_message("Type error")
                    .with_labels(labels)
                    .with_notes(vec![msg.clone()])]
            }
            EvalError::NotAFunc(t, arg, pos_opt) => vec![Diagnostic::error()
                .with_message("Not a function")
                .with_labels(vec![
                    primary_term(&t, files)
                        .with_message("this term is applied, but it is not a function"),
                    secondary_alt(
                        &pos_opt,
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

                if let Some(span) = span_opt {
                    labels.push(
                        Label::primary(span.src_id, span.start.to_usize()..span.end.to_usize())
                            .with_message(format!("this requires field {} to exist", field)),
                    );
                } else {
                    notes.push(format!(
                        "Field {} was required by the operator {}",
                        field, op
                    ));
                }

                if let Some(ref span) = t.pos {
                    labels.push(
                        secondary(span).with_message(format!("field {} is missing here", field)),
                    );
                }

                vec![Diagnostic::error()
                    .with_message("Missing field")
                    .with_labels(labels)]
            }
            EvalError::NotEnoughArgs(count, op, span_opt) => {
                let mut labels = Vec::new();
                let mut notes = Vec::new();
                let msg = format!(
                    "{} expects {} arguments, but not enough were provided",
                    op, count
                );

                if let Some(span) = span_opt {
                    labels.push(
                        Label::primary(span.src_id, span.start.to_usize()..span.end.to_usize())
                            .with_message(msg),
                    );
                } else {
                    notes.push(msg);
                }

                vec![Diagnostic::error()
                    .with_message("Not enough arguments")
                    .with_labels(labels)
                    .with_notes(notes)]
            }
            EvalError::MergeIncompatibleArgs(t1, t2, span_opt) => {
                let mut labels = vec![
                    primary_term(&t1, files).with_message("cannot merge this expression"),
                    primary_term(&t2, files).with_message("with this expression"),
                ];

                if let Some(span) = span_opt {
                    labels.push(secondary(&span).with_message("merged here"));
                }

                vec![Diagnostic::error()
                    .with_message("Non mergeable terms")
                    .with_labels(labels)]
            }
            EvalError::UnboundIdentifier(Ident(ident), span_opt) => vec![Diagnostic::error()
                .with_message("Unbound identifier")
                .with_labels(vec![primary_alt(span_opt, ident.clone(), files)
                    .with_message("this identifier is unbound")])],
            EvalError::Other(msg, span_opt) => {
                let labels = span_opt
                    .as_ref()
                    .map(|span| vec![primary(span).with_message("here")])
                    .unwrap_or(Vec::new());

                vec![Diagnostic::error().with_message(msg).with_labels(labels)]
            }
            EvalError::InternalError(msg, span_opt) => {
                let labels = span_opt
                    .as_ref()
                    .map(|span| vec![primary(span).with_message("here")])
                    .unwrap_or(Vec::new());

                vec![Diagnostic::error()
                    .with_message(format!("Internal error ({})", msg))
                    .with_labels(labels)
                    .with_notes(vec![String::from(INTERNAL_ERROR_MSG)])]
            }
        }
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
                Diagnostic::error().with_message(format!(
                    "Unexpected end of file when parsing {}",
                    files.name(file_id.clone()).to_string_lossy()
                ))
            }
            ParseError::UnexpectedToken(span, _expected) => Diagnostic::error()
                .with_message("Unexpected token")
                .with_labels(vec![primary(span)]),
            ParseError::ExtraToken(span) => Diagnostic::error()
                .with_message("Superfluous unexpected token")
                .with_labels(vec![primary(span)]),
            ParseError::UnmatchedCloseBrace(span) => Diagnostic::error()
                .with_message("Unmatched closing brace \'}\'")
                .with_labels(vec![primary(span)]),
            ParseError::NumThenIdent(span) => Diagnostic::error()
                .with_message("Invalid character in a number literal")
                .with_labels(vec![primary(span)]),
            ParseError::InvalidEscapeSequence(span) => Diagnostic::error()
                .with_message("Invalid escape sequence")
                .with_labels(vec![primary(span)]),
        };

        vec![diagnostic]
    }
}

impl ToDiagnostic<FileId> for TypecheckError {
    fn to_diagnostic(
        &self,
        _files: &mut Files<String>,
        _contract_id: Option<FileId>,
    ) -> Vec<Diagnostic<FileId>> {
        match self {
            _ => vec![Diagnostic::error()
                .with_message("Typechecking failed [WIP].")
                .with_notes(vec![format!("{:?}", self)])],
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
                    .as_ref()
                    .map(|span| vec![secondary(span).with_message("imported here")])
                    .unwrap_or(Vec::new());

                vec![Diagnostic::error()
                    .with_message(format!("Import of {} failed: {}", path, error))
                    .with_labels(labels)]
            }
            ImportError::ParseError(error, span_opt) => {
                let mut diagnostic = error.to_diagnostic(files, contract_id);

                if let Some(span) = span_opt {
                    diagnostic[0]
                        .labels
                        .push(secondary(span).with_message("imported here"));
                }

                diagnostic
            }
        }
    }
}
