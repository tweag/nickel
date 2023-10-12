//! Define the type of labels.
//!
//! A label is a value holding metadata relative to contract checking. It gives the user useful
//! information about the context of a contract failure.
use std::{collections::HashMap, rc::Rc};

use crate::{
    eval::cache::{Cache as EvalCache, CacheIndex},
    identifier::LocIdent,
    mk_uty_enum, mk_uty_record,
    position::{RawSpan, TermPos},
    term::{
        record::{Field, RecordData},
        RichTerm, SealingKey, Term,
    },
    typ::{Type, TypeF},
    typecheck::{ReifyAsUnifType, UnifType},
};

use codespan::Files;

pub mod ty_path {
    //! Type paths.
    //!
    //! Checking higher-order contracts can involve a good share of intermediate contract checking.
    //! Take the following example:
    //! ```text
    //! (fun ev => fun cst => ev (fun x => cst))
    //!   | ((Number -> Number) -> Number) -> Number -> Number,
    //! ```
    //! Once called, various checks will be performed on the arguments of functions and their return
    //! values:
    //!
    //! 1. Check that `ev` provides a `Number` to `(fun x => cst)`
    //! 2. Check that `(fun x => cst)` returns a `Number`
    //! 3. Check that `ev (fun x => cst)` return a `Number`
    //! 4. etc.
    //!
    //! Each check can be linked to a base type occurrence (here, a `Number`) in the original type:
    //! ```text
    //! ((Number -> Number) -> Number) -> Number -> Number
    //!   ^^^^^1    ^^^^^^2    ^^^^^^3    etc.
    //! ```
    //!
    //! This is the information encoded by a type path: what part of the original type is currently
    //! being checked by this label. It is then reported to the user in case of a blame.
    //!
    //! Paths are encoded as lists of elements, specifying if the next step is either to go to the
    //! **domain** or to the **codomain**.
    //!
    //! When reporting a blame error on a record type, one faces the same situation as with
    //! higher-order functions: the precise cause of an error can correspond to a small subtype of
    //! the original record type. Type path elements can thus also consist of a record field,
    //! indicating that the path leading to the subtype of interest goes through a record via a
    //! particular field.

    use crate::{
        identifier::LocIdent,
        position::RawSpan,
        typ::{RecordRowF, RecordRowsIteratorItem, Type, TypeF},
    };

    /// An element of a path type.
    #[derive(Debug, Clone, PartialEq, Eq, Copy)]
    pub enum Elem {
        Domain,
        Codomain,
        Field(LocIdent),
        Array,
        Dict,
    }

    pub type Path = Vec<Elem>;

    /// Determine if the path has no `Domain` arrow component in it.
    pub fn has_no_dom(p: &Path) -> bool {
        !p.iter().any(|elt| matches!(*elt, Elem::Domain))
    }

    /// Determine if the path is not higher order, that is, if it doesn't contain any arrow.
    pub fn has_no_arrow(p: &Path) -> bool {
        !p.iter()
            .any(|elt| matches!(*elt, Elem::Domain | Elem::Codomain))
    }

    #[derive(Clone, Debug)]
    /// Represent the span of a type path in the string representation of the corresponding type,
    /// together with additional data useful to error reporting.
    ///
    /// Produced by [`span`]. The last element of the path that could be matched with the type is
    /// returned, as well as the last element of the part of the path that could be matched which
    /// is an arrow element (`Domain` or `Codomain`).
    ///
    /// In the general case, this should be respectively the last element of the path and the last
    /// element of the path filtered by keeping only `Domain` and `Codomain`. But sometimes the
    /// `span` function couldn't make further progress:
    ///
    /// ```nickel
    /// let Foo = Array Num in
    /// let f : Num -> Foo = fun x => ["a"] in
    /// f 0
    /// ```
    ///
    /// This code will blame with path `[Codomain, Array]`. However, because the "alias" `Foo` is
    /// used for `Array Num`, we can't descend further inside `Foo` which doesn't have the shape
    /// `Array _`. `span` will stop here, and the last elements saved in `PathSpan` will both be
    /// `Codomain`, instead of `Array` and `Codomain`. This helps specializing the error message
    /// accordingly.
    pub struct PathSpan {
        /// The span of the subtype corresponding to the path.
        pub span: RawSpan,
        pub last: Option<Elem>,
        pub last_arrow_elem: Option<Elem>,
    }

    /// Return the span encoded (as well as additional data: see [PathSpan]) by a type path in the
    /// string representation of the corresponding type.
    ///
    /// Used in the error reporting of blame errors (see `crate::error::report_ty_path`).
    ///
    /// # Returns
    ///
    /// The function returns `None` if the position of the found subtype is not defined.
    ///
    /// # Example
    ///
    /// - Type path: `[Codomain, Domain]`
    /// - Type : `Num -> Num -> Num`
    /// - Return: `{start: 7, end: 10, last: Some(Domain), last_arrow_elem: Some(Domain)}`. The
    ///   span `(7, 10)` corresponds to the second `Num` occurrence.
    ///
    /// # Mismatch between `path` and `ty`
    ///
    /// If at some point the shape of the type `ty` doesn't correspond to the path (e.g. if the
    /// next element is `Array`, but the type isn't of the form `Array _`), `span` just reports the
    /// span of the whole current subtype.
    ///
    /// ```nickel
    /// let Foo = Array Num in
    /// ["a"] | Foo
    /// ```
    ///
    /// Here, the type path will contain an `Array` (added by the builtin implementation of the
    /// `Array` contract), but the original type will be `Foo`, which isn't of the form `Array _`.
    /// Thus we can't underline the subtype `_`, and stops at the whole `Array T`.
    pub fn span<'a, I>(mut path_it: std::iter::Peekable<I>, mut ty: &Type) -> Option<PathSpan>
    where
        I: Iterator<Item = &'a Elem>,
        I: std::clone::Clone,
    {
        while let TypeF::Forall { body, .. } = &ty.typ {
            ty = body.as_ref();
        }

        match (&ty.typ, path_it.next()) {
            (TypeF::Arrow(dom, codom), Some(next)) => match next {
                Elem::Domain => {
                    let path_span = span(path_it, dom.as_ref())?;
                    Some(PathSpan {
                        last: path_span.last.or(Some(*next)),
                        last_arrow_elem: path_span.last_arrow_elem.or(Some(*next)),
                        ..path_span
                    })
                }
                Elem::Codomain => {
                    let path_span = span(path_it, codom.as_ref())?;
                    Some(PathSpan {
                        last: path_span.last.or(Some(*next)),
                        last_arrow_elem: path_span.last_arrow_elem.or(Some(*next)),
                        ..path_span
                    })
                }
                _ => panic!(
                    "span(): \
                    seeing an arrow type, but the type path is neither domain nor codomain"
                ),
            },
            (TypeF::Record(rows), next @ Some(Elem::Field(ident))) => {
                for row_item in rows.iter() {
                    match row_item {
                        RecordRowsIteratorItem::Row(RecordRowF { id, typ: ty }) if id == *ident => {
                            let path_span = span(path_it, ty)?;

                            return Some(PathSpan {
                                last: path_span.last.or_else(|| next.copied()),
                                last_arrow_elem: path_span.last_arrow_elem,
                                ..path_span
                            });
                        }
                        RecordRowsIteratorItem::Row(RecordRowF { .. }) => (),
                        RecordRowsIteratorItem::TailDyn | RecordRowsIteratorItem::TailVar(_) => (),
                    }
                }

                panic!(
                    "span: current type path element indicates to go to field `{}`, \
                     but this field doesn't exist in {}",
                    ident,
                    Type::from(TypeF::Record(rows.clone())),
                )
            }
            (TypeF::Array(ty), Some(Elem::Array)) if ty.as_ref().typ == TypeF::Dyn =>
            // Dyn shouldn't be the target of any blame
            {
                panic!("span(): unexpected blame of a dyn contract inside an array")
            }
            (TypeF::Array(ty), next @ Some(Elem::Array)) => {
                let path_span = span(path_it, ty)?;

                Some(PathSpan {
                    last: path_span.last.or_else(|| next.copied()),
                    last_arrow_elem: path_span.last_arrow_elem,
                    ..path_span
                })
            }
            (TypeF::Dict { type_fields, .. }, next @ Some(Elem::Dict)) => {
                let path_span = span(path_it, type_fields)?;

                Some(PathSpan {
                    last: path_span.last.or_else(|| next.copied()),
                    last_arrow_elem: path_span.last_arrow_elem,
                    ..path_span
                })
            }
            // The type and the path don't match, or the path is empty. We stop here.
            _ => ty.pos.into_opt().map(|span| PathSpan {
                span,
                last: None,
                last_arrow_elem: None,
            }),
        }
    }
}

/// A blame label.
///
/// A label is associated to a contract check (introduced by an explicit contract annotation, or
/// implicitly by a merge expression) and contains information to report to the user when a
/// contract fails and a blame occurs.
/// It includes in particular a [type path][ty_path::Path] and a **polarity**.
///
/// # Polarity
///
/// One crucial aspect of first class contracts is to be able to check higher-order types, which
/// are types with arrows in it. Consider the simplest example:
///
/// ```text
/// f | Number -> Number
/// ```
///
/// This does not entail that `f` returns a `Number` in *every* situation. The identity function
/// `id = fun x => x` can certainly be given the type `Number -> Number`, but `id "a" = "a"` is not
/// a `Number`.
///
/// To satisfy the contract `Number -> Number` for `f` is to satisfy the predicate "if you give me
/// a `Number` as an argument, I give you a `Number` as a result". There is an additional contract
/// to be checked, which is not the responsibility of `f`, but the caller's (or context) one.
///
/// `f | Number -> Number` should thus be evaluated as `fun arg => ((f (arg | Number)) | Number)`,
/// but we want to report the failures of the two introduced subcontracts in a different way:
///
///  - The inner one (on the argument) says that `f` has been misused: it has been applied to
///  something that is not a `Number`.
///  - The outer one says that `f` failed to satisfy its contract, as it has been provided with a
///  `Number` (otherwise the inner contracts would have failed before) but failed to deliver a
///  `Number`.
///
/// This duality caller/callee or function/context is indicated by the polarity: the outer
/// corresponds to a *positive* polarity (the contract is on the term), while the inner corresponds
/// to a *negative* one (the contact is on the context). The polarity always starts as `true` in
/// user-written contracts, but is toggled in the argument contract when the interpreter decomposes
/// an higher order-contract. This also generalizes to higher types such as `((Number -> Number) ->
/// Number) -> Number` where the polarity alternates each time.
#[derive(Debug, Clone, PartialEq)]
pub struct Label {
    /// The type checked by the original contract.
    pub typ: Rc<Type>,

    /// Custom diagnostics set by user code. There might be several diagnostics stacked up, as some
    /// contracts might in turn apply other subcontracts.
    ///
    /// The last diagnostic of the stack is usually the current working diagnostic (the one mutated
    /// by corresponding primops), and the latest/most precise when a blame error is raised.
    pub diagnostics: Vec<ContractDiagnostic>,

    /// The position of the original contract.
    pub span: RawSpan,

    /// The index corresponding to the value being checked. Set at run-time by the interpreter.
    pub arg_idx: Option<CacheIndex>,

    /// The original position of the value being checked. Set at run-time by the interpreter.
    pub arg_pos: TermPos,

    /// The polarity, used for higher-order contracts, that specifies if the current contract is
    /// on the environment (ex, the argument of a function) or on the term.
    pub polarity: Polarity,

    /// The path of the type being currently checked in the original type.
    pub path: ty_path::Path,

    /// An environment mapping type variables to [`TypeVarData`]. Used by polymorphic contracts to
    /// decide which actions to take when encountering a `forall`.
    pub type_environment: HashMap<SealingKey, TypeVarData>,

    /// The name of the record field to report in blame errors. This is set
    /// while first transforming a record as part of the pending contract generation.
    /// Contract applications outside of records will have this field set to `None`.
    pub field_name: Option<LocIdent>,
}

/// Data about type variables that is needed for polymorphic contracts to decide which actions to
/// take.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeVarData {
    pub polarity: Polarity,
}

impl From<&TypeVarData> for Term {
    fn from(value: &TypeVarData) -> Self {
        Term::Record(RecordData {
            fields: [(
                LocIdent::new("polarity"),
                Field::from(RichTerm::from(Term::from(value.polarity))),
            )]
            .into(),
            attrs: Default::default(),
            sealed_tail: None,
        })
    }
}

impl ReifyAsUnifType for TypeVarData {
    fn unif_type() -> UnifType {
        mk_uty_record!(("polarity", Polarity::unif_type()))
    }
}

impl ReifyAsUnifType for Polarity {
    fn unif_type() -> UnifType {
        mk_uty_enum!("Positive", "Negative")
    }
}
/// A polarity. See [`Label`]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Polarity {
    Positive,
    Negative,
}

impl Polarity {
    pub fn flip(self) -> Self {
        match self {
            Polarity::Positive => Polarity::Negative,
            Polarity::Negative => Polarity::Positive,
        }
    }
}

impl From<Polarity> for Term {
    fn from(value: Polarity) -> Self {
        match value {
            Polarity::Positive => Term::Enum(LocIdent::new("Positive")),
            Polarity::Negative => Term::Enum(LocIdent::new("Negative")),
        }
    }
}

impl TryFrom<&Term> for Polarity {
    type Error = ();

    fn try_from(value: &Term) -> Result<Self, Self::Error> {
        match value {
            Term::Enum(positive) if positive.label() == "Positive" => Ok(Self::Positive),
            Term::Enum(negative) if negative.label() == "Negative" => Ok(Self::Negative),
            _ => Err(()),
        }
    }
}

/// Custom reporting diagnostic that can be set by user-code through the `label` API. Used to
/// customize contract error messages, and provide more context than "a contract has failed".
#[derive(Debug, Clone, PartialEq, Default)]
pub struct ContractDiagnostic {
    /// The main error message tag to be printed together with the error message.
    pub message: Option<String>,
    /// Additional notes printed at the end of the message.
    pub notes: Vec<String>,
}

impl ContractDiagnostic {
    pub fn new() -> Self {
        Self::default()
    }

    /// Attach a message to this diagnostic, and return the updated value. Erase potential previous
    /// message.
    pub fn with_message(mut self, message: impl Into<String>) -> Self {
        self.message = Some(message.into());
        self
    }

    /// Attach notes to this diagnostic, and return the updated value. Erase potential previous
    /// notes.
    pub fn with_notes(mut self, notes: Vec<String>) -> Self {
        self.notes = notes;
        self
    }

    /// Append a note to this diagnostic.
    pub fn append_note(&mut self, note: impl Into<String>) {
        self.notes.push(note.into());
    }

    /// Return `true` if this diagnostic is empty, that is if `message` is either not set (`None`)
    /// or is set but empty, AND notes are empty.
    pub fn is_empty(&self) -> bool {
        self.message.as_ref().map(String::is_empty).unwrap_or(true) && self.notes.is_empty()
    }
}

impl Label {
    /// Generate a dummy label for testing purpose.
    pub fn dummy() -> Label {
        Label {
            typ: Rc::new(Type::from(TypeF::Number)),
            diagnostics: vec![ContractDiagnostic::new().with_message(String::from("testing"))],
            span: RawSpan {
                src_id: Files::new().add("<test>", String::from("empty")),
                start: 0.into(),
                end: 1.into(),
            },
            polarity: Polarity::Positive,
            ..Default::default()
        }
    }

    pub fn get_evaluated_arg<EC: EvalCache>(&self, cache: &EC) -> Option<RichTerm> {
        self.arg_idx.clone().map(|idx| cache.get(idx).body)
    }

    /// Set the message of the current diagnostic (the last diagnostic of the stack). Potentially
    /// erase the previous value.
    ///
    /// If the diagnostic stack is empty, this method pushes a new diagnostic with the given notes.
    pub fn with_diagnostic_message(mut self, message: impl Into<String>) -> Self {
        if let Some(current) = self.diagnostics.last_mut() {
            current.message = Some(message.into());
        } else {
            self.diagnostics
                .push(ContractDiagnostic::new().with_message(message));
        };

        self
    }

    /// Set the notes of the current diagnostic (the last diagnostic of the stack). Potentially
    /// erase the previous value.
    ///
    /// If the diagnostic stack is empty, this method pushes a new diagnostic with the given notes.
    pub fn with_diagnostic_notes(mut self, notes: Vec<String>) -> Self {
        if let Some(current) = self.diagnostics.last_mut() {
            current.notes = notes;
        } else {
            self.diagnostics
                .push(ContractDiagnostic::new().with_notes(notes));
        };

        self
    }

    /// Append a note to the current diagnostic (the last diagnostic of the stack). Potentially
    /// erase the previous value.
    ///
    /// If the diagnostic stack is empty, this method pushes a new diagnostic with the given note.
    pub fn append_diagnostic_note(mut self, note: impl Into<String>) -> Self {
        if let Some(current) = self.diagnostics.last_mut() {
            current.append_note(note);
        } else {
            self.diagnostics
                .push(ContractDiagnostic::new().with_notes(vec![note.into()]));
        };

        self
    }

    /// Return a reference to the current contract diagnostic, which is the last element of the
    /// stack, if any.
    pub fn current_diagnostic(&self) -> Option<&ContractDiagnostic> {
        self.diagnostics.last()
    }

    /// Push a new, fresh diagnostic on the stack if the current diagnostic isn't empty. This has
    /// the effect of saving the current diagnostic, that can't then be mutated anymore by the
    /// label's API.
    pub fn push_diagnostic(&mut self) {
        if matches!(self.current_diagnostic(), Some(diag) if !diag.is_empty()) {
            self.diagnostics.push(ContractDiagnostic::new());
        }
    }

    pub fn with_field_name(self, field_name: Option<LocIdent>) -> Self {
        Label { field_name, ..self }
    }

    /// Tests if the contract associated to this label might have polymorphic subcontracts
    /// (equivalently, if the contract is derived from a type which has free type variables). Such
    /// contracts are special, in particular because they aren't idempotent and thus can't be
    /// freely deduplicated.
    ///
    /// This check is an over approximation and might return `true` even if the contract is not
    /// polymorphic, in exchange of being fast (constant time).
    pub fn can_have_poly_ctrs(&self) -> bool {
        // Checking that the type environment is not empty is a bit coarse: what it actually checks
        // is that this contract is derived from the body of a `forall`. For example, in `forall a.
        // a -> Number`, `Number` isn't polymorphic, but `has_polymorphic_ctrs` will return `true`.
        !self.type_environment.is_empty()
    }
}

impl Default for Label {
    fn default() -> Label {
        Label {
            typ: Rc::new(Type::from(TypeF::Dyn)),
            span: RawSpan {
                src_id: Files::new().add("<null>", String::from("")),
                start: 0.into(),
                end: 1.into(),
            },
            polarity: Polarity::Positive,
            diagnostics: Default::default(),
            arg_idx: Default::default(),
            arg_pos: Default::default(),
            path: Default::default(),
            type_environment: Default::default(),
            field_name: None,
        }
    }
}

/// Possible origins of a merge operation.
#[derive(Clone, Copy, Eq, PartialEq, Debug, Default)]
pub enum MergeKind {
    /// A standard, user-written merge operation (or a merge operation descending from a
    /// user-written merge operation).
    #[default]
    Standard,
    /// A merge generated by the parser when reconstructing piecewise definition, for example:
    ///
    /// ```nickel
    /// { foo = def1, foo = def2}
    /// ```
    PiecewiseDef,
}

/// A merge label.
///
/// Like [`Label`], a merge label is used to carry and propagate error reporting data during the
/// evaluation. While [`Label`] is used for contracts, `MergeLabel` is used for merging. The latter
/// track less information than the former, but this information is still important. Indeed,
/// recursive merging of records usually can't track original positions very well. The merge label
/// allows to at least remember the original position of the merge, as written somewhere in a
/// Nickel source.
///
/// Additionally, merging arrays currently generates a contract and its associated label for which
/// we don't necessarily have a defined span at hand. The merge label makes it possible to fallback
/// to the original position of the merge.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct MergeLabel {
    /// The span of the original merge (which might then decompose into many others).
    pub span: RawSpan,
    pub kind: MergeKind,
}

impl From<Label> for MergeLabel {
    fn from(label: Label) -> Self {
        MergeLabel {
            span: label.span,
            kind: Default::default(),
        }
    }
}
