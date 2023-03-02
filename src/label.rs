//! Define the type of labels.
//!
//! A label is a value holding metadata relative to contract checking. It gives the user useful
//! information about the context of a contract failure.
use std::rc::Rc;

use crate::{
    eval::cache::{Cache as EvalCache, CacheIndex},
    position::{RawSpan, TermPos},
    term::RichTerm,
    types::{TypeF, Types},
};

use codespan::Files;

pub mod ty_path {
    //! Type paths.
    //!
    //! Checking higher-order contracts can involve a good share of intermediate contract checking.
    //! Take the following example:
    //! ```text
    //! Assume((Num -> Num) -> Num) -> Num -> Num, fun ev => fun cst => ev (fun x => cst))
    //! ```
    //! Once called, various checks will be performed on the arguments of functions and their return
    //! values:
    //! 1. Check that `ev` provides a `Num` to `(fun x => cst)`
    //! 2. Check that `(fun x => cst)` returns a `Num`
    //! 3. Check that `ev (fun x => cst)` return a `Num`
    //! 4. etc.
    //!
    //! Each check can be linked to a base type occurrence (here, a `Num`) in the original type:
    //! ```text
    //! (Num -> Num) -> Num) -> Num -> Num
    //!  ^^^1   ^^^2    ^^^3    etc.
    //! ```
    //!
    //! This is the information encoded by a type path: what part of the original type is currently
    //! being checked by this label. It is then reported to the user in case of a blame.
    //!
    //! Paths are encoded as lists of elements, specifying if the next step is either to go to the **domain**
    //! or to the **codomain**.
    //!
    //! When reporting a blame error on a record type, one faces the same situation as with
    //! higher-order functions: the precise cause of an error can correspond to a small subtype of
    //! the original record type. Type path elements can thus also consist of a record field,
    //! indicating that the path leading to the subtype of interest goes through a record via a
    //! particular field.

    use crate::{
        identifier::Ident,
        position::TermPos,
        types::{RecordRowF, RecordRowsIteratorItem, TypeF, Types},
    };

    /// An element of a path type.
    #[derive(Debug, Clone, PartialEq, Eq, Copy)]
    pub enum Elem {
        Domain,
        Codomain,
        Field(Ident),
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

    #[derive(Clone, Copy, Debug)]
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
        pub start: usize,
        pub end: usize,
        pub last: Option<Elem>,
        pub last_arrow_elem: Option<Elem>,
    }

    /// Return the span encoded (as well as additional data: see [PathSpan]) by a type path in the
    /// string representation of the corresponding type.
    ///
    /// Used in the error reporting of blame errors (see `crate::error::report_ty_path`).
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
    pub fn span<'a, I>(mut path_it: std::iter::Peekable<I>, mut ty: &Types) -> PathSpan
    where
        I: Iterator<Item = &'a Elem>,
        I: std::clone::Clone,
    {
        // peek() returns a reference, and hence keeps a mutable borrow of `path_it` which forbids
        // to call to next() in the same region. This is why we need to split the match in two
        // different blocks.
        let forall_offset = match (&ty.ty, path_it.peek()) {
            (_, None) => {
                let repr = format!("{ty}");
                return PathSpan {
                    start: 0,
                    end: repr.len(),
                    last: None,
                    last_arrow_elem: None,
                };
            }
            (TypeF::Forall { .. }, Some(_)) => {
                // The length of "forall" plus the final separating dot and whitespace ". "
                let mut result = 8;
                while let TypeF::Forall { var, body, .. } = &ty.ty {
                    // The length of the identifier plus the preceding whitespace
                    result += var.to_string().len() + 1;
                    ty = body.as_ref();
                }

                result
            }
            _ => 0,
        };

        match (&ty.ty, path_it.next()) {
            (TypeF::Arrow(dom, codom), Some(next)) => {
                // The potential shift of the start position of the domain introduced by the couple
                // of parentheses around the domain. Parentheses are added when printing a function
                // type whose domain is itself a function.
                // For example, `Arrow(Arrow(Num, Num), Num)` is rendered as "(Num -> Num) -> Num".
                // In this case, the position of the sub-type "Num -> Num" starts at 1 instead of
                // 0.
                let paren_offset = match dom.ty {
                    TypeF::Arrow(_, _) => 1,
                    _ => 0,
                };

                match next {
                    Elem::Domain => {
                        let PathSpan { start: dom_start, end: dom_end, last, last_arrow_elem} = span(path_it, dom.as_ref());
                        PathSpan {
                            start: dom_start + paren_offset + forall_offset,
                            end: dom_end + paren_offset + forall_offset,
                            last: last.or(Some(*next)),
                            last_arrow_elem: last_arrow_elem.or(Some(*next)),
                        }
                    }
                    Elem::Codomain => {
                        let PathSpan {end: dom_end, ..} = span(std::iter::empty().peekable(), dom.as_ref());
                        let PathSpan {start: codom_start, end: codom_end, last, last_arrow_elem} = span(path_it, codom.as_ref());
                        // At this point, paren_offset is:
                        // (a) `1` if there is a couple of parentheses around the domain
                        // (b) `0` otherwise
                        // In case (a), we need to shift the beginning of the codomain by two,
                        // to also take into account the closing ')' character, whence the `offset*2`.
                        // The `4` corresponds to the arrow " -> ".
                        let offset = (paren_offset * 2) + 4 + dom_end + forall_offset;
                        PathSpan {
                            start: codom_start + offset,
                            end: codom_end + offset,
                            last: last.or(Some(*next)),
                            last_arrow_elem: last_arrow_elem.or(Some(*next)),
                        }
                    }
                    _ => panic!("span(): seeing an arrow type, but the type path is neither domain nor codomain"),
                }
            }
            (TypeF::Record(rows), next @ Some(Elem::Field(ident))) => {
                // initial "{"
                let mut start_offset = 1;
                // middle ": " between the field name and the type
                let id_offset = 2;
                // The ", " between two fields
                let end_offset = 2;

                for row_item in rows.iter() {
                    match row_item {
                        RecordRowsIteratorItem::Row(RecordRowF { id, types: ty })
                            if id == *ident =>
                        {
                            let PathSpan {
                                start: sub_start,
                                end: sub_end,
                                last,
                                last_arrow_elem,
                            } = span(path_it, ty);
                            let full_offset = start_offset + format!("{id}").len() + id_offset;

                            return PathSpan {
                                start: full_offset + sub_start,
                                end: full_offset + sub_end,
                                last: last.or_else(|| next.copied()),
                                last_arrow_elem,
                            };
                        }
                        RecordRowsIteratorItem::Row(RecordRowF { id, types: ty }) => {
                            // The last +1 is for the
                            start_offset += format!("{id}").len()
                                + id_offset
                                + format!("{ty}").len()
                                + end_offset;
                        }
                        RecordRowsIteratorItem::TailDyn | RecordRowsIteratorItem::TailVar(_) => (),
                    }
                }

                panic!(
                    "span: current type path element indicates to go to field `{}`,\
but this field doesn't exist in {}",
                    ident,
                    Types::with_default_pos(TypeF::Record(rows.clone())),
                )
            }
            (TypeF::Array(ty), Some(Elem::Array)) if ty.as_ref().ty == TypeF::Dyn =>
            // Dyn shouldn't be the target of any blame
            {
                panic!("span(): unexpected blame of a dyn contract inside an array")
            }
            (TypeF::Array(ty), next @ Some(Elem::Array)) => {
                // initial "Array "
                let start_offset = 6;
                let paren_offset = usize::from(!ty.fmt_is_atom());

                let PathSpan {
                    start: sub_start,
                    end: sub_end,
                    last,
                    last_arrow_elem,
                } = span(path_it, ty);
                PathSpan {
                    start: start_offset + paren_offset + sub_start,
                    end: start_offset + paren_offset + sub_end,
                    last: last.or_else(|| next.copied()),
                    last_arrow_elem,
                }
            }
            (TypeF::Dict(ty), next @ Some(Elem::Dict)) => {
                // initial "{_: "
                let start_offset = 4;
                let paren_offset = usize::from(!ty.fmt_is_atom());

                let PathSpan {
                    start: sub_start,
                    end: sub_end,
                    last,
                    last_arrow_elem,
                } = span(path_it, ty);
                PathSpan {
                    start: start_offset + paren_offset + sub_start,
                    end: start_offset + paren_offset + sub_end,
                    last: last.or_else(|| next.copied()),
                    last_arrow_elem,
                }
            }
            // The type and the path don't match, we stop here.
            _ => {
                let repr = format!("{ty}");
                PathSpan {
                    start: 0,
                    end: repr.len(),
                    last: None,
                    last_arrow_elem: None,
                }
            }
        }
    }
}

/// A blame label.
///
/// A label is associated to a contract check (an assume, a promise or a contract as an enriched
/// value) and contains information to report to the user when a contract fails and a blame occurs.
/// It includes in particular a [type path][ty_path::Path] and a **polarity**.
///
/// # Polarity
///
/// One crucial aspect of first class contracts is to be able to check higher-order types, which
/// are types with arrows in it. Consider the simplest example:
///
/// ```text
/// f | Num -> Num
/// ```
///
/// This does not entail that `f` returns a `Num` in *every* situation. The identity function `id
/// = fun x => x` can certainly be given the type `Num -> Num`, but `id "a" = "a"` is not a `Num`.
///
/// To satisfy the contract `Num -> Num` for `f` is to satisfy the predicate "if you give me a
/// `Num` as an argument, I give you a `Num` as a result". There is an additional contract to be
/// checked, which is not the responsibility of `f`, but the caller's (or context)
/// one.
///
/// `f | Num -> Num` should thus be evaluated as `fun arg => ((f (arg | Num)) | Num)`, but we want
/// to report the failures of the two introduced subcontracts in a different way:
///
///  - The inner one (on the argument) says that `f` has been misused: it has been applied to
///  something that is not a `Num`.
///  - The outer one says that `f` failed to satisfy its contract, as it has been provided with a
///  `Num` (otherwise the inner contracts would have failed before) but failed to deliver a `Num`.
///
/// This duality caller/callee or function/context is indicated by the polarity: the outer
/// corresponds to a *positive* polarity (the contract is on the term), while the inner corresponds
/// to a *negative* one (the contact is on the context). The polarity always starts as `true` in
/// user-written contracts, but is toggled in the argument contract when the interpreter decomposes
/// an higher order-contract. This also generalizes to higher types such as `((Num -> Num) -> Num)
/// -> Num` where the polarity alternates each time.
#[derive(Debug, Clone, PartialEq)]
pub struct Label {
    /// The type checked by the original contract.
    pub types: Rc<Types>,
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
    pub polarity: bool,
    /// The path of the type being currently checked in the original type.
    pub path: ty_path::Path,
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
            types: Rc::new(Types::from(TypeF::Num)),
            diagnostics: vec![ContractDiagnostic::new().with_message(String::from("testing"))],
            span: RawSpan {
                src_id: Files::new().add("<test>", String::from("empty")),
                start: 0.into(),
                end: 1.into(),
            },
            polarity: true,
            ..Default::default()
        }
    }

    pub fn get_evaluated_arg<EC: EvalCache>(&self, cache: &EC) -> Option<RichTerm> {
        self.arg_idx.clone().map(|idx| cache.get(idx).body)
    }

    /// Set the message of the current diagnostic (the last diagnostic of the stack). Potentially
    /// erase the previous value.
    ///
    /// If the diagnostic stack is empty, this methods pushes a new diagnostic with the given notes.
    pub fn set_diagnostic_message(&mut self, message: impl Into<String>) {
        if let Some(current) = self.diagnostics.last_mut() {
            current.message = Some(message.into());
        } else {
            self.diagnostics
                .push(ContractDiagnostic::new().with_message(message));
        }
    }

    /// Set the notes of the current diagnostic (the last diagnostic of the stack). Potentially
    /// erase the previous value.
    ///
    /// If the diagnostic stack is empty, this methods pushes a new diagnostic with the given notes.
    pub fn set_diagnostic_notes(&mut self, notes: Vec<String>) {
        if let Some(current) = self.diagnostics.last_mut() {
            current.notes = notes;
        } else {
            self.diagnostics
                .push(ContractDiagnostic::new().with_notes(notes));
        }
    }

    /// Append a note to the current diagnostic (the last diagnostic of the stack). Potentially
    /// erase the previous value.
    ///
    /// If the diagnostic stack is empty, this methods pushes a new diagnostic with the given note.
    pub fn append_diagnostic_note(&mut self, note: impl Into<String>) {
        if let Some(current) = self.diagnostics.last_mut() {
            current.append_note(note);
        } else {
            self.diagnostics
                .push(ContractDiagnostic::new().with_notes(vec![note.into()]));
        }
    }

    /// Return a reference to the current contract diagnostic, that is the last one of the stack,
    /// if any.
    pub fn current_diagnostic(&self) -> Option<&ContractDiagnostic> {
        self.diagnostics.last()
    }
}

impl Default for Label {
    fn default() -> Label {
        Label {
<<<<<<< HEAD
            types: Rc::new(Types(TypeF::Dyn)),
=======
            types: Rc::new(Types {
                ty: TypeF::Dyn,
                pos: TermPos::None,
            }),
            tag: "".to_string(),
>>>>>>> bedbab36 (Add `pos` field to all `Type`, and fix for all modules.)
            span: RawSpan {
                src_id: Files::new().add("<null>", String::from("")),
                start: 0.into(),
                end: 1.into(),
            },
            polarity: true,
            diagnostics: Default::default(),
            arg_idx: Default::default(),
            arg_pos: Default::default(),
            path: Default::default(),
        }
    }
}
