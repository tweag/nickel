//! Pattern matching and destructuring of Nickel values.
use std::collections::{hash_map::Entry, HashMap};

use super::{Annotation, Ast, Number};

use crate::{identifier::LocIdent, parser::error::ParseError, position::TermPos, traverse::*};

pub mod bindings;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum PatternData<'ast> {
    /// A wildcard pattern, matching any value. As opposed to any, this pattern doesn't bind any
    /// variable.
    Wildcard,
    /// A simple pattern consisting of an identifier. Match anything and bind the result to the
    /// corresponding identifier.
    Any(LocIdent),
    /// A record pattern as in `{ a = { b, c } }`
    Record(&'ast RecordPattern<'ast>),
    /// An array pattern as in `[a, b, c]`
    Array(&'ast ArrayPattern<'ast>),
    /// An enum pattern as in `'Foo x` or `'Foo`
    Enum(&'ast EnumPattern<'ast>),
    /// A constant pattern as in `42` or `true`.
    Constant(&'ast ConstantPattern<'ast>),
    /// A sequence of alternative patterns as in `'Foo _ or 'Bar _ or 'Baz _`.
    Or(&'ast OrPattern<'ast>),
}

/// A generic pattern, that can appear in a match expression (not yet implemented) or in a
/// destructuring let-binding.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Pattern<'ast> {
    /// The content of this pattern
    pub data: PatternData<'ast>,
    /// A potential alias for this pattern, capturing the whole matched value. In the source
    /// language, an alias is introduced by `x @ <pattern>`, where `x` is an arbitrary identifier.
    pub alias: Option<LocIdent>,
    /// The position of the pattern in the source.
    pub pos: TermPos,
}

/// An enum pattern, including both an enum tag and an enum variant.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct EnumPattern<'ast> {
    pub tag: LocIdent,
    pub pattern: Option<Pattern<'ast>>,
    pub pos: TermPos,
}

/// A field pattern inside a record pattern. Every field can be annotated with a type, contracts or
/// with a default value.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FieldPattern<'ast> {
    /// The name of the matched field. For example, in `{..., foo = {bar, baz}, ...}`, the matched
    /// identifier is `foo`.
    pub matched_id: LocIdent,
    /// Type and contract annotations of this field.
    pub annotation: Annotation<'ast>,
    /// Potential default value, set with the `? value` syntax.
    pub default: Option<Ast<'ast>>,
    /// The pattern on the right-hand side of the `=`. A pattern like `{foo, bar}`, without the `=`
    /// sign, is parsed as `{foo=foo, bar=bar}`. In this case, `pattern.data` will be
    /// [PatternData::Any].
    pub pattern: Pattern<'ast>,
    pub pos: TermPos,
}

/// A record pattern.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct RecordPattern<'ast> {
    /// The patterns for each field in the record.
    pub patterns: &'ast [FieldPattern<'ast>],
    /// The tail of the pattern, indicating if the pattern is open, i.e. if it ended with an
    /// ellipsis, capturing the rest or not.
    pub tail: TailPattern,
    pub pos: TermPos,
}

/// An array pattern.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ArrayPattern<'ast> {
    /// The patterns of the elements of the array.
    pub patterns: &'ast [Pattern<'ast>],
    /// The tail of the pattern, indicating if the pattern is open, i.e. if it ended with an
    /// ellipsis, capturing the rest or not.
    pub tail: TailPattern,
    pub pos: TermPos,
}

impl ArrayPattern<'_> {
    /// Check if this record contract is open, meaning that it accepts additional fields to be
    /// present, whether the rest is captured or not.
    pub fn is_open(&self) -> bool {
        self.tail.is_open()
    }
}

/// A constant pattern, matching a constant value.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ConstantPattern<'ast> {
    pub data: ConstantPatternData<'ast>,
    pub pos: TermPos,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ConstantPatternData<'ast> {
    Bool(bool),
    Number(&'ast Number),
    String(&'ast str),
    Null,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct OrPattern<'ast> {
    pub patterns: &'ast [Pattern<'ast>],
    pub pos: TermPos,
}

/// The tail of a data structure pattern (record or array) which might capture the rest of said
/// data structure.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TailPattern {
    /// The pattern is closed, i.e. it doesn't allow more fields. For example, `{foo, bar}`.
    Empty,
    /// The pattern ends with an ellipsis, making it open. For example, `{foo, bar, ..}`.
    Open,
    /// The pattern ends with an ellispis and a variable capturing the rest of the record. For
    /// example, `{foo, bar, ..rest}`.
    Capture(LocIdent),
}

impl Pattern<'_> {
    /// Creates an `Any` pattern with the corresponding capture name.
    pub fn any(id: LocIdent) -> Self {
        let pos = id.pos;

        Pattern {
            data: PatternData::Any(id),
            alias: None,
            pos,
        }
    }

    /// Returns `Some(id)` if this pattern is an [DataPattern::Any] pattern, `None` otherwise.
    pub fn try_as_any(&self) -> Option<LocIdent> {
        if let PatternData::Any(id) = &self.data {
            Some(*id)
        } else {
            None
        }
    }
}

impl TailPattern {
    /// Check if this tail pattern makes the enclosing data structure pattern open, meaning that it
    /// accepts additional fields or elements to be present, whether the rest is captured or not.
    pub fn is_open(&self) -> bool {
        matches!(self, TailPattern::Open | TailPattern::Capture(_))
    }
}

impl RecordPattern<'_> {
    /// Check the matches for duplication, and raise an error if any occurs.
    ///
    /// Note that for backwards-compatibility reasons this function _only_
    /// checks top-level matches. In Nickel 1.0, this code panicked:
    ///
    /// ```text
    /// let f = fun { x, x, .. } => x in f { x = 1 }
    /// ```
    ///
    /// However this "works", even though the binding to `y` is duplicated.
    ///
    /// ```text
    /// let f =
    ///   fun { x = { y }, z = { y }, .. } => y
    /// in f { x = { y = 1 }, z = { y = 2 } }
    /// # evaluates to 1
    /// ```
    ///
    /// This function aims to raise errors in the first case, but maintain the
    /// behaviour in the second case.
    pub fn check_dup(&self) -> Result<(), ParseError> {
        let mut bindings = HashMap::new();

        for pat in self.patterns.iter() {
            let binding = pat.matched_id;
            let label = binding.label().to_owned();
            match bindings.entry(label) {
                Entry::Occupied(occupied_entry) => {
                    return Err(ParseError::DuplicateIdentInRecordPattern {
                        ident: binding,
                        prev_ident: occupied_entry.remove_entry().1,
                    })
                }
                Entry::Vacant(vacant_entry) => {
                    vacant_entry.insert(binding);
                }
            }
        }

        Ok(())
    }

    /// Check if this record contract is open, meaning that it accepts additional fields to be
    /// present, whether the rest is captured or not.
    pub fn is_open(&self) -> bool {
        self.tail.is_open()
    }
}

impl<'ast> TraverseAlloc<'ast, Ast<'ast>> for Pattern<'ast> {
    fn traverse<F, E>(
        self,
        alloc: &'ast super::AstAlloc,
        f: &mut F,
        order: TraverseOrder,
    ) -> Result<Self, E>
    where
        F: FnMut(Ast<'ast>) -> Result<Ast<'ast>, E>,
    {
        match self.data {
            data @ (PatternData::Wildcard | PatternData::Any(_) | PatternData::Constant(_)) => {
                Ok(Pattern { data, ..self })
            }
            PatternData::Record(record) => {
                let record = record.clone();
                let patterns =
                    traverse_alloc_many(alloc, record.patterns.iter().cloned(), f, order)?;

                Ok(Pattern {
                    data: PatternData::Record(alloc.alloc(RecordPattern { patterns, ..record })),
                    ..self
                })
            }
            PatternData::Array(array) => {
                let array = array.clone();
                let patterns =
                    traverse_alloc_many(alloc, array.patterns.iter().cloned(), f, order)?;

                Ok(Pattern {
                    data: PatternData::Array(alloc.alloc(ArrayPattern { patterns, ..array })),
                    ..self
                })
            }
            PatternData::Enum(enum_pat) => {
                let enum_pat = enum_pat.clone();
                let pattern = enum_pat
                    .pattern
                    .map(|p| p.traverse(alloc, f, order))
                    .transpose()?;

                Ok(Pattern {
                    data: PatternData::Enum(alloc.alloc(EnumPattern {
                        pattern,
                        ..enum_pat
                    })),
                    ..self
                })
            }
            PatternData::Or(or) => {
                let or = or.clone();
                let patterns = traverse_alloc_many(alloc, or.patterns.iter().cloned(), f, order)?;

                Ok(Pattern {
                    data: PatternData::Or(alloc.alloc(OrPattern { patterns, ..or })),
                    ..self
                })
            }
        }
    }

    fn traverse_ref<S, U>(
        &self,
        f: &mut dyn FnMut(&Ast<'ast>, &S) -> TraverseControl<S, U>,
        scope: &S,
    ) -> Option<U> {
        match &self.data {
            PatternData::Wildcard | PatternData::Any(_) | PatternData::Constant(_) => None,
            PatternData::Record(record) => record
                .patterns
                .iter()
                .find_map(|field_pat| field_pat.traverse_ref(f, scope)),
            PatternData::Array(array) => array
                .patterns
                .iter()
                .find_map(|pat| pat.traverse_ref(f, scope)),
            PatternData::Enum(enum_pat) => enum_pat
                .pattern
                .as_ref()
                .and_then(|pat| pat.traverse_ref(f, scope)),
            PatternData::Or(or) => or
                .patterns
                .iter()
                .find_map(|pat| pat.traverse_ref(f, scope)),
        }
    }
}

impl<'ast> TraverseAlloc<'ast, Ast<'ast>> for FieldPattern<'ast> {
    fn traverse<F, E>(
        self,
        alloc: &'ast super::AstAlloc,
        f: &mut F,
        order: TraverseOrder,
    ) -> Result<Self, E>
    where
        F: FnMut(Ast<'ast>) -> Result<Ast<'ast>, E>,
    {
        let annotation = self.annotation.traverse(alloc, f, order)?;
        let default = self
            .default
            .map(|d| d.traverse(alloc, f, order))
            .transpose()?;
        let pattern = self.pattern.traverse(alloc, f, order)?;

        Ok(FieldPattern {
            annotation,
            default,
            pattern,
            ..self
        })
    }

    fn traverse_ref<S, U>(
        &self,
        f: &mut dyn FnMut(&Ast<'ast>, &S) -> TraverseControl<S, U>,
        scope: &S,
    ) -> Option<U> {
        self.annotation
            .traverse_ref(f, scope)
            .or_else(|| self.default.as_ref().and_then(|d| d.traverse_ref(f, scope)))
            .or_else(|| self.pattern.traverse_ref(f, scope))
    }
}

//TODO: restore Pretty and Display.
//impl_display_from_pretty!(PatternData);
//impl_display_from_pretty!(Pattern);
//impl_display_from_pretty!(ConstantPatternData);
//impl_display_from_pretty!(ConstantPattern);
//impl_display_from_pretty!(RecordPattern);
//impl_display_from_pretty!(EnumPattern);
//impl_display_from_pretty!(ArrayPattern);
