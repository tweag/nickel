//! Pattern matching and destructuring of Nickel values.
use std::collections::{hash_map::Entry, HashMap};

use super::{
    record::{Field, RecordData},
    NickelString, Number, RichTerm, TypeAnnotation,
};

use crate::{
    error::EvalError, identifier::LocIdent, impl_display_from_pretty, parser::error::ParseError,
    position::TermPos,
};

pub mod compile;

/// A small helper to generate a

#[derive(Debug, PartialEq, Clone)]
pub enum PatternData {
    /// A wildcard pattern, matching any value. As opposed to any, this pattern doesn't bind any
    /// variable.
    Wildcard,
    /// A simple pattern consisting of an identifier. Match anything and bind the result to the
    /// corresponding identifier.
    Any(LocIdent),
    /// A record pattern as in `{ a = { b, c } }`
    Record(RecordPattern),
    /// An array pattern as in `[a, b, c]`
    Array(ArrayPattern),
    /// An enum pattern as in `'Foo x` or `'Foo`
    Enum(EnumPattern),
    /// A constant pattern as in `42` or `true`.
    Constant(ConstantPattern),
    /// A sequence of alternative patterns as in `'Foo _ or 'Bar _ or 'Baz _`.
    Or(OrPattern),
}

/// A generic pattern, that can appear in a match expression (not yet implemented) or in a
/// destructuring let-binding.
#[derive(Debug, PartialEq, Clone)]
pub struct Pattern {
    /// The content of this pattern
    pub data: PatternData,
    /// A potential alias for this pattern, capturing the whole matched value. In the source
    /// language, an alias is introduced by `x @ <pattern>`, where `x` is an arbitrary identifier.
    pub alias: Option<LocIdent>,
    /// The position of the pattern in the source.
    pub pos: TermPos,
}

/// An enum pattern, including both an enum tag and an enum variant.
#[derive(Debug, PartialEq, Clone)]
pub struct EnumPattern {
    pub tag: LocIdent,
    pub pattern: Option<Box<Pattern>>,
    pub pos: TermPos,
}

/// A field pattern inside a record pattern. Every field can be annotated with a type, contracts or
/// with a default value.
#[derive(Debug, PartialEq, Clone)]
pub struct FieldPattern {
    /// The name of the matched field. For example, in `{..., foo = {bar, baz}, ...}`, the matched
    /// identifier is `foo`.
    pub matched_id: LocIdent,
    /// Type and contract annotations of this field.
    pub annotation: TypeAnnotation,
    /// Potentital default value, set with the `? value` syntax.
    pub default: Option<RichTerm>,
    /// The pattern on the right-hand side of the `=`. A pattern like `{foo, bar}`, without the `=`
    /// sign, is parsed as `{foo=foo, bar=bar}`. In this case, `pattern.data` will be
    /// [PatternData::Any].
    pub pattern: Pattern,
    pub pos: TermPos,
}

/// The last match in a data structure pattern. This can either be a normal match, or an ellipsis
/// which can capture the rest of the data structure. The type parameter `P` is the type of the
/// pattern of the data structure: currently, ellipsis matches are only supported for record, but
/// we'll probably support them for arrays as well.
///
/// This enum is mostly used during parsing.
///
/// # Example
///
/// - In `{foo={}, bar}`, the last match is an normal match.
/// - In `{foo={}, bar, ..}`, the last match is a non-capturing ellipsis.
/// - In `{foo={}, bar, ..rest}`, the last match is a capturing ellipsis.
#[derive(Debug, PartialEq, Clone)]
pub enum LastPattern<P> {
    /// The last field is a normal match. In this case the pattern is "closed" so every record
    /// fields should be matched.
    Normal(Box<P>),
    /// The pattern is "open" `, ..}`. Optionally you can bind a record containing the remaining
    /// fields to an `Identifier` using the syntax `, ..y}`.
    Ellipsis(Option<LocIdent>),
}

/// A record pattern.
#[derive(Debug, PartialEq, Clone)]
pub struct RecordPattern {
    /// The patterns for each field in the record.
    pub patterns: Vec<FieldPattern>,
    /// The tail of the pattern, indicating if the pattern is open, i.e. if it ended with an
    /// ellipsis, capturing the rest or not.
    pub tail: TailPattern,
    pub pos: TermPos,
}

/// An array pattern.
#[derive(Debug, PartialEq, Clone)]
pub struct ArrayPattern {
    /// The patterns of the elements of the array.
    pub patterns: Vec<Pattern>,
    /// The tail of the pattern, indicating if the pattern is open, i.e. if it ended with an
    /// ellipsis, capturing the rest or not.
    pub tail: TailPattern,
    pub pos: TermPos,
}

impl ArrayPattern {
    /// Check if this record contract is open, meaning that it accepts additional fields to be
    /// present, whether the rest is captured or not.
    pub fn is_open(&self) -> bool {
        self.tail.is_open()
    }
}

/// A constant pattern, matching a constant value.
#[derive(Debug, PartialEq, Clone)]
pub struct ConstantPattern {
    pub data: ConstantPatternData,
    pub pos: TermPos,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ConstantPatternData {
    Bool(bool),
    Number(Number),
    String(NickelString),
    Null,
}

#[derive(Debug, PartialEq, Clone)]
pub struct OrPattern {
    pub patterns: Vec<Pattern>,
    pub pos: TermPos,
}

/// The tail of a data structure pattern (record or array) which might capture the rest of said
/// data structure.
#[derive(Debug, PartialEq, Clone)]
pub enum TailPattern {
    /// The pattern is closed, i.e. it doesn't allow more fields. For example, `{foo, bar}`.
    Empty,
    /// The pattern ends with an ellipsis, making it open. For example, `{foo, bar, ..}`.
    Open,
    /// The pattern ends with an ellispis and a variable capturing the rest of the record. For
    /// example, `{foo, bar, ..rest}`.
    Capture(LocIdent),
}

impl TailPattern {
    /// Check if this tail pattern makes the enclosing data structure pattern open, meaning that it
    /// accepts additional fields or elements to be present, whether the rest is captured or not.
    pub fn is_open(&self) -> bool {
        matches!(self, TailPattern::Open | TailPattern::Capture(_))
    }
}

impl RecordPattern {
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

impl_display_from_pretty!(PatternData);
impl_display_from_pretty!(Pattern);
impl_display_from_pretty!(ConstantPatternData);
impl_display_from_pretty!(ConstantPattern);
impl_display_from_pretty!(RecordPattern);
impl_display_from_pretty!(EnumPattern);
impl_display_from_pretty!(ArrayPattern);
