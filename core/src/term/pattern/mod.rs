//! Pattern matching and destructuring of Nickel values.
use std::collections::{hash_map::Entry, HashMap};

use crate::{
    error::EvalError,
    identifier::LocIdent,
    impl_display_from_pretty,
    label::Label,
    mk_app,
    parser::error::ParseError,
    position::TermPos,
    stdlib::internals,
    term::{
        record::{Field, RecordAttrs, RecordData},
        LabeledType, RichTerm, Term, TypeAnnotation,
    },
    typ::{Type, TypeF},
};

pub mod compile;

#[derive(Debug, PartialEq, Clone)]
pub enum PatternData {
    /// A simple pattern consisting of an identifier. Match anything and bind the result to the
    /// corresponding identfier.
    Any(LocIdent),
    /// A record pattern as in `{ a = { b, c } }`
    Record(RecordPattern),
    /// An enum pattern as in `'Foo x` or `'Foo`
    Enum(EnumPattern),
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
    pub tail: RecordPatternTail,
    pub pos: TermPos,
}

/// The tail of a record pattern which might capture the rest of the record.
#[derive(Debug, PartialEq, Clone)]
pub enum RecordPatternTail {
    /// The pattern is closed, i.e. it doesn't allow more fields. For example, `{foo, bar}`.
    Empty,
    /// The pattern ends with an ellipsis, making it open. For example, `{foo, bar, ..}`.
    Open,
    /// The pattern ends with an ellispis and a variable capturing the rest of the record. For
    /// example, `{foo, bar, ..rest}`.
    Capture(LocIdent),
}

impl RecordPattern {
    /// Check the matches for duplication, and raise an error if any occur.
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
        matches!(
            self.tail,
            RecordPatternTail::Open | RecordPatternTail::Capture(_)
        )
    }
}

impl FieldPattern {
    /// Convert this field pattern to a record field binding with metadata. Used to generate the
    /// record contract associated to a record pattern.
    pub fn as_record_binding(&self) -> (LocIdent, Field) {
        let mut annotation = self.annotation.clone();
        // If the inner pattern gives rise to a contract, add it the to the field decoration.
        annotation
            .contracts
            .extend(self.pattern.elaborate_contract());

        (self.matched_id, Field::from(annotation))
    }
}

// We don't implement elaborate_contract for `FieldPattern`, which is of a slightly different
// nature (it's a part of a record pattern). Instead, we call to `FieldPattern::as_record_binding`,
// which takes care of elaborating a field pattern to an appropriate record field.
pub trait ElaborateContract {
    /// Elaborate a contract from this pattern. The contract will check both the structure of the
    /// matched value (e.g. the presence of fields in a record) and incoporate user-provided
    /// contracts and annotations, as well as default values.
    ///
    /// Some patterns don't give rise to any contract (e.g. `Any`), in which case this function
    /// returns `None`.
    fn elaborate_contract(&self) -> Option<LabeledType>;
}

impl ElaborateContract for PatternData {
    fn elaborate_contract(&self) -> Option<LabeledType> {
        match self {
            PatternData::Any(_) => None,
            PatternData::Record(pat) => pat.elaborate_contract(),
            PatternData::Enum(pat) => pat.elaborate_contract(),
        }
    }
}

impl ElaborateContract for Pattern {
    fn elaborate_contract(&self) -> Option<LabeledType> {
        self.data.elaborate_contract()
    }
}

impl ElaborateContract for EnumPattern {
    fn elaborate_contract(&self) -> Option<LabeledType> {
        // TODO[adts]: it would be better to simply build a type like `[| 'tag arg |]` or `[| 'tag
        // |]` and to rely on its derived contract. However, for the time being, the contract
        // derived from enum variants isn't implemented yet.
        let contract = if self.pattern.is_some() {
            mk_app!(internals::enum_variant(), Term::Enum(self.tag))
        } else {
            mk_app!(internals::stdlib_contract_equal(), Term::Enum(self.tag))
        };

        let typ = Type {
            typ: TypeF::Flat(contract),
            pos: self.pos,
        };

        Some(LabeledType {
            typ: typ.clone(),
            label: Label {
                typ: typ.into(),
                // [^unwrap-span]: We need the position to be defined here. Hopefully,
                // contract-generating pattern are pattern used in destructuring, and destructuring
                // patterns aren't currently generated by the Nickel interpreter. So we should only
                // encounter user-written patterns here, which should have a position.
                span: self.pos.unwrap(),
                ..Default::default()
            },
        })
    }
}

impl ElaborateContract for RecordPattern {
    fn elaborate_contract(&self) -> Option<LabeledType> {
        let typ = Type {
            typ: TypeF::Flat(
                Term::Record(RecordData::new(
                    self.patterns
                        .iter()
                        .map(FieldPattern::as_record_binding)
                        .collect(),
                    RecordAttrs {
                        open: self.is_open(),
                        ..Default::default()
                    },
                    None,
                ))
                .into(),
            ),
            pos: self.pos,
        };

        Some(LabeledType {
            typ: typ.clone(),
            label: Label {
                typ: typ.into(),
                // unwrap(): cf [^unwrap-span]
                span: self.pos.unwrap(),
                ..Default::default()
            },
        })
    }
}

impl_display_from_pretty!(PatternData);
impl_display_from_pretty!(Pattern);
impl_display_from_pretty!(RecordPattern);
impl_display_from_pretty!(EnumPattern);
