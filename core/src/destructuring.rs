//! Pattern matching and destructuring of Nickel values.
use std::collections::{hash_map::Entry, HashMap};

use crate::{
    identifier::LocIdent,
    label::Label,
    parser::error::ParseError,
    position::{RawSpan, TermPos},
    term::{
        record::{Field, RecordAttrs, RecordData},
        LabeledType, Term,
    },
    typ::{Type, TypeF},
};

#[derive(Debug, PartialEq, Clone)]
pub enum Pattern {
    /// A simple pattern consisting of an identifier. Match anything and bind the result to the
    /// corresponding identfier.
    Any(LocIdent),
    /// A record pattern as in `{ a = { b, c } }`
    RecordPattern(RecordPattern),
    /// An aliased pattern as in `x @ { a = b @ { c, d }, ..}`
    AliasedPattern {
        alias: LocIdent,
        pattern: Box<LocPattern>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub struct LocPattern {
    pub pattern: Pattern,
    pub span: RawSpan,
}

/// A field pattern inside a record pattern. Every field can be annotated with a type, contracts or
/// with a default value.
#[derive(Debug, PartialEq, Clone)]
pub struct FieldPattern {
    /// The name of the matched field. For example, in `{..., foo = {bar, baz}, ...}`, the matched
    /// identifier is `foo`.
    pub matched_id: LocIdent,
    /// The potentital decoration of the pattern, such as a type annotation, contract annotations,
    /// or a default value.
    pub decoration: Field,
    /// The pattern on the right-hand side of the `=`. A pattern like `{foo, bar}`, without the `=`
    /// sign, is considered to be `{foo=foo, bar=bar}`. In this case, `pattern` will be
    /// [Pattern::Any].
    pub pattern: LocPattern,
    pub span: RawSpan,
}

/// The last match in a data structure pattern. This can either be a normal match, or an ellipsis
/// which can capture the rest of the data structure. The type parameter `P` is the type of the
/// pattern of the data structure: currently, ellipsis matches are only supported for record, but
/// we'll probably support them for arrays as well.
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
    /// If the pattern is open, i.e. if it ended with an ellipsis, capturing the rest or not.
    pub open: bool,
    /// If the pattern is open and the rest is captured, the capturing identifier is stored here.
    pub rest: Option<LocIdent>,
    pub span: RawSpan,
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
    pub fn check_matches(&self) -> Result<(), ParseError> {
        let mut matches = HashMap::new();

        for pat in self.patterns.iter() {
            let binding = pat.matched_id;
            let label = binding.label().to_owned();
            match matches.entry(label) {
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
}

impl FieldPattern {
    /// Convert this field pattern to a field binding with metadata. It's used to generate the
    /// record contract associated to a record pattern.
    pub fn as_record_binding(&self) -> (LocIdent, Field) {
        let mut decoration = self.decoration.clone();

        // If the inner pattern gives rise to a contract, add it the to the field decoration.
        decoration
            .metadata
            .annotation
            .contracts
            .extend(self.pattern.elaborate_contract());

        (self.matched_id, decoration)
    }
}

// We don't implement elaborate_contract for `FieldPattern`, which is of a slightly different
// nature (it's a part of a record pattern). Instead, we call to `FieldPattern::as_record_binding`,
// which takes care of elaborating to an appropriate record field.
pub trait ElaborateContract {
    /// Elaborate a contract from this pattern. The contract will check both the structure of the
    /// matched value (e.g. the presence of fields in a record) and incoporate user provided
    /// contracts and annotations, as well as default values.
    ///
    /// Some patterns don't give rise to any contract (e.g. `Any`), in which case this function
    /// returns `None`.
    fn elaborate_contract(&self) -> Option<LabeledType>;
}

impl ElaborateContract for Pattern {
    fn elaborate_contract(&self) -> Option<LabeledType> {
        match self {
            Pattern::Any(_) => None,
            Pattern::RecordPattern(pat) => pat.elaborate_contract(),
            Pattern::AliasedPattern { pattern, .. } => pattern.elaborate_contract(),
        }
    }
}

impl ElaborateContract for LocPattern {
    fn elaborate_contract(&self) -> Option<LabeledType> {
        self.pattern.elaborate_contract()
    }
}

impl ElaborateContract for RecordPattern {
    fn elaborate_contract(&self) -> Option<LabeledType> {
        let pos = TermPos::Original(self.span);

        let typ = Type {
            typ: TypeF::Flat(
                Term::Record(RecordData::new(
                    self.patterns
                        .iter()
                        .map(FieldPattern::as_record_binding)
                        .collect(),
                    RecordAttrs {
                        open: self.open,
                        ..Default::default()
                    },
                    None,
                ))
                .into(),
            ),
            pos,
        };

        Some(LabeledType {
            typ: typ.clone(),
            label: Label {
                typ: typ.into(),
                span: self.span,
                ..Default::default()
            },
        })
    }
}
