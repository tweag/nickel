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
    pattern: Pattern,
    span: RawSpan,
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

    /// Generate the contract elaborated from this pattern.
    pub fn into_contract(self) -> LabeledType {
        let span = self.span;
        self.into_contract_with_span(span)
    }

    fn into_contract_with_span(self, span: RawSpan) -> LabeledType {
        let pos = TermPos::Original(span);
        let typ = Type {
            typ: TypeF::Flat(
                Term::Record(RecordData::new(
                    self.patterns
                        .into_iter()
                        .map(FieldPattern::as_binding)
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
        LabeledType {
            typ: typ.clone(),
            label: Label {
                typ: typ.into(),
                span,
                ..Default::default()
            },
        }
    }
}

impl FieldPattern {
    /// Convert this field pattern to a field binding with metadata. It's used to generate the
    /// record contract representing a record pattern destructuring.
    pub fn as_binding(self) -> (LocIdent, Field) {
        match self.pattern.pattern {
            Pattern::Any(_) => (self.matched_id, self.decoration),
            // In this case we fuse spans of the `Ident` (LHS) with the destruct (RHS)
            // because we can have two cases:
            //
            // - extra field on the destructuring `d`
            // - missing field on the `id`
            Pattern::RecordPattern(pattern) | Pattern::AliasedPattern { pattern, .. } => {
                let span = RawSpan::fuse(id.pos.unwrap(), pattern.span).unwrap();
                field
                    .metadata
                    .annotation
                    .contracts
                    .push(pattern.into_contract_with_span(span));

                (id, field)
            }
        }
    }

    /// Returns info about each variable bound in a particular pattern.
    /// It also tells the "path" to the bound variable; this is just the
    /// record field names traversed to get to a pattern.
    pub fn to_flattened_bindings(&self) -> Vec<(Vec<LocIdent>, LocIdent, Field)> {
        fn get_span(id: &LocIdent, pattern: &RecordPattern) -> RawSpan {
            RawSpan::fuse(id.pos.unwrap(), pattern.span).unwrap()
        }

        fn flatten_matches(
            id: &LocIdent,
            matches: &[FieldPattern],
        ) -> Vec<(Vec<LocIdent>, LocIdent, Field)> {
            matches
                .iter()
                .flat_map(|m| m.to_flattened_bindings())
                .map(|(mut path, bind, field)| {
                    path.push(*id);
                    (path, bind, field)
                })
                .collect()
        }

        match self {
            FieldPattern::Simple(id, field) => vec![(vec![*id], *id, field.clone())],
            FieldPattern::Assign(id, field, Pattern::Any(bind_id)) => {
                vec![(vec![*id], *bind_id, field.clone())]
            }
            FieldPattern::Assign(
                id,
                field,
                Pattern::RecordPattern(
                    ref pattern @ RecordPattern {
                        patterns: ref matches,
                        ..
                    },
                ),
            ) => {
                let span = get_span(id, pattern);
                let pattern = pattern.clone();
                let mut field = field.clone();
                field
                    .metadata
                    .annotation
                    .contracts
                    .push(pattern.into_contract_with_span(span));

                flatten_matches(id, matches)
            }
            FieldPattern::Assign(
                id,
                field,
                Pattern::AliasedPattern {
                    alias: bind_id,
                    pattern:
                        ref pattern @ RecordPattern {
                            patterns: ref matches,
                            ..
                        },
                },
            ) => {
                let span = get_span(id, pattern);
                let pattern = pattern.clone();
                let mut field = field.clone();
                field
                    .metadata
                    .annotation
                    .contracts
                    .push(pattern.into_contract_with_span(span));

                let mut flattened = flatten_matches(id, matches);
                flattened.push((vec![*id], *bind_id, field));
                flattened
            }
        }
    }
}
