//! In this module, you have the main structures used in the destructuring feature of nickel.
//! Also, there are implementation managing the generation of a contract from a pattern.

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
pub enum FieldPattern {
    /// An assignment match like `{ ..., a = b, ... }`
    Ident(LocIdent),
    /// A nested record pattern like `{ ..., a = { b, c }, ... }`
    RecordPattern(RecordPattern),
    /// An aliased nested record pattern like `{ ..., a = b @ { c, d }, ... }`
    AliasedRecordPattern {
        alias: LocIdent,
        pattern: RecordPattern,
    },
}

/// A match field in a `Destruct` pattern. Every field can be annotated with a type, with contracts
/// or with a default value.
#[derive(Debug, PartialEq, Clone)]
pub enum Match {
    /// `{..., a=b, ...}` will bind the field `a` of the record to variable `b`. Here, `a` is the
    /// first field of this variant and `b` the optional one. The last field can actualy be a
    /// nested destruct pattern.
    Assign(LocIdent, Field, FieldPattern),
    /// Simple binding. the `Ident` is bind to a variable with the same name.
    Simple(LocIdent, Field),
}

impl Match {
    fn ident(&self) -> LocIdent {
        match self {
            Match::Assign(ident, ..) | Match::Simple(ident, ..) => *ident,
        }
    }
}

/// Last match field of a `Destruct`.
#[derive(Debug, PartialEq, Clone)]
pub enum LastMatch {
    /// The last field is a normal match. In this case the pattern is "closed" so every record
    /// fields should be matched.
    Match(Box<Match>),
    /// The pattern is "open" `, ..}`. Optionally you can bind a record containing the remaining
    /// fields to an `Identifier` using the syntax `, ..y}`.
    Ellipsis(Option<LocIdent>),
}

/// A destructured record pattern
#[derive(Debug, PartialEq, Clone)]
pub struct RecordPattern {
    pub matches: Vec<Match>,
    pub open: bool,
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

        for m in self.matches.iter() {
            let binding = m.ident();
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
        let label = self.label();
        self.into_contract_with_lbl(label)
    }

    fn into_contract_with_lbl(self, label: Label) -> LabeledType {
        let is_open = self.is_open();
        let pos = TermPos::Original(self.span);
        LabeledType {
            typ: Type {
                typ: TypeF::Flat(
                    Term::Record(RecordData::new(
                        self.inner().into_iter().map(Match::as_binding).collect(),
                        RecordAttrs { open: is_open },
                        None,
                    ))
                    .into(),
                ),
                pos,
            },
            label,
        }
    }

    /// Get the inner vector of `Matches` of the pattern. If `Empty` return a empty vector.
    pub fn inner(self) -> Vec<Match> {
        self.matches
    }

    // Generate a label for this `Destruct`. if `Empty`, return default label.
    fn label(&self) -> Label {
        Label {
            span: self.span,
            ..Default::default()
        }
    }

    /// Is this pattern open? Does it finish with `, ..}` form?
    pub fn is_open(&self) -> bool {
        self.open
    }
}

impl Match {
    /// Convert the `Match` to a field binding with metadata. It's used to generate the record
    /// contract representing a record pattern destructuring.
    pub fn as_binding(self) -> (LocIdent, Field) {
        match self {
            Match::Assign(id, field, FieldPattern::Ident(_)) | Match::Simple(id, field) => {
                (id, field)
            }

            // In this case we fuse spans of the `Ident` (LHS) with the destruct (RHS)
            // because we can have two cases:
            //
            // - extra field on the destructuring `d`
            // - missing field on the `id`
            Match::Assign(
                id,
                mut field,
                FieldPattern::RecordPattern(pattern)
                | FieldPattern::AliasedRecordPattern { pattern, .. },
            ) => {
                let mut label = pattern.label();
                label.span = RawSpan::fuse(id.pos.unwrap(), label.span).unwrap();
                field
                    .metadata
                    .annotation
                    .contracts
                    .push(pattern.into_contract_with_lbl(label));

                (id, field)
            }
        }
    }

    /// Returns info about each variable bound in a particular pattern.
    /// It also tells the "path" to the bound variable; this is just the
    /// record field names traversed to get to a pattern.
    pub fn as_flattened_bindings(self) -> Vec<(Vec<LocIdent>, Option<LocIdent>, Field)> {
        fn get_label(id: &LocIdent, pattern: &RecordPattern) -> Label {
            let mut label = pattern.label();
            label.span = RawSpan::fuse(id.pos.unwrap(), label.span).unwrap();
            label
        }

        fn flatten_matches(
            id: &LocIdent,
            matches: &[Match],
        ) -> Vec<(Vec<LocIdent>, Option<LocIdent>, Field)> {
            matches
                .iter()
                .flat_map(|m| m.clone().as_flattened_bindings())
                .map(|(mut path, bind, field)| {
                    path.push(*id);
                    (path, bind, field)
                })
                .collect()
        }

        match self {
            Match::Simple(id, field) => vec![(vec![id], None, field)],
            Match::Assign(id, field, FieldPattern::Ident(bind_id)) => {
                vec![(vec![id], Some(bind_id), field)]
            }
            Match::Assign(
                id,
                mut field,
                FieldPattern::RecordPattern(ref pattern @ RecordPattern { ref matches, .. }),
            ) => {
                let label = get_label(&id, pattern);
                let pattern = pattern.clone();
                field
                    .metadata
                    .annotation
                    .contracts
                    .push(pattern.into_contract_with_lbl(label));

                flatten_matches(&id, matches)
            }
            Match::Assign(
                id,
                mut field,
                FieldPattern::AliasedRecordPattern {
                    alias: bind_id,
                    pattern: ref pattern @ RecordPattern { ref matches, .. },
                },
            ) => {
                let label = get_label(&id, pattern);
                let pattern = pattern.clone();
                field
                    .metadata
                    .annotation
                    .contracts
                    .push(pattern.into_contract_with_lbl(label));

                let mut flattened = flatten_matches(&id, matches);
                flattened.push((vec![id], Some(bind_id), field));
                flattened
            }
        }
    }
}
