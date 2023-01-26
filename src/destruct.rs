//! In this module, you have the main structures used in the destructuring feature of nickel.
//! Also, there are implementation managing the generation of a contract from a pattern.

use crate::{
    identifier::Ident,
    label::Label,
    position::RawSpan,
    term::{
        record::{Field, RecordAttrs, RecordData},
        LabeledType, Term,
    },
    types::{TypeF, Types},
};

/// A match field in a `Destruct` pattern. Every field can be annotated with a type, with contracts
/// or with a default value.
#[derive(Debug, PartialEq, Clone)]
pub enum Match {
    /// `{..., a=b, ...}` will bind the field `a` of the record to variable `b`. Here, `a` is the
    /// first field of this variant and `b` the optional one. The last field can actualy be a
    /// nested destruct pattern.
    Assign(Ident, Field, (Option<Ident>, Destruct)),
    /// Simple binding. the `Ident` is bind to a variable with the same name.
    Simple(Ident, Field),
}

/// Last match field of a `Destruct`.
#[derive(Debug, PartialEq, Clone)]
pub enum LastMatch {
    /// The last field is a normal match. In this case the pattern is "closed" so every record
    /// fields should be matched.
    Match(Box<Match>),
    /// The pattern is "open" `, ..}`. Optionaly you can bind a record containing the remaining
    /// fields to an `Identifier` using the syntax `, ..y}`.
    Ellipsis(Option<Ident>),
}

/// A destructuring pattern without the `x @` part.
#[derive(Debug, PartialEq, Clone)]
pub enum Destruct {
    /// A record pattern, the only one implemented for now.
    Record {
        matches: Vec<Match>,
        open: bool,
        rest: Option<Ident>,
        span: RawSpan,
    },
    /// An array destructuring. Not implemented.
    Array { matches: Vec<Match>, span: RawSpan },
    /// An empty destructuring. In this case, the pattern is a clasical `let var = something in
    /// body` form.
    Empty,
}

impl Destruct {
    /// Generate the contract elaborated from this pattern.
    pub fn into_contract(self) -> LabeledType {
        let label = self.label();
        self.into_contract_with_lbl(label)
    }

    fn into_contract_with_lbl(self, label: Label) -> LabeledType {
        let is_open = self.is_open();

        LabeledType {
            types: Types(TypeF::Flat(
                Term::Record(RecordData::new(
                    self.inner().into_iter().map(Match::as_binding).collect(),
                    RecordAttrs { open: is_open },
                    None,
                ))
                .into(),
            )),
            label,
        }
    }

    /// Get the inner vector of `Matches` of the pattern. If `Empty` return a empty vector.
    pub fn inner(self) -> Vec<Match> {
        match self {
            Destruct::Record { matches, .. } | Destruct::Array { matches, .. } => matches,
            Destruct::Empty => vec![],
        }
    }

    // Generate a label for this `Destruct`. if `Empty`, return default label.
    fn label(&self) -> Label {
        match *self {
            Destruct::Record { span, .. } | Destruct::Array { span, .. } => Label {
                span,
                ..Default::default()
            },
            Destruct::Empty => Label::default(),
        }
    }

    /// Is this pattern open? Does it finish with `, ..}` form?
    pub fn is_open(&self) -> bool {
        matches!(self, Destruct::Record { open: true, .. })
    }

    /// check if the pattern is empty.
    pub fn is_empty(&self) -> bool {
        matches!(self, Destruct::Empty)
    }
}

impl Match {
    /// Convert the `Match` to a field binding with metadata. It's used to generate the record
    /// contract representing a record pattern destructuring.
    pub fn as_binding(self) -> (Ident, Field) {
        match self {
            Match::Assign(id, field, (_, Destruct::Empty)) | Match::Simple(id, field) => {
                (id, field)
            }

            // In this case we fuse spans of the `Ident` (LHS) with the destruct (RHS)
            // because we can have two cases:
            //
            // - extra field on the destructuring `d`
            // - missing field on the `id`
            Match::Assign(id, mut field, (_, destruct @ Destruct::Record { .. })) => {
                let mut label = destruct.label();
                label.span = RawSpan::fuse(id.pos.unwrap(), label.span).unwrap();
                field
                    .metadata
                    .annotation
                    .contracts
                    .push(destruct.into_contract_with_lbl(label));

                (id, field)
            }
            Match::Assign(_id, _m, (_, _d @ Destruct::Array { .. })) => unimplemented!(),
        }
    }
}
