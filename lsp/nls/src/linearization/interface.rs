use std::collections::HashMap;

use nickel_lang::{identifier::Ident, typecheck::UnifType, types::Types};

use super::ItemId;

pub trait ResolutionState {}
/// Types are available as [TypeWrapper] only during recording
/// They are resolved after typechecking has collected all terms into concrete
/// [Types]
pub type Unresolved = UnifType;
impl ResolutionState for Unresolved {}

/// When resolved a concrete [Types] is known
pub type Resolved = Types;
impl ResolutionState for Resolved {}

/// Abstract term kinds.
/// Currently tracks
/// 1. Declarations
/// 2. Usages
/// 3. Records, listing their fields
/// 4. wildcard (Structure) for any other kind of term.
/// Can be extended later to represent Contracts, Records, etc.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TermKind {
    Declaration(
        Ident,
        Vec<ItemId>,
        ValueState,
        /* pattern binding? */ Option<Ident>,
    ),
    Usage(UsageState),
    Record(HashMap<Ident, ItemId>),
    RecordField {
        ident: Ident,
        record: ItemId,
        usages: Vec<ItemId>,
        value: ValueState,
    },
    Structure,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueState {
    Unknown,
    Known(ItemId),
}

impl ValueState {
    pub fn as_option(&self) -> Option<ItemId> {
        match self {
            ValueState::Unknown => None,
            ValueState::Known(value) => Some(*value),
        }
    }
}
/// Some usages cannot be fully resolved in a first pass (i.e. recursive record fields)
/// In these cases we defer the resolution to a second pass during linearization
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UsageState {
    Unbound,
    Resolved(ItemId),
    Deferred { parent: ItemId, child: Ident },
}

impl From<Option<ItemId>> for UsageState {
    fn from(option: Option<ItemId>) -> Self {
        match option {
            Some(id) => UsageState::Resolved(id),
            None => UsageState::Unbound,
        }
    }
}
