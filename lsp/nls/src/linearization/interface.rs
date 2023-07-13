use std::collections::HashMap;

use nickel_lang_core::{identifier::Ident, typecheck::UnifType, types::Types};

use super::ItemId;

pub trait ResolutionState {}
/// Types are available as [nickel_lang_core::typecheck::UnifType] only during recording. They are
/// resolved after typechecking as [nickel_lang_core::types::Types]
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
#[derive(Debug, Clone, PartialEq)]
pub enum TermKind {
    Declaration {
        id: Ident,
        usages: Vec<ItemId>,
        value: ValueState,
        // This is the path to a bound variable. If we have
        // `let { a = {b = {c, ..}, ..}, ..} = ...`,  the `path` will
        // be `Some([a, b, c])`, and `ident` will be `c`.
        // If we have `let { a = {b = {c = somevar, ..}, ..}, ..} = ...`
        // instead, the `path` remains the same, but the ident will be `somevar`
        // If there is no pattern variable bound, the `path` is `None`
        path: Option<Vec<Ident>>,
    },
    Usage(UsageState),
    Record(HashMap<Ident, ItemId>),
    RecordField {
        ident: Ident,
        record: ItemId,
        usages: Vec<ItemId>,
        value: ValueState,
    },
    Types(Types),
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
