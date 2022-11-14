use std::collections::HashMap;

use nickel_lang::{identifier::Ident, typecheck::UnifType, types::Types};

use super::building::ID;

pub trait ResolutionState {}
/// Types are available as [TypeWrapper] only during recording
/// They are resolved after typechecking has collected all terms into concrete
/// [Types]
pub type Unresolved = UnifType;
impl ResolutionState for Unresolved {}

/// When resolved a concrete [Types] is known
pub type Resolved = Types;
impl ResolutionState for Resolved {}

/// Abstact term kinds.
/// Currently tracks
/// 1. Declarations
/// 2. Usages
/// 3. Records, listing their fields
/// 4. wildcard (Structure) for any other kind of term.
/// Can be extended later to represent Contracts, Records, etc.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TermKind {
    Declaration(Ident, Vec<ID>, ValueState),
    Usage(UsageState),
    Record(HashMap<Ident, ID>),
    RecordField {
        ident: Ident,
        record: ID,
        usages: Vec<ID>,
        value: ValueState,
    },
    Structure,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueState {
    Unknown,
    Known(ID),
}

impl ValueState {
    pub fn as_option(&self) -> Option<ID> {
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
    Resolved(ID),
    Deferred { parent: ID, child: Ident },
}

impl From<Option<ID>> for UsageState {
    fn from(option: Option<ID>) -> Self {
        match option {
            Some(id) => UsageState::Resolved(id),
            None => UsageState::Unbound,
        }
    }
}
