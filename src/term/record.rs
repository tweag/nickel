use super::{RichTerm, SealingKey};
use crate::{identifier::Ident, label::Label};
use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

#[derive(Debug, Default, Eq, PartialEq, Copy, Clone)]
pub struct RecordAttrs {
    pub open: bool,
}

impl RecordAttrs {
    pub fn merge(attrs1: RecordAttrs, attrs2: RecordAttrs) -> RecordAttrs {
        RecordAttrs {
            open: attrs1.open || attrs2.open,
        }
    }
}

/// Dependencies of a field or a thunk over the other recursive fields of a recursive record.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FieldDeps {
    /// The set of dependencies is fixed and has been computed. When attached to a thunk, an empty
    /// set of dependency means that the thunk isn't revertible, but standard.
    Known(Rc<HashSet<Ident>>),
    /// The thunk is revertible, but the set of dependencies hasn't been computed. In that case,
    /// the interpreter should be conservative and assume that any recursive references can appear
    /// in the content of the corresponding thunk.
    Unknown,
}

impl FieldDeps {
    /// Compute the union of two thunk dependencies. [`FieldDeps::Unknown`] can be see as the top
    /// element, meaning that if one of the two set of dependencies is [`FieldDeps::Unknown`], so
    /// is the result.
    pub fn union(self, other: Self) -> Self {
        match (self, other) {
            // If one of the field has unknown dependencies (understand: may depend on all the other
            // fields), then the resulting fields has unknown dependencies as well
            (FieldDeps::Unknown, _) | (_, FieldDeps::Unknown) => FieldDeps::Unknown,
            (FieldDeps::Known(deps1), FieldDeps::Known(deps2)) => {
                let union: HashSet<Ident> = deps1.union(&*deps2).cloned().collect();
                FieldDeps::Known(Rc::new(union))
            }
        }
    }

    /// Return an empty set of dependencies
    pub fn empty() -> Self {
        FieldDeps::Known(Rc::new(HashSet::new()))
    }

    /// Return `true` if the dependencies are known and are empty, or `false` otherwise.
    pub fn is_empty(&self) -> bool {
        matches!(self, FieldDeps::Known(deps) if deps.is_empty())
    }
}

impl From<HashSet<Ident>> for FieldDeps {
    fn from(set: HashSet<Ident>) -> Self {
        FieldDeps::Known(Rc::new(set))
    }
}

/// Store field interdependencies in a recursive record. Map each static and dynamic field to the
/// set of recursive fields that syntactically appears in their definition as free variables.
#[derive(Debug, Default, Eq, PartialEq, Clone)]
pub struct RecordDeps {
    /// Must have exactly the same keys as the static fields map of the recursive record.
    pub stat_fields: HashMap<Ident, FieldDeps>,
    /// Must have exactly the same length as the dynamic fields list of the recursive record.
    pub dyn_fields: Vec<FieldDeps>,
}

/// The base structure of a Nickel record.
///
/// Used to group together fields common to both the [super::Term::Record] and
/// [super::Term::RecRecord] terms.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct RecordData {
    /// Fields whose names are known statically.
    pub fields: HashMap<Ident, RichTerm>,
    /// Attributes which may be applied to a record.
    pub attrs: RecordAttrs,
    /// The hidden part of a record under a polymorphic contract.
    pub sealed_tail: Option<SealedTail>,
}

impl RecordData {
    pub fn new(
        fields: HashMap<Ident, RichTerm>,
        attrs: RecordAttrs,
        sealed_tail: Option<SealedTail>,
    ) -> Self {
        RecordData {
            fields,
            attrs,
            sealed_tail,
        }
    }

    /// A record with no fields and the default set of attributes.
    pub fn empty() -> Self {
        Default::default()
    }

    /// A record with the provided fields & the default set of attributes.
    pub fn with_fields(fields: HashMap<Ident, RichTerm>) -> Self {
        let attrs = Default::default();
        let sealed_tail = Default::default();
        RecordData {
            fields,
            attrs,
            sealed_tail,
        }
    }

    /// Returns the record resulting from applying the provided function
    /// to each field.
    ///
    /// Note that `f` is taken as `mut` in order to allow it to mutate
    /// external state while iterating.
    pub fn map_fields<F>(self, mut f: F) -> Self
    where
        F: FnMut(Ident, RichTerm) -> RichTerm,
    {
        let fields = self
            .fields
            .into_iter()
            .map(|(id, t)| (id, f(id, t)))
            .collect();
        RecordData { fields, ..self }
    }
}

/// The sealed tail of a Nickel record under a polymorphic contract.
///
/// Note that access to the enclosed term must only be allowed when a matching sealing key is
/// provided. If this is not enforced it will lead to parametricity violations.
#[derive(Clone, Debug, PartialEq)]
pub struct SealedTail {
    /// The key with which the tail is sealed.
    sealing_key: SealingKey,
    /// The label to which blame will be attributed if code tries to
    /// interact with the sealed tail in any way.
    pub label: Label,
    /// The term which is sealed.
    term: RichTerm,
}

impl SealedTail {
    pub fn new(sealing_key: SealingKey, label: Label, term: RichTerm) -> Self {
        Self {
            sealing_key,
            label,
            term,
        }
    }

    /// Returns the sealed term if the key matches, otherwise returns None.
    pub fn unseal(&self, key: &SealingKey) -> Option<&RichTerm> {
        if key == &self.sealing_key {
            Some(&self.term)
        } else {
            None
        }
    }
}
