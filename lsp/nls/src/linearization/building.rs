use std::{collections::HashMap, mem};

use codespan::FileId;
use log::debug;
use nickel_lang::{
    identifier::Ident,
    term::{MetaValue, RichTerm, Term},
    typecheck::{linearization::LinearizationState, UnifType},
    types::TypeF,
};

use crate::linearization::interface::{TermKind, UsageState};

use super::{
    interface::{Unresolved, ValueState},
    Environment, IdGen, LinearizationItem,
};

/// A concrete [LinearizationState]
/// Holds any inner datatype that can be used as stable resource
/// while recording terms.
#[derive(Default)]
pub struct Building {
    pub linearization: Vec<LinearizationItem<Unresolved>>,
}

pub type ID = usize;

impl Building {
    pub(super) fn push(&mut self, item: LinearizationItem<Unresolved>) {
        self.linearization.push(item)
    }

    pub(super) fn add_usage(&mut self, decl: usize, usage: usize) {
        match self
            .linearization
            .get_mut(decl)
            .expect("Could not find parent")
            .kind
        {
            TermKind::Structure => unreachable!(),
            TermKind::Usage(_) => unreachable!(),
            TermKind::Record(_) => unreachable!(),
            TermKind::Declaration(_, ref mut usages, _)
            | TermKind::RecordField { ref mut usages, .. } => usages.push(usage),
        };
    }

    pub(super) fn inform_declaration(&mut self, declaration: ID, value: ID) {
        if let Some(LinearizationItem {
            kind: TermKind::Declaration(_, _, value_state),
            ..
        }) = self.linearization.get_mut(declaration)
        {
            *value_state = ValueState::Known(value)
        }
    }

    pub(super) fn register_fields(
        &mut self,
        record_fields: &HashMap<Ident, RichTerm>,
        record: usize,
        env: &mut Environment,
        file: FileId,
    ) {
        for (ident, value) in record_fields.iter() {
            let id = self.id_gen().get_and_advance();
            self.push(LinearizationItem {
                env: env.clone(),
                id,
                pos: ident.pos,
                // temporary, the actual type is resolved later and the item retyped
                ty: UnifType::Concrete(TypeF::Dyn),
                kind: TermKind::RecordField {
                    record,
                    ident: *ident,
                    usages: Vec::new(),
                    value: ValueState::Unknown,
                },
                meta: match value.term.as_ref() {
                    Term::MetaValue(meta @ MetaValue { .. }) => Some(MetaValue {
                        value: None,
                        ..meta.clone()
                    }),
                    _ => None,
                },
            });
            let key = (*ident, file);
            env.insert(key, id);
            self.add_record_field(record, (*ident, id))
        }
    }

    pub(super) fn add_record_field(
        &mut self,
        record: usize,
        (field_ident, reference_id): (Ident, usize),
    ) {
        match self
            .linearization
            .get_mut(record)
            .expect("Could not find record")
            .kind
        {
            TermKind::Record(ref mut fields) => {
                fields.insert(field_ident, reference_id);
            }
            _ => unreachable!(),
        }
    }

    pub(super) fn resolve_reference<'a>(
        &'a self,
        item: &'a LinearizationItem<UnifType>,
    ) -> Option<&'a LinearizationItem<UnifType>> {
        // if item is a usage, resolve the usage first
        match item.kind {
            TermKind::Usage(UsageState::Resolved(pointed)) => self.linearization.get(pointed),
            _ => Some(item),
        }
        // load referenced value, either from record field or declaration
        .and_then(|item_pointer| {
            match &item_pointer.kind {
                // if declaration is a record field, resolve its value
                TermKind::RecordField { value, .. } => {
                    debug!("parent referenced a record field {:?}", value);
                    value
                        // retrieve record
                        .as_option()
                        .and_then(|value_index| self.linearization.get(value_index))
                }
                // if declaration is a let binding resolve its value
                TermKind::Declaration(_, _, ValueState::Known(value)) => {
                    self.linearization.get(*value)
                }

                // if something else was referenced, stop.
                _ => Some(item_pointer),
            }
        })
    }

    pub(super) fn resolve_record_references(&mut self, mut defers: Vec<(usize, usize, Ident)>) {
        let mut unresolved: Vec<(usize, usize, Ident)> = Vec::new();

        while let Some(deferred) = defers.pop() {
            // child_item: current deferred usage item
            //       i.e.: root.<child>
            // parent_accessor_id: id of the parent usage
            //               i.e.: <parent>.child
            // child_ident: identifier the child item references
            let (child_item, parent_accessor_id, child_ident) = &deferred;
            // resolve the value referenced by the parent accessor element
            // get the parent accessor, and read its resolved reference
            let parent_referenced = self.linearization.get(*parent_accessor_id);

            if let Some(LinearizationItem {
                kind: TermKind::Usage(UsageState::Deferred { .. }),
                ..
            }) = parent_referenced
            {
                debug!("parent references deferred usage");
                unresolved.push(deferred);
                continue;
            }

            // load the parent referenced declaration (i.e.: a declaration or record field term)
            let parent_declaration = parent_referenced
                .and_then(|parent_usage_value| self.resolve_reference(parent_usage_value));

            if let Some(LinearizationItem {
                kind: TermKind::Usage(UsageState::Deferred { .. }),
                ..
            }) = parent_declaration
            {
                debug!("parent references deferred usage");
                unresolved.push(deferred);
                continue;
            }

            let referenced_declaration = parent_declaration
                // resolve indirection by following the usage
                .and_then(|parent_declaration| self.resolve_reference(parent_declaration))
                // get record field
                .and_then(|parent_declaration| match &parent_declaration.kind {
                    TermKind::Record(fields) => {
                        fields.get(child_ident).and_then(|child_declaration_id| {
                            self.linearization.get(*child_declaration_id)
                        })
                    }
                    _ => None,
                });

            let referenced_declaration =
                referenced_declaration.and_then(|referenced| match &referenced.kind {
                    TermKind::Usage(UsageState::Resolved(pointed)) => {
                        self.linearization.get(*pointed)
                    }
                    TermKind::RecordField { value, .. } => value
                        // retrieve record
                        .as_option()
                        .and_then(|value_index| self.linearization.get(value_index))
                        // retrieve field
                        .and_then(|record| match &record.kind {
                            TermKind::Record(fields) => {
                                debug!(
                                    "parent referenced a nested record indirectly`: {:?}",
                                    fields
                                );
                                fields
                                    .get(child_ident)
                                    .and_then(|accessor_id| self.linearization.get(*accessor_id))
                            }
                            TermKind::Usage(UsageState::Resolved(pointed)) => {
                                self.linearization.get(*pointed)
                            }
                            _ => None,
                        })
                        .or(Some(referenced)),

                    _ => Some(referenced),
                });

            let referenced_id = referenced_declaration.map(|reference| reference.id);

            debug!(
                "Associating child {} to value {:?}",
                child_ident, referenced_declaration
            );

            {
                let child: &mut LinearizationItem<UnifType> =
                    self.linearization.get_mut(*child_item).unwrap();
                child.kind = TermKind::Usage(UsageState::from(referenced_id));
            }

            if let Some(referenced_id) = referenced_id {
                self.add_usage(referenced_id, *child_item);
            }

            if defers.is_empty() && !unresolved.is_empty() {
                debug!("unresolved references: {:?}", unresolved);
                defers = mem::take(&mut unresolved);
            }
        }
    }

    pub(super) fn id_gen(&self) -> IdGen {
        IdGen::new(self.linearization.len())
    }
}

impl LinearizationState for Building {}
