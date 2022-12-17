use std::{collections::HashMap, mem};

use codespan::FileId;
use log::debug;
use nickel_lang::{
    identifier::Ident,
    term::{MetaValue, RichTerm, Term},
    typecheck::{linearization::LinearizationState, UnifType},
    types::TypeF, cache::CachedTerm,
};

use crate::linearization::interface::{TermKind, UsageState};

use super::{
    completed::Completed,
    interface::{Unresolved, ValueState},
    Environment, IdGen, ItemId, LinearizationItem,
};

/// A concrete [LinearizationState]
/// Holds any inner datatype that can be used as stable resource
/// while recording terms.
pub struct Building<'a> {
    pub linearization: Vec<LinearizationItem<Unresolved>>,
    pub lin_cache: &'a mut HashMap<FileId, Completed>,
    pub terms: &'a HashMap<FileId, CachedTerm>,
}

impl<'b> Building<'b> {
    pub(super) fn push(&mut self, item: LinearizationItem<Unresolved>) {
        self.linearization.push(item)
    }

    fn get_item_kind_mut(&mut self, file: FileId, id: ItemId) -> Option<&mut TermKind> {
        if file == id.file_id {
            // This usage references an item in the file we're currently linearizing
            let item = self.linearization.get_mut(id.index)?;
            Some(&mut item.kind)
        } else {
            // This usage references an item in another file (that has already been linearized)
            let item = self.lin_cache.get_mut(&id.file_id)?.get_item_mut(id)?;
            Some(&mut item.kind)
        }
    }

    pub(super) fn add_usage(&mut self, file: FileId, decl: ItemId, usage: ItemId) {
        match self
            .get_item_kind_mut(file, decl)
            .expect("Could not find parent")
        {
            TermKind::Record(_) | TermKind::Structure | TermKind::Usage(_) => unreachable!(),
            TermKind::Declaration(_, ref mut usages, _)
            | TermKind::RecordField { ref mut usages, .. } => usages.push(usage),
        };
    }

    pub(super) fn inform_declaration(&mut self, file: FileId, declaration: ItemId, value: ItemId) {
        let kind = self.get_item_kind_mut(file, declaration);
        if let Some(TermKind::Declaration(_, _, value_state)) = kind {
            *value_state = ValueState::Known(value)
        }
    }

    pub(super) fn register_fields(
        &mut self,
        record_fields: &HashMap<Ident, RichTerm>,
        record: ItemId,
        env: &mut Environment,
        file: FileId,
    ) {
        for (ident, value) in record_fields.iter() {
            let id = ItemId {
                file_id: file,
                index: self.id_gen().get_and_advance(),
            };
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
            let key = *ident;
            env.insert(key, id);
            self.add_record_field(record, (*ident, id))
        }
    }

    pub(super) fn add_record_field(
        &mut self,
        record: ItemId,
        (field_ident, reference_id): (Ident, ItemId),
    ) {
        match self
            .linearization
            .get_mut(record.index)
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
            TermKind::Usage(UsageState::Resolved(pointed)) => self.linearization.get(pointed.index),
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
                        .and_then(|value_index| self.linearization.get(value_index.index))
                }
                // if declaration is a let binding resolve its value
                TermKind::Declaration(_, _, ValueState::Known(value)) => {
                    self.linearization.get(value.index)
                }

                // if something else was referenced, stop.
                _ => Some(item_pointer),
            }
        })
    }

    pub(super) fn resolve_record_references(
        &mut self,
        file: FileId,
        mut defers: Vec<(ItemId, ItemId, Ident)>,
    ) {
        let mut unresolved: Vec<(ItemId, ItemId, Ident)> = Vec::new();

        while let Some(deferred) = defers.pop() {
            // child_item: current deferred usage item
            //       i.e.: root.<child>
            // parent_accessor_id: id of the parent usage
            //               i.e.: <parent>.child
            // child_ident: identifier the child item references
            let (child_item, parent_accessor_id, child_ident) = &deferred;
            // resolve the value referenced by the parent accessor element
            // get the parent accessor, and read its resolved reference
            let parent_referenced = self.linearization.get(parent_accessor_id.index);

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
                            self.linearization.get(child_declaration_id.index)
                        })
                    }
                    _ => None,
                });

            let referenced_declaration =
                referenced_declaration.and_then(|referenced| match &referenced.kind {
                    TermKind::Usage(UsageState::Resolved(pointed)) => {
                        self.linearization.get(pointed.index)
                    }
                    TermKind::RecordField { value, .. } => value
                        // retrieve record
                        .as_option()
                        .and_then(|value_index| self.linearization.get(value_index.index))
                        // retrieve field
                        .and_then(|record| match &record.kind {
                            TermKind::Record(fields) => {
                                debug!(
                                    "parent referenced a nested record indirectly`: {:?}",
                                    fields
                                );
                                fields.get(child_ident).and_then(|accessor_id| {
                                    self.linearization.get(accessor_id.index)
                                })
                            }
                            TermKind::Usage(UsageState::Resolved(pointed)) => {
                                self.linearization.get(pointed.index)
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
                    self.linearization.get_mut(child_item.index).unwrap();
                child.kind = TermKind::Usage(UsageState::from(referenced_id));
            }

            if let Some(referenced_id) = referenced_id {
                self.add_usage(file, referenced_id, *child_item);
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

impl<'a> LinearizationState for Building<'a> {}
