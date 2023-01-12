use std::{collections::HashMap, mem};

use codespan::FileId;
use log::debug;
use nickel_lang::{
    cache::Cache,
    identifier::Ident,
    term::{MetaValue, RichTerm, Term},
    typecheck::{linearization::LinearizationState, UnifType},
    types::TypeF,
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
    pub cache: &'a Cache,
}

impl<'b> Building<'b> {
    pub(super) fn push(&mut self, item: LinearizationItem<Unresolved>) {
        self.linearization.push(item)
    }

    fn get_item(&self, id: ItemId) -> Option<&LinearizationItem<Unresolved>> {
        self.linearization.get(id.index)
    }

    fn get_item_mut(&mut self, id: ItemId) -> Option<&mut LinearizationItem<Unresolved>> {
        self.linearization.get_mut(id.index)
    }

    fn get_item_kind_with_id(
        &self,
        current_file: FileId,
        id: ItemId,
    ) -> Option<(&ItemId, &TermKind)> {
        if current_file == id.file_id {
            // This usage references an item in the file we're currently linearizing
            let item = self.get_item(id)?;
            Some((&item.id, &item.kind))
        } else {
            // This usage references an item in another file (that has already been linearized)
            let item = self
                .lin_cache
                .get(&id.file_id)?
                .get_item(id, self.lin_cache)?;
            Some((&item.id, &item.kind))
        }
    }

    fn get_item_kind(&self, current_file: FileId, id: ItemId) -> Option<&TermKind> {
        let (_, kind) = self.get_item_kind_with_id(current_file, id)?;
        Some(kind)
    }

    fn get_item_kind_mut(&mut self, current_file: FileId, id: ItemId) -> Option<&mut TermKind> {
        if current_file == id.file_id {
            // This usage references an item in the file we're currently linearizing
            let item = self.get_item_mut(id)?;
            Some(&mut item.kind)
        } else {
            // This usage references an item in another file (that has already been linearized)
            let item = self.lin_cache.get_mut(&id.file_id)?.get_item_mut(id)?;
            Some(&mut item.kind)
        }
    }

    pub(super) fn add_usage(&mut self, current_file: FileId, decl: ItemId, usage: ItemId) {
        match self
            .get_item_kind_mut(current_file, decl)
            .expect("Could not find parent")
        {
            TermKind::Record(_) | TermKind::Structure | TermKind::Usage(_) => unreachable!(),
            TermKind::Declaration(_, ref mut usages, _)
            | TermKind::RecordField { ref mut usages, .. } => usages.push(usage),
        };
    }

    pub(super) fn inform_declaration(
        &mut self,
        current_file: FileId,
        declaration: ItemId,
        value: ItemId,
    ) {
        let kind = self.get_item_kind_mut(current_file, declaration);
        if let Some(TermKind::Declaration(_, _, value_state)) = kind {
            *value_state = ValueState::Known(value)
        }
    }

    pub(super) fn register_fields(
        &mut self,
        current_file: FileId,
        record_fields: &HashMap<Ident, RichTerm>,
        record: ItemId,
        env: &mut Environment,
    ) {
        for (ident, value) in record_fields.iter() {
            let id = ItemId {
                file_id: current_file,
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
            self.add_record_field(current_file, record, (*ident, id))
        }
    }

    pub(super) fn add_record_field(
        &mut self,
        current_file: FileId,
        record: ItemId,
        (field_ident, reference_id): (Ident, ItemId),
    ) {
        match self
            .get_item_kind_mut(current_file, record)
            .expect("Could not find record")
        {
            TermKind::Record(ref mut fields) => {
                fields.insert(field_ident, reference_id);
            }
            _ => unreachable!(),
        }
    }

    pub(super) fn resolve_reference<'a>(
        &'a self,
        current_file: FileId,
        item: &'a TermKind,
    ) -> Option<&'a TermKind> {
        match item {
            // if declaration is a record field, resolve its value
            TermKind::RecordField { value, .. } => {
                debug!("parent referenced a record field {:?}", value);
                value
                    // retrieve record
                    .as_option()
                    .and_then(|value_index| self.get_item_kind(current_file, value_index))
            }
            // if declaration is a let binding resolve its value
            TermKind::Declaration(_, _, ValueState::Known(value)) => {
                self.get_item_kind(current_file, *value)
            }

            TermKind::Usage(UsageState::Resolved(pointed)) => {
                let kind = self.get_item_kind(current_file, *pointed)?;
                self.resolve_reference(current_file, kind)
            }
            // if something else was referenced, stop.
            _ => Some(item),
        }
    }

    /// [`resolve_record_references`] tries to resolve the references passed to it, and
    /// returns the ids of the items it couldn't resolve.
    pub(super) fn resolve_record_references(
        &mut self,
        current_file: FileId,
        mut defers: Vec<(ItemId, ItemId, Ident)>,
    ) -> Vec<(ItemId, ItemId, Ident)> {
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
            let parent_referenced = self.get_item_kind(current_file, *parent_accessor_id);
            if let Some(TermKind::Usage(UsageState::Deferred { .. })) = parent_referenced {
                debug!("parent references deferred usage");
                unresolved.push(deferred);
                continue;
            }

            // load the parent referenced declaration (i.e.: a declaration or record field term)
            let parent_declaration = parent_referenced.and_then(|parent_usage_value| {
                self.resolve_reference(current_file, parent_usage_value)
            });

            if let Some(TermKind::Usage(UsageState::Deferred { .. })) = parent_declaration {
                debug!("parent references deferred usage");
                unresolved.push(deferred);
                continue;
            }

            let referenced_declaration = parent_declaration
                // resolve indirection by following the usage
                .and_then(|parent_declaration| {
                    self.resolve_reference(current_file, parent_declaration)
                })
                // get record field
                .and_then(|parent_declaration| match &parent_declaration {
                    TermKind::Record(fields) => {
                        fields.get(child_ident).and_then(|child_declaration_id| {
                            self.get_item_kind_with_id(current_file, *child_declaration_id)
                        })
                    }
                    _ => None,
                });

            let referenced_declaration =
                referenced_declaration.and_then(|(id, referenced)| match &referenced {
                    TermKind::Usage(UsageState::Resolved(pointed)) => {
                        self.get_item_kind_with_id(current_file, *pointed)
                    }
                    TermKind::RecordField { value, .. } => value
                        // retrieve record
                        .as_option()
                        .and_then(|value_index| self.get_item_kind(current_file, value_index))
                        // retrieve field
                        .and_then(|record| match &record {
                            TermKind::Record(fields) => {
                                debug!(
                                    "parent referenced a nested record indirectly`: {:?}",
                                    fields
                                );
                                fields.get(child_ident).and_then(|accessor_id| {
                                    self.get_item_kind_with_id(current_file, *accessor_id)
                                })
                            }
                            TermKind::Usage(UsageState::Resolved(pointed)) => {
                                self.get_item_kind_with_id(current_file, *pointed)
                            }
                            _ => None,
                        })
                        .or(Some((id, referenced))),

                    _ => Some((id, referenced)),
                });

            let referenced_id = referenced_declaration.map(|(id, _)| *id);

            debug!(
                "Associating child {} to value {:?}",
                child_ident, referenced_declaration
            );

            {
                let child = self.get_item_kind_mut(current_file, *child_item).unwrap();
                *child = TermKind::Usage(UsageState::from(referenced_id));
            }

            if let Some(referenced_id) = referenced_id {
                self.add_usage(current_file, referenced_id, *child_item);
            }
        }

        if defers.is_empty() && !unresolved.is_empty() {
            defers = mem::take(&mut unresolved);
            defers
        } else {
            Vec::new()
        }
    }

    pub(super) fn id_gen(&self) -> IdGen {
        IdGen::new(self.linearization.len())
    }
}

impl<'a> LinearizationState for Building<'a> {}
