use std::{collections::HashMap, hash::Hash};

use codespan::FileId;
use nickel_lang_core::{
    position::{RawPos, TermPos},
    term::{record::FieldMetadata, SharedTerm, Term},
};

use super::{
    interface::{Resolved, TermKind, UsageState, ValueState},
    ItemId, LinRegistry, LinearizationItem,
};

#[derive(Clone, Debug)]
struct SharedTermPtr(SharedTerm);

impl PartialEq for SharedTermPtr {
    fn eq(&self, other: &Self) -> bool {
        SharedTerm::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for SharedTermPtr {}

impl Hash for SharedTermPtr {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self.0.as_ref() as *const Term).hash(state);
    }
}

#[derive(Debug, Default, Clone)]
pub struct Completed {
    pub linearization: Vec<LinearizationItem<Resolved>>,
    pub import_locations: HashMap<FileId, TermPos>,
    id_to_index: HashMap<ItemId, usize>,
}

impl Completed {
    pub fn new(
        linearization: Vec<LinearizationItem<Resolved>>,
        id_to_index: HashMap<ItemId, usize>,
        import_locations: HashMap<FileId, TermPos>,
    ) -> Self {
        Self {
            linearization,
            import_locations,
            id_to_index,
        }
    }

    /// Returns the closest item to the left (if any) and to the right (if any) of
    /// a specified item. The "closeness" metric in this context is just the source
    /// position.
    pub fn get_items_adjacent(
        &self,
        id: ItemId,
    ) -> (
        Option<&LinearizationItem<Resolved>>,
        Option<&LinearizationItem<Resolved>>,
    ) {
        let Some(index) = self.id_to_index.get(&id).copied() else {
            return (None, None);
        };
        let (left_index, right_index) = (index - 1, index + 1);
        let left = self.linearization.get(left_index);
        let right = self.linearization.get(right_index);
        (left, right)
    }

    pub fn get_item(&self, id: ItemId) -> Option<&LinearizationItem<Resolved>> {
        self.id_to_index
            .get(&id)
            .and_then(|index| self.linearization.get(*index))
    }

    /// Try to retrieve the item from the current linearization first, and if that fails, look into
    /// the registry if there is a linearization corresponding to the item's file id.
    pub fn get_item_with_reg<'a>(
        &'a self,
        id: ItemId,
        lin_registry: &'a LinRegistry,
    ) -> Option<&'a LinearizationItem<Resolved>> {
        self.get_item(id).or_else(|| lin_registry.get_item(id))
    }

    pub fn get_item_mut(&mut self, id: ItemId) -> Option<&mut LinearizationItem<Resolved>> {
        let index = self.id_to_index.get(&id)?;
        self.linearization.get_mut(*index)
    }

    /// Finds the index of a linearization item for a given location
    /// The linearization is a list of items that are sorted by their physical occurrence.
    /// - Each element has a corresponding span in the source
    /// - Spans are either equal (same starting point, same length)
    ///   or shorter but never intersecting
    ///
    ///   (start_element_2 >= start_element_1 AND end_element_2 <= end_element_1)
    ///
    /// For any location a binary search is used to efficiently find the index
    /// of the *last* element that starts at this position.
    /// This corresponds to the most concrete Element as the linearization is
    /// 1. produced by a stable sort and
    /// 2. lower elements are more concrete
    ///
    /// If a perfect match cannot be found, the binary search still provides an
    /// anchor point from which we reversely find the first element that *contains*
    /// the location looked up
    ///
    /// If neither is possible `None` is returned as no corresponding linearization
    /// item could be found.
    ///
    pub fn item_at(&self, pos: RawPos) -> Option<&LinearizationItem<Resolved>> {
        let linearization = &self.linearization;
        let item = match linearization.binary_search_by(|item| {
            item.pos
                .as_opt_ref()
                .and_then(|span| span.start_pos().partial_cmp(&pos))
                .unwrap_or(std::cmp::Ordering::Less)
        }) {
            // Found item(s) starting at `locator`
            // search for most precise element
            Ok(index) => linearization[index..]
                .iter()
                .take_while(|item| {
                    // Here because None is smaller than everything, if binary search succeeds,
                    // we can safely unwrap the position.
                    item.pos.unwrap().start_pos() == pos
                })
                .last(),
            // No perfect match found
            // iterate back finding the first wrapping linearization item
            Err(index) => linearization[..index].iter().rfind(|item| {
                item.pos
                    .as_opt_ref()
                    .map(|span| span.contains(pos))
                    // if the item found is None, we can not find a better one.
                    .unwrap_or(true)
            }),
        };
        item
    }

    /// Return all the items in the scope of the given linearization item.
    pub fn get_in_scope<'a>(
        &'a self,
        LinearizationItem { env, .. }: &'a LinearizationItem<Resolved>,
        lin_registry: &'a LinRegistry,
    ) -> Vec<&'a LinearizationItem<Resolved>> {
        env.iter()
            .filter_map(|(_, id)| self.get_item_with_reg(*id, lin_registry))
            .collect()
    }

    /// Retrive the type and the metadata of a linearization item. Requires a registry as this code
    /// tries to jump to the definitions of objects to find the relevant data, which might lie in a
    /// different file (and thus in a different linearization within the registry).
    pub fn get_type_and_metadata(
        &self,
        item: &LinearizationItem<Resolved>,
        lin_registry: &LinRegistry,
    ) -> (Resolved, Vec<String>) {
        let mut extra = Vec::new();

        let item = match item.kind {
            TermKind::Usage(UsageState::Resolved(declaration)) => self
                .get_item_with_reg(declaration, lin_registry)
                .and_then(|decl| match decl.kind {
                    TermKind::Declaration {
                        value: ValueState::Known(value),
                        ..
                    }
                    | TermKind::RecordField {
                        value: ValueState::Known(value),
                        ..
                    } => self.get_item_with_reg(value, lin_registry),
                    _ => None,
                })
                .unwrap_or(item),
            TermKind::Declaration {
                value: ValueState::Known(value),
                ..
            } => self.get_item_with_reg(value, lin_registry).unwrap_or(item),
            _ => item,
        };

        if let Some(FieldMetadata {
            doc, annotation, ..
        }) = item.metadata.as_ref()
        {
            if let Some(doc) = doc {
                extra.push(doc.to_owned());
            }

            if let Some(contracts) = annotation.contracts_to_string() {
                extra.push(contracts);
            }
        }

        (item.ty.to_owned(), extra)
    }
}
