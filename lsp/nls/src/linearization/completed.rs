use std::collections::HashMap;

use codespan::{ByteIndex, FileId};
use nickel_lang::{term::MetaValue, typecheck::linearization::LinearizationState};

use super::{
    interface::{Resolved, TermKind, UsageState, ValueState},
    ItemId, LinearizationItem,
};

#[derive(Debug, Default, Clone)]
pub struct Completed {
    pub linearization: Vec<LinearizationItem<Resolved>>,
    id_to_index: HashMap<ItemId, usize>,
}

impl Completed {
    pub fn new(
        linearization: Vec<LinearizationItem<Resolved>>,
        id_to_index: HashMap<ItemId, usize>,
    ) -> Self {
        Self {
            linearization,
            id_to_index,
        }
    }

    pub fn get_item<'a>(
        &'a self,
        id: ItemId,
        lin_cache: &'a HashMap<FileId, Completed>,
    ) -> Option<&'a LinearizationItem<Resolved>> {
        let ItemId { file_id, .. } = id;
        self.id_to_index
            .get(&id)
            .and_then(|index| self.linearization.get(*index))
            .or_else(|| {
                let lin = lin_cache.get(&file_id).unwrap();
                lin.get_item(id, lin_cache)
            })
    }

    pub fn get_item_mut(&mut self, id: ItemId) -> Option<&mut LinearizationItem<Resolved>> {
        let index = self.id_to_index.get(&id)?;
        self.linearization.get_mut(*index)
    }

    pub fn get_in_scope<'a>(
        &'a self,
        LinearizationItem { env, .. }: &'a LinearizationItem<Resolved>,
        lin_cache: &'a HashMap<FileId, Completed>,
    ) -> Vec<&LinearizationItem<Resolved>> {
        env.iter()
            .filter_map(|(_, id)| self.get_item(*id, lin_cache))
            .collect()
    }

    /// Finds the index of a linearization item for a given location
    /// The linearization is a list of items that are sorted by their physical occurence.
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
    pub fn item_at(
        &self,
        locator: &(codespan::FileId, ByteIndex),
    ) -> Option<&LinearizationItem<Resolved>> {
        let (file_id, start) = locator;
        let linearization = &self.linearization;
        let item = match linearization.binary_search_by(|item| {
            item.pos
                .as_opt_ref()
                .map(|pos| (pos.src_id, pos.start).cmp(locator))
                .unwrap_or(std::cmp::Ordering::Less)
        }) {
            // Found item(s) starting at `locator`
            // search for most precise element
            Ok(index) => linearization[index..]
                .iter()
                .take_while(|item| {
                    // Here because None is smaller than everything, if binary search succeeds,
                    // we can safely unwrap the position.
                    let pos = item.pos.unwrap();
                    (pos.src_id, pos.start) == *locator
                })
                .last(),
            // No perfect match found
            // iterate back finding the first wrapping linearization item
            Err(index) => linearization[..index].iter().rfind(|item| {
                item.pos
                    .as_opt_ref()
                    .map(|pos| file_id == &pos.src_id && start > &pos.start && start < &pos.end)
                    // if the item found is None, we can not find a better one.
                    .unwrap_or(true)
            }),
        };
        item
    }

    /// Resolve type and meta information for a given item
    pub fn resolve_item_type_meta(
        &self,
        item: &LinearizationItem<Resolved>,
        lin_cache: &HashMap<FileId, Completed>,
    ) -> (Resolved, Vec<String>) {
        let mut extra = Vec::new();

        let item = match item.kind {
            TermKind::Usage(UsageState::Resolved(declaration)) => self
                .get_item(declaration, lin_cache)
                .and_then(|decl| match decl.kind {
                    TermKind::Declaration(_, _, ValueState::Known(value))
                    | TermKind::RecordField {
                        value: ValueState::Known(value),
                        ..
                    } => self.get_item(value, lin_cache),
                    _ => None,
                })
                .unwrap_or(item),
            TermKind::Declaration(_, _, ValueState::Known(value)) => {
                self.get_item(value, lin_cache).unwrap_or(item)
            }
            _ => item,
        };

        if let Some(
            meta @ MetaValue {
                ref doc,
                ref types,
                priority,
                ..
            },
        ) = item.meta.as_ref()
        {
            if let Some(doc) = doc {
                extra.push(doc.to_owned());
            }
            if let Some(types) = types {
                extra.push(types.label.tag.to_string());
            }
            if let Some(contracts) = meta.contracts_to_string() {
                extra.push(contracts);
            }

            extra.push(format!("Merge Priority: {:?}", priority));
        }

        (item.ty.to_owned(), extra)
    }
}

impl LinearizationState for Completed {}
