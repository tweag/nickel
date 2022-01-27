use std::collections::HashMap;

use codespan::ByteIndex;
use nickel::{
    term::MetaValue,
    typecheck::linearization::{LinearizationState, Scope, ScopeId},
};

use super::{
    building::ID,
    interface::{Resolved, TermKind, UsageState, ValueState},
    LinearizationItem,
};

#[derive(Debug, Default)]
pub struct Completed {
    pub linearization: Vec<LinearizationItem<Resolved>>,
    scope: HashMap<Scope, Vec<usize>>,
    id_to_index: HashMap<ID, usize>,
}

impl Completed {
    pub fn new(
        linearization: Vec<LinearizationItem<Resolved>>,
        scope: HashMap<Scope, Vec<usize>>,
        id_to_index: HashMap<ID, usize>,
    ) -> Self {
        Self {
            linearization,
            scope,
            id_to_index,
        }
    }

    pub fn get_item(&self, id: usize) -> Option<&LinearizationItem<Resolved>> {
        self.id_to_index
            .get(&id)
            .and_then(|index| self.linearization.get(*index))
    }

    pub fn get_in_scope(
        &self,
        LinearizationItem { scope, .. }: &LinearizationItem<Resolved>,
    ) -> Vec<&LinearizationItem<Resolved>> {
        let empty = Vec::with_capacity(0);
        (0..scope.len())
            .flat_map(|end| {
                eprintln!("in scope {:?}: {:?}", scope, self.scope.get(scope));
                self.scope.get(&scope[..=end]).unwrap_or(&empty)
            })
            .map(|id| self.get_item(*id))
            .flatten()
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
        let item = match linearization
            .binary_search_by_key(locator, |item| (item.pos.src_id, item.pos.start))
        {
            // Found item(s) starting at `locator`
            // search for most precise element
            Ok(index) => linearization[index..]
                .iter()
                .take_while(|item| (item.pos.src_id, item.pos.start) == *locator)
                .last(),
            // No perfect match found
            // iterate back finding the first wrapping linearization item
            Err(index) => linearization[..index].iter().rfind(|item| {
                let (istart, iend, ifile) = (item.pos.start, item.pos.end, item.pos.src_id);

                file_id == &ifile && start > &istart && start < &iend
            }),
        };
        item
    }

    /// Resolve type and meta information for a given item
    pub fn resolve_item_type_meta(
        &self,
        item: &LinearizationItem<Resolved>,
    ) -> (Resolved, Vec<String>) {
        let mut extra = Vec::new();

        let item = match item.kind {
            TermKind::Usage(UsageState::Resolved(declaration)) => self
                .get_item(declaration)
                .and_then(|decl| match decl.kind {
                    TermKind::Declaration(_, _, ValueState::Known(value))
                    | TermKind::RecordField {
                        value: ValueState::Known(value),
                        ..
                    } => self.get_item(value),
                    _ => None,
                })
                .unwrap_or(item),
            TermKind::Declaration(_, _, ValueState::Known(value)) => {
                self.get_item(value).unwrap_or(item)
            }
            _ => item,
        };

        if let Some(MetaValue {
            ref doc,
            ref types,
            ref contracts,
            priority,
            ..
        }) = item.meta.as_ref()
        {
            if let Some(doc) = doc {
                extra.push(doc.to_owned());
            }
            if let Some(types) = types {
                extra.push(types.label.tag.to_string());
            }
            if !contracts.is_empty() {
                extra.push(
                    contracts
                        .iter()
                        .map(|contract| format!("{}", contract.label.types,))
                        .collect::<Vec<_>>()
                        .join(","),
                );
            }

            extra.push(format!("Merge Priority: {:?}", priority));
        }

        (item.ty.to_owned(), extra)
    }
}

impl LinearizationState for Completed {}
