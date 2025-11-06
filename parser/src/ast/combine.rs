//! Define a `Combine` trait that takes an allocator, two AST components, and returns a new AST
//! component.

use super::{
    Annotation, AstAlloc,
    record::{FieldMetadata, MergePriority},
};

/// Trait for structures representing a series of annotation that can be combined (flattened).
/// Pedantically, `Combine` is a monoid: we expect that combining with `Default::default()` leaves
/// the other value unchanged.
pub trait Combine<'ast>: Default {
    /// Combine two elements.
    fn combine(alloc: &'ast AstAlloc, left: Self, right: Self) -> Self;
}

impl<'ast> Combine<'ast> for FieldMetadata<'ast> {
    /// Combine two field metadata into one. If data that can't be combined (typically, the
    /// documentation or the type annotation) are set by both, the left one's are kept.
    fn combine(alloc: &'ast AstAlloc, left: Self, right: Self) -> Self {
        let priority = match (left.priority, right.priority) {
            // Neutral corresponds to the case where no priority was specified. In that case, the
            // other priority takes precedence.
            (MergePriority::Neutral, p) | (p, MergePriority::Neutral) => p,
            // Otherwise, we keep the maximum of both priorities, as we would do when merging
            // values.
            (p1, p2) => std::cmp::max(p1, p2),
        };

        FieldMetadata {
            doc: merge_doc(left.doc, right.doc),
            annotation: Combine::combine(alloc, left.annotation, right.annotation),
            opt: left.opt || right.opt,
            // The resulting field will be suppressed from serialization if either of the fields to be merged is.
            not_exported: left.not_exported || right.not_exported,
            priority,
        }
    }
}

impl<'ast> Combine<'ast> for Annotation<'ast> {
    /// Combine two annotations. If both have `types` set, the final type
    /// is the one of the left annotation, while the right one's type is put
    /// inside the final `contracts`.
    ///
    /// Contracts are combined from left to right; the left one's are put first,
    /// then maybe the right one's type annotation and then the right one's
    /// contracts.
    fn combine(alloc: &'ast AstAlloc, left: Self, right: Self) -> Self {
        let (typ, leftover) = match (left.typ, right.typ) {
            (left_ty @ Some(_), right_ty @ Some(_)) => (left_ty, right_ty),
            (left_ty, right_ty) => (left_ty.or(right_ty), None),
        };

        let contracts: Vec<_> = left
            .contracts
            .iter()
            .cloned()
            .chain(leftover)
            .chain(right.contracts.iter().cloned())
            .collect();

        alloc.annotation(typ, contracts)
    }
}

/// Merge two optional documentations.
pub(crate) fn merge_doc<'ast>(
    doc1: Option<&'ast str>,
    doc2: Option<&'ast str>,
) -> Option<&'ast str> {
    //FIXME: how to merge documentation? Just concatenate?
    doc1.or(doc2)
}
