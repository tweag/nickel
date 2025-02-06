//! Traversal of trees of objects.

use crate::bytecode::ast::{Allocable, AstAlloc};

#[derive(Copy, Clone)]
pub enum TraverseOrder {
    TopDown,
    BottomUp,
}

/// Flow control for tree traverals.
pub enum TraverseControl<S, U> {
    /// Normal control flow: continue recursing into the children.
    ///
    /// Pass the state &S to all children.
    ContinueWithScope(S),
    /// Normal control flow: continue recursing into the children.
    ///
    /// The state that was passed to the parent will be re-used for the children.
    Continue,

    /// Skip this branch of the tree.
    SkipBranch,

    /// Finish traversing immediately (and return a value).
    Return(U),
}

impl<S, U> From<Option<U>> for TraverseControl<S, U> {
    fn from(value: Option<U>) -> Self {
        match value {
            Some(u) => TraverseControl::Return(u),
            None => TraverseControl::Continue,
        }
    }
}

pub trait Traverse<T>: Sized {
    /// Apply a transformation on a object containing syntactic elements of type `T` (terms, types,
    /// etc.) by mapping a faillible function `f` on each such node as prescribed by the order.
    ///
    /// `f` may return a generic error `E` and use the state `S` which is passed around.
    fn traverse<F, E>(self, f: &mut F, order: TraverseOrder) -> Result<Self, E>
    where
        F: FnMut(T) -> Result<T, E>;

    /// Recurse through the tree of objects top-down (a.k.a. pre-order), applying `f` to
    /// each object.
    ///
    /// Through its return value, `f` can short-circuit one branch of the traversal or
    /// the entire traversal.
    ///
    /// This traversal can make use of "scoped" state. The `scope` argument is passed to
    /// each callback, and the callback can optionally override that scope just for its
    /// own subtree in the traversal. For example, when traversing a tree of terms you can
    /// maintain an environment. Most of the time the environment should get passed around
    /// unchanged, but a let binder should override the environment of its subtree. It
    /// does this by returning a `TraverseControl::ContinueWithScope` that contains the
    /// new environment.
    fn traverse_ref<S, U>(
        &self,
        f: &mut dyn FnMut(&T, &S) -> TraverseControl<S, U>,
        scope: &S,
    ) -> Option<U>;

    fn find_map<S>(&self, mut pred: impl FnMut(&T) -> Option<S>) -> Option<S>
    where
        T: Clone,
    {
        self.traverse_ref(
            &mut |t, _state: &()| {
                if let Some(s) = pred(t) {
                    TraverseControl::Return(s)
                } else {
                    TraverseControl::Continue
                }
            },
            &(),
        )
    }
}

/// Similar to [Traverse], but takes an additional AST allocator for AST components that require
/// such an allocator in order to build the result.
pub trait TraverseAlloc<'ast, T>: Sized {
    /// Same as [Traverse::traverse], but takes an additional AST allocator.
    fn traverse<F, E>(
        self,
        alloc: &'ast AstAlloc,
        f: &mut F,
        order: TraverseOrder,
    ) -> Result<Self, E>
    where
        F: FnMut(T) -> Result<T, E>;

    /// Same as [Traverse::traverse_ref], but takes an additional AST allocator.
    ///
    /// There is as small difference though: this function guarantees that the lifetime of the
    /// references is bound to the lifetime of the AST allocator, which the signature in
    /// [Traverse::traverse_ref] does not. This is useful e.g. in the LSP to extract references and
    /// store them in separate data structure. We can guarantee that those reference won't be
    /// dangling as long as the allocator is around.
    fn traverse_ref<S, U>(
        &'ast self,
        f: &mut dyn FnMut(&'ast T, &S) -> TraverseControl<S, U>,
        scope: &S,
    ) -> Option<U>;

    fn find_map<S>(&'ast self, mut pred: impl FnMut(&'ast T) -> Option<S>) -> Option<S>
    where
        T: Clone + 'ast,
    {
        self.traverse_ref(
            &mut |t, _state: &()| {
                if let Some(s) = pred(t) {
                    TraverseControl::Return(s)
                } else {
                    TraverseControl::Continue
                }
            },
            &(),
        )
    }
}

/// Takes an iterator whose item type implements [TraverseAlloc], traverse each element, and
/// collect the result as a slice allocated via `alloc`.
pub fn traverse_alloc_many<'ast, T, U, I, F, E>(
    alloc: &'ast AstAlloc,
    it: I,
    f: &mut F,
    order: TraverseOrder,
) -> Result<&'ast [U], E>
where
    U: TraverseAlloc<'ast, T> + Sized + Allocable,
    I: IntoIterator<Item = U>,
    F: FnMut(T) -> Result<T, E>,
{
    let collected: Result<Vec<_>, E> = it
        .into_iter()
        .map(|elt| elt.traverse(alloc, f, order))
        .collect();

    Ok(alloc.alloc_many(collected?))
}
