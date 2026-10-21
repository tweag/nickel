//! # Cross-evaluation Unique Identifier
//!
//! The CUI is a semantic hash of expressions that makes it possible for the incremental evaluator
//! to identify and match expressions that haven't changed since the last evaluation, so that their
//! result can be re-used.
//!
//! # Semantic hash of closures and open expressions
//!
//! We compute the semantic hash of an actual closure by combining the CUI of its core expression
//! with the CUI of its dependencies, hashing everything together. For example, if `CUI(x + 1) =
//! A`, `x` is bound to `0` in the environment, and `CUI(0) = B`, the semantic hash of the closure
//! `{x + 1 | x <- 0 }` is something akin to `hash((A,B))`.
//!
//! # Semantic hashing schemes
//!
//! There are a lot of possible semantic hashing schemes. By scheme, we mean a specific
//! implementation as a function from expressions to CUI/hashes. The fundamental constraint we
//! require is that if `CUI(e1) = CUI(e2)`, then `e1` and `e2` are beta-equivalent
//! (to simplify, they either both loop or evaluate to the same value: for example `1+1` and `2`
//! are beta-equivalent). Otherwise the incremental evaluator could change the result of a program
//! by replacing `e1` with a different, non-equivalent `e2` that happens to have the same CUI.
//!
//! Possible schemes are for example:
//!
//! - hashing the source expression as text
//! - hashing the AST
//! - hashing the AST modulo some rules or some normalization (for example hashing modulo
//!   alpha-conversion)
//! - unique and deterministic index based on hash-consing
//! - etc.
//!
//! The main trade-off for the scheme selection is between overhead and generality. A more general
//! semantic hash equalize more terms, or put differently, is invariant by more semantic-preserving
//! transformations. The more general, the better: the interpreter can identify more expressions
//! for reuse. However, it usually also means that the CUI is more expensive to compute, which can
//! nullify the benefits or heavily penalize cases with a lot of changes.
//!
//! The spectrum goes from the degenerate case of assigning a fresh, unique, random CUI to every
//! expression, which is fast and sound (probabilistically at least) but useless (no matching is
//! possible). The other end is an ideal scheme verifying `e1` beta-equivalent to `e2` implies
//! `CUI(e1) = CUI(e2)`. This would give the best reuse but isn't even computable because of the
//! halting problem.
//!
//! The interpreter picks so-called _thunks of interest_, which are thunks that are worth caching
//! across evaluations (as hashing, recording and persisting has a cost). This is the thunks for
//! which we compute the CUI.

use super::{
    Closure,
    cache::Cache,
    value::{
        ArrayData, Container, EnumVariantData, NickelValue, RecordData, Thunk, ValueContentRef,
    },
};

use std::hash::{DefaultHasher, Hash, Hasher};

/// A semantic hash for re-using previous computations in the incremental evaluation mode.
#[derive(Copy, Debug, PartialEq, Eq, Clone, Hash)]
pub struct SemanticHash(pub u64);

pub fn cui(_v: &NickelValue) -> SemanticHash {
    unimplemented!()
}

/// In the context of incremental evaluation, decides if an expression put in thunk should be
/// (given its content):
///
/// 1. Fetched from the incremental cache, re-using its value from the last evaluation, if
///    possible.
/// 2. Recorded as thunk of interest in the incremental cache given its content. A thunk of
///    interest is hashed and persisted as a candidate to be re-used in the next evaluation.
///
/// Such thunks are called thunks of interest. Trying to fetch a thunk or recording it for future
/// use has a cost. Ideally we'd like to strike a balance between this cost and the expected
/// return. Typically, thunk of interests should be rather costly to compute (otherwise, it might
/// be cheaper to recompute them from scratch) and have good chances of surviving successive
/// changes (e.g focusing on top-level configurations fields rather than local variables).
///
/// Currently, the decision algorithm for thunks of interest is unimplemented.
pub fn is_of_interest(_v: &NickelValue) -> bool {
    unimplemented!()
}

pub trait Register<C: Cache> {
    /// If `self` is a thunk and its content makes it a candidate for incremental caching
    /// [interesting][is_of_interest], it's registered in the incremental cache (in practice, set
    /// its [CUI][cui]).
    ///
    /// When `Self` represents a container with thunks inside ([RecordData], [ArrayData]), the
    /// _register if interesting_ operation is applied to each element.
    ///
    /// For instances where `self` might sometime not be a thunk or a thunk container, or for
    /// thunks that are not of interest, this function is a no-op.
    fn register(&self, cache: &mut C);
}

impl<C: Cache> Register<C> for NickelValue {
    fn register(&self, cache: &mut C) {
        if let Some(thunk) = self.as_thunk() {
            thunk.register(cache);
        }
    }
}

impl<C: Cache> Register<C> for Thunk {
    fn register(&self, cache: &mut C) {
        let content = &self.borrow().value;

        if is_of_interest(content) {
            cache.attach_cui(self, cui(content));
        }
    }
}

impl<C: Cache> Register<C> for RecordData {
    fn register(&self, cache: &mut C) {
        for field in self.fields.values() {
            //TODO: we cache the pure value, but not the potential pending contracts applied to this
            //value, which we probably want as well. It's not entirely clear how to do that given the
            //current implementation (there's no associated thunk), so it's left for future work.
            if let Some(value) = &field.value {
                value.register(cache)
            }
        }
    }
}

impl<C: Cache> Register<C> for ArrayData {
    fn register(&self, cache: &mut C) {
        for value in self.array.iter() {
            //TODO: as for records, we should try to cache the value with pending contracts appied.
            value.register(cache)
        }
    }
}

/// Computes the semantic hash of a closure, given an optional Cross-evaluation Unique Identifier.
///
/// If no CUI is provided, the closure to hash is a dependency of a thunk of interest, but isn't
/// itself of interest. We don't want to override the original decision of the interpreter so we
/// don't compute its CUI, but we still try a shallow, fast hash that works on simple values.
pub fn semantic_hash(closure: &Closure, cui: Option<SemanticHash>) -> Option<SemanticHash> {
    // TODO: For now, we're being stupid, and hash the whole environment. What we should do is
    // 1. Compute the free variables of each expression of interest
    // 2. Only retrieve the free variables as dependencies from the environment
    let mut hasher = DefaultHasher::new();

    for (id, thunk) in closure.env.iter_elems() {
        id.hash(&mut hasher);
        thunk.semantic_hash()?.hash(&mut hasher);
    }

    if let Some(cui) = cui {
        cui.hash(&mut hasher);
    } else {
        // If we don't have a cross-evaluation unique identifier, we still try to structurally
        // hash simple constants, that don't involve other expressions.
        shallow_semantic_hash(&closure.value, &mut hasher).ok()?;
    }

    Some(SemanticHash(hasher.finish()))
}

struct ComplexValueError;

/// Compute a fast hash for simple values (mostly constants) with a given hasher, or return `Err` if
/// the value is more complex.
fn shallow_semantic_hash<H: Hasher>(
    value: &NickelValue,
    hasher: &mut H,
) -> Result<(), ComplexValueError> {
    // If we don't have a cross-evaluation unique identifier, we still try to structurally
    // hash simple constants, that don't involve other expressions.
    value.tag().hash(hasher);

    match value.content_ref() {
        ValueContentRef::Null => 0u8.hash(hasher),
        ValueContentRef::Bool(b) => b.hash(hasher),
        ValueContentRef::Number(n) => n.hash(hasher),
        ValueContentRef::String(s) => s.hash(hasher),
        ValueContentRef::EnumVariant(EnumVariantData { tag, arg }) => {
            // We also compute a hash for a (nested) enum with constants inside, which is rather
            // common. We start with the argument to avoid hashing the `tag` if the argument is not
            // a simple value.
            if let Some(arg) = arg {
                shallow_semantic_hash(arg, hasher)?;
            }

            tag.hash(hasher);
        }
        ValueContentRef::Array(Container::Empty) | ValueContentRef::Record(Container::Empty) => {
            0.hash(hasher)
        }
        _ => return Err(ComplexValueError),
    }

    Ok(())
}
