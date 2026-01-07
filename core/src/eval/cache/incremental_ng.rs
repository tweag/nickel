//! Cross-evaluation incremental evaluation capabilities.

use super::{
    Cache,
    lazy::{CBNCache, ContentHash, Thunk},
};
use std::{collections::HashMap, io};

#[derive(Clone)]
pub struct LoadableThunk {}

impl LoadableThunk {
    pub fn load(&self) -> Thunk {
        todo!()
    }
}

#[derive(Clone)]
pub enum CacheEntry {
    /// The entry is coming from a previous evaluation round, but it hasn't been re-used yet.
    Loadable(LoadableThunk),
    /// The entry is coming from a previous evaluation round, and it has been re-used in the
    /// current one.
    Loaded(Thunk),
    /// The entry is a new thunk coming from the current evaluation round.
    Recorded(Thunk),
}

//TODO[design]:
// Not clear yet if all hashable thunks should always be fetched from cache, or if we need a
// distinction between dependents (intermediate thunks) whose hash is needed but that we don't
// necessarily want to put in the cache, and thunks of interest. Note that at the time of
// serialization, we'll need _some_ dependencies, but they might have changed (or be gone
// completly). If there is no distinction, then we don't need the whole on-demand computation for
// hashes: we need to pre-compute the hash anyway to see if the thunk is in the cache, so we can
// very much fill it right away at thunk creation. Concretely, the question is: should we have only
// one `add_cached` additional method, or a `add_hashed` and `add_or_get_from_cache`? Let's
// meditate.
//
// # After meditation
//
// I think there's an even better way: makes hashing/cache fetching, and only try to fetch from the
// incremental cache when we first evaluate a (hashable) thunk. Doing so, any unused thunk will be
// left alone. We can compute hashes in a lazy way. This means we allocate a thunk unconditionally
// at first (even if it ends up being pulled from the incremental cache), but it's not a huge cost
// and is likely to be similar to the cost of pulling an existing loadable thunk from the
// incremental cache anyway, as we need to put the thunk data somewhere.
#[derive(Default, Clone)]
pub struct IncrementalCache {
    cbn_cache: CBNCache,
    thunks: HashMap<ContentHash, CacheEntry>,
}

impl IncrementalCache {
    /// Serialize the cache to a persistent storage (typically a file), to be re-used in a
    /// subsequent evaluation. Only loaded and recorded [CacheEntry]s will be saved; loadable ones
    /// (recorded from the previous evaluation but not used in the current one) are dropped.
    pub fn persist(self, out: impl io::Write) -> io::Result<()> {
        todo!()
    }

    /// Loads a persisted cache.
    pub fn load(src: impl io::Read) -> io::Result<Self> {
        todo!()
    }

    /// Allocates a thunk for the given closure. This replaces [Cache::add] for thunks of interest
    /// in the incremental cache.
    ///
    /// The hash is left unknown, and is computed for thunks of interest when they're first
    /// evaluated.
    #[inline]
    pub fn add_cached(
        &mut self,
        clos: crate::eval::Closure,
        bty: crate::term::BindingType,
        cui: ContentHash,
    ) -> <Self as Cache>::UpdateIndex {
        let idx = self.cbn_cache.add(clos, bty);
        idx.set_cui(cui);
        idx
    }
}

impl Cache for IncrementalCache {
    type UpdateIndex = <CBNCache as Cache>::UpdateIndex;

    fn get(&self, idx: super::CacheIndex) -> crate::eval::Closure {
        self.cbn_cache.get(idx)
    }

    fn get_update_index(
        &mut self,
        idx: &mut super::CacheIndex,
    ) -> Result<Option<Self::UpdateIndex>, super::BlackholedError> {
        self.cbn_cache.get_update_index(idx)
    }

    fn add(
        &mut self,
        clos: crate::eval::Closure,
        bty: crate::term::BindingType,
    ) -> super::CacheIndex {
        self.cbn_cache.add(clos, bty)
    }

    fn patch<F: Fn(&mut crate::eval::Closure)>(&mut self, idx: super::CacheIndex, f: F) {
        self.cbn_cache.patch(idx, f)
    }

    fn get_then<T, F: FnOnce(&crate::eval::Closure) -> T>(
        &self,
        idx: super::CacheIndex,
        f: F,
    ) -> T {
        self.cbn_cache.get_then(idx, f)
    }

    fn update(&mut self, clos: crate::eval::Closure, idx: Self::UpdateIndex) {
        self.cbn_cache.update(clos, idx)
    }

    fn new() -> Self {
        Self::default()
    }

    fn reset_index_state(&mut self, idx: &mut Self::UpdateIndex) {
        self.cbn_cache.reset_index_state(idx)
    }

    fn build_cached(
        &mut self,
        idx: &mut super::CacheIndex,
        rec_env: &[(nickel_lang_parser::identifier::Ident, super::CacheIndex)],
    ) {
        self.cbn_cache.build_cached(idx, rec_env)
    }

    fn saturate<I: DoubleEndedIterator<Item = nickel_lang_parser::identifier::Ident> + Clone>(
        &mut self,
        idx: super::CacheIndex,
        fields: I,
    ) -> crate::eval::value::NickelValue {
        self.cbn_cache.saturate(idx, fields)
    }

    fn revert(&mut self, idx: &super::CacheIndex) -> super::CacheIndex {
        self.cbn_cache.revert(idx)
    }

    fn deps(&self, idx: &super::CacheIndex) -> Option<crate::term::record::FieldDeps> {
        self.cbn_cache.deps(idx)
    }

    fn make_update_index(
        &mut self,
        idx: &mut super::CacheIndex,
    ) -> Result<Self::UpdateIndex, super::BlackholedError> {
        self.cbn_cache.make_update_index(idx)
    }
}
