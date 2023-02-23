use std::mem::transmute;
use std::mem::ManuallyDrop;

use super::*;

#[derive(Debug, Default, PartialEq, Clone)]
pub struct ArrayAttrs {
    /// A `closurized` array verifies the following conditions:
    ///   - Each element is a generated variable with a unique name (although the same
    ///     variable can occur in several places, it should always refer to the same index anyway).
    ///   - The environment of the array's closure only contains those generated variables.
    pub closurized: bool,
    /// List of lazily-applied contracts.
    /// These are only observed when data enters or leaves the array.
    pub pending_contracts: Vec<PendingContract>,
}

impl ArrayAttrs {
    /// Create an `ArrayAttrs`.
    /// By default, the `closurized` flag is set to `false`,
    /// and `pending_contracts` is empty.
    pub fn new() -> Self {
        Default::default()
    }

    /// Set the `closurized` flag to `true`.
    pub fn closurized(mut self) -> Self {
        self.closurized = true;
        self
    }

    /// Drop the pending contracts.
    pub fn contracts_cleared(mut self) -> Self {
        self.pending_contracts.clear();
        self
    }

    /// Extend contracts from an iterator of `PendingContract`.
    /// De-duplicate equal contracts. Note that current contract
    /// equality testing is very limited, but this may change in the
    /// future
    pub fn with_extra_contracts<I>(mut self, iter: I) -> Self
    where
        I: IntoIterator<Item = PendingContract>,
    {
        for ctr in iter {
            if !self.pending_contracts.contains(&ctr) {
                self.pending_contracts.push(ctr)
            }
        }

        self
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Array {
    inner: Rc<[RichTerm]>,
    start: usize,
    end: usize,
}

impl Array {
    /// Creates a Nickel array from reference-counted slice.
    pub fn new(inner: Rc<[RichTerm]>) -> Self {
        let start = 0;
        let end = inner.len();

        Self { inner, start, end }
    }

    /// Returns the effective length of the array.
    pub fn len(&self) -> usize {
        self.end - self.start
    }

    /// Returns `true` if the array is empty.
    pub fn is_empty(&self) -> bool {
        self.end == self.start
    }

    /// Returns a reference to the term at the given index.
    pub fn get(&self, idx: usize) -> Option<&RichTerm> {
        self.inner.get(self.start + idx)
    }

    /// Discards the first `diff` terms of the array.
    pub fn advance_by(mut self, diff: usize) -> Self {
        self.start += usize::min(diff, self.len());
        self
    }

    /// Makes a mutable slice into the given `Array`.
    pub fn make_mut(&mut self) -> &mut [RichTerm] {
        // NOTE: trying to use `Rc::make_mut` will result in the following compiler error:
        // > the trait `Clone` is not implemented for `[term::RichTerm]`
        // This seems to be an edge-case in the standard library.
        // As a workaround, if `get_mut` fails we recollect the array into a new `Rc` by cloning all terms.
        // Thus we make sure that the second call to `get_mut` won't fail.

        // This condition is the same as `!Rc::is_unique(&mut self.inner)`, but that function
        // is not public.
        if Rc::strong_count(&self.inner) != 1 || Rc::weak_count(&self.inner) != 0 {
            self.inner = self.iter().cloned().collect::<Rc<[_]>>();
        }

        Rc::get_mut(&mut self.inner).expect("non-unique Rc after deep-cloning Array")
    }

    /// Returns an iterator of references over the array.
    pub fn iter(&self) -> std::slice::Iter<'_, RichTerm> {
        self.as_ref().iter()
    }
}

impl Default for Array {
    fn default() -> Self {
        Self::new(Rc::new([]))
    }
}

impl FromIterator<RichTerm> for Array {
    fn from_iter<T: IntoIterator<Item = RichTerm>>(iter: T) -> Self {
        let inner = iter.into_iter().collect::<Rc<[_]>>();
        let start = 0;
        let end = inner.len();

        Self { inner, start, end }
    }
}

impl AsRef<[RichTerm]> for Array {
    fn as_ref(&self) -> &[RichTerm] {
        &self.inner[self.start..self.end]
    }
}

impl IntoIterator for Array {
    type Item = RichTerm;

    type IntoIter = IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        // SAFETY: If there are no other strong or weak references
        // to the inner slice, then we can:
        // - Drop the elements outside our inner view,
        // - Move out the elements inside out inner view.
        // Otherwise, we clone everything.

        unsafe {
            let mut inner: Rc<[ManuallyDrop<RichTerm>]> = transmute(self.inner);
            let idx = self.start;
            let end = self.end;

            if let Some(slice) = Rc::get_mut(&mut inner) {
                for term in &mut slice[..idx] {
                    ManuallyDrop::drop(term)
                }
                for term in &mut slice[end..] {
                    ManuallyDrop::drop(term)
                }
            }

            IntoIter { inner, idx, end }
        }
    }
}

pub struct IntoIter {
    inner: Rc<[ManuallyDrop<RichTerm>]>,
    idx: usize,
    end: usize,
}

impl Iterator for IntoIter {
    type Item = RichTerm;

    fn next(&mut self) -> Option<Self::Item> {
        if self.idx == self.end {
            None
        } else {
            let term = match Rc::get_mut(&mut self.inner) {
                None => self.inner[..self.end]
                    .get(self.idx)
                    .cloned()
                    .map(ManuallyDrop::into_inner),
                Some(slice) => slice[..self.end]
                    .get_mut(self.idx)
                    .map(|t| unsafe { ManuallyDrop::take(t) }),
            };

            self.idx += 1;
            term
        }
    }
}

impl ExactSizeIterator for IntoIter {
    fn len(&self) -> usize {
        self.end - self.idx
    }
}
