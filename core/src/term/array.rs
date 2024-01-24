use std::mem::transmute;
use std::mem::ManuallyDrop;

use super::*;

#[derive(Debug, Default, PartialEq, Clone)]
pub struct ArrayAttrs {
    /// An array is closurized when each element is a [crate::term::Term::Closure] or a constant.
    ///
    /// When initially produced by the parser, arrays aren't closurized. At the first evaluation,
    /// they will be turned into closurized arrays, by allocating cache nodes (think thunks) for
    /// non constant elements, and this flag is set to true.
    ///
    /// Ideally, we would have a different AST representation for evaluation, where arrays would
    /// always be closurized. In the meantime, while we need to cope with a unique AST across the
    /// whole pipeline, we use this flag.
    pub closurized: bool,
    /// List of lazily-applied contracts.
    /// These are only observed when data enters or leaves the array.
    pub pending_contracts: Vec<RuntimeContract>,
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
}

/// A Nickel array, represented as a view (slice) into a shared backing array. The view is
/// delimited by `start` (included) and `end` (excluded). This allows to take the tail of an array,
/// or an arbitrary slice, in constant time, providing actual linear time iteration when
/// imlementing recursive functions, such as folds, for example.
#[derive(Debug, Clone, PartialEq)]
pub struct Array {
    inner: Rc<[RichTerm]>,
    start: usize,
    end: usize,
}

pub struct OutOfBoundError;

impl Array {
    /// Creates a Nickel array from reference-counted slice.
    pub fn new(inner: Rc<[RichTerm]>) -> Self {
        let start = 0;
        let end = inner.len();

        Self { inner, start, end }
    }

    /// Resize the view to be a a sub-view of the current one, by considering a slice `start`
    /// (included) to `end` (excluded).
    ///
    /// The parameters must satisfy `0 <= start <= end <= self.end - self.start`. Otherwise,
    /// `Err(..)` is returned.
    pub fn slice(&mut self, start: usize, end: usize) -> Result<(), OutOfBoundError> {
        if start > end || end > self.len() {
            return Err(OutOfBoundError);
        }

        let prev_start = self.start;
        self.start = prev_start + start;
        self.end = prev_start + end;

        Ok(())
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
        // As a workaround, if `get_mut` fails we recollect the array into a new `Rc` by cloning all
        // terms.
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
        // - Drop the rest of the elements when we're dropped
        // Otherwise, we clone everything.

        let inner = if Rc::strong_count(&self.inner) != 1 || Rc::weak_count(&self.inner) != 0 {
            IntoIterInner::Shared(self.inner)
        } else {
            unsafe {
                let mut inner =
                    transmute::<Rc<[RichTerm]>, Rc<[ManuallyDrop<RichTerm>]>>(self.inner);
                let slice =
                    Rc::get_mut(&mut inner).expect("non-unique Rc after checking for uniqueness");
                for term in &mut slice[..self.start] {
                    ManuallyDrop::drop(term)
                }
                for term in &mut slice[self.end..] {
                    ManuallyDrop::drop(term)
                }

                IntoIterInner::Owned(inner)
            }
        };
        IntoIter {
            inner,
            idx: self.start,
            end: self.end,
        }
    }
}

enum IntoIterInner {
    Shared(Rc<[RichTerm]>),
    // This shouldn't really be an Rc. It should only ever have one reference.
    // There may be a way to rewrite this once unsized locals are stabilized.
    // But for now, there is no good way to repackage the inner array, so we
    // keep it in an Rc.
    Owned(Rc<[ManuallyDrop<RichTerm>]>),
}
pub struct IntoIter {
    inner: IntoIterInner,
    idx: usize,
    end: usize,
}

impl Iterator for IntoIter {
    type Item = RichTerm;

    fn next(&mut self) -> Option<Self::Item> {
        if self.idx == self.end {
            None
        } else {
            let term = match &mut self.inner {
                IntoIterInner::Shared(inner) => inner.get(self.idx).cloned(),
                IntoIterInner::Owned(inner) => Rc::get_mut(inner)
                    .expect("non-unique Rc after checking for uniqueness")
                    .get_mut(self.idx)
                    // SAFETY: We already checcked that we have the only
                    // reference to inner, and after this we increment idx, and
                    // we never access elements with indexes less than idx
                    .map(|t| unsafe { ManuallyDrop::take(t) }),
            };
            self.idx += 1;
            term
        }
    }
}

impl Drop for IntoIter {
    fn drop(&mut self) {
        // drop the rest of the items we haven't iterated through
        if let IntoIterInner::Owned(inner) = &mut self.inner {
            let inner = Rc::get_mut(inner).expect("non-unique Rc after checking for uniqueness");
            for term in &mut inner[self.idx..self.end] {
                // SAFETY: Wwe already checked that we have the only reference
                // to inner, and once we are done dropping the remaining
                // elements, `inner` will get dropped and there will be no
                // remaining references.
                unsafe {
                    let _ = ManuallyDrop::take(term);
                }
            }
        }
    }
}

impl ExactSizeIterator for IntoIter {
    fn len(&self) -> usize {
        self.end - self.idx
    }
}
