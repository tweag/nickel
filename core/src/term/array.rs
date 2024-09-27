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
    inner: rpds::Vector<RichTerm>,
    start: usize,
    end: usize,
}

pub struct OutOfBoundError;

impl Array {
    /// Creates a Nickel array from reference-counted slice.
    pub fn new(iter: impl IntoIterator<Item = RichTerm>) -> Self {
        iter.into_iter().collect()
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
        self.inner.get(self.inner.len() - self.start - 1 - idx)
    }

    /// Discards the first `diff` terms of the array.
    pub fn advance_by(mut self, diff: usize) -> Self {
        self.start += usize::min(diff, self.len());
        self
    }

    /// Returns an iterator of references over the array.
    pub fn iter<'a>(&'a self) -> impl Iterator<Item = &'a RichTerm> + 'a {
        self.inner.iter().rev().skip(self.start).take(self.len())
    }

    pub fn iter_rev<'a>(&'a self) -> impl Iterator<Item = &'a RichTerm> + 'a {
        self.inner
            .iter()
            .skip(self.inner.len() - self.end)
            .take(self.len())
    }

    pub fn from_reversed_vector(inner: rpds::Vector<RichTerm>) -> Self {
        Self {
            start: 0,
            end: inner.len(),
            inner,
        }
    }

    pub fn into_reversed_vector(mut self) -> rpds::Vector<RichTerm> {
        if self.end != self.inner.len() {
            // There's no efficient way to chop off the beginning of a vector, so
            // in this case we just need to copy it.
            self.iter_rev().cloned().collect()
        } else {
            for _ in 0..self.start {
                self.inner.drop_last_mut();
            }
            self.inner
        }
    }
}

impl Default for Array {
    fn default() -> Self {
        Self {
            inner: rpds::Vector::default(),
            start: 0,
            end: 0,
        }
    }
}

impl FromIterator<RichTerm> for Array {
    fn from_iter<T: IntoIterator<Item = RichTerm>>(iter: T) -> Self {
        // Ugh. rpds::Vector doesn't support reverse-in-place
        let items = iter.into_iter().collect::<Vec<_>>();
        let inner = items.into_iter().rev().collect::<rpds::Vector<_>>();
        let start = 0;
        let end = inner.len();

        Self { inner, start, end }
    }
}
