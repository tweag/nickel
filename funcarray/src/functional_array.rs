use std::ops::Index;

use crate::vector::{RevIntoIter, RevIter};

use super::Vector;

/// A persistent list with fast random access.
///
/// The `FunctionArray` container is so called because it has some array-like
/// performance characteristics (like fast random access and fast slicing),
/// while also being fast at patterns that are common in functional languages
/// (like prepending elements).
///
/// This is implemented internally as a tree, and the parameter `N` controls its
/// branching factor. For performance, it should always be a power of 2. Values
/// between `8` and `64` are pretty reasonable.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FunctionalArray<T, const N: usize> {
    rev_vec: Vector<T, N>,
    // Our slice involves the range of indices [start, end), like most slicing.
    // But since we work in reverse, our "first" element is at `end - 1`.
    start: usize,
    end: usize,
}

impl<T, const N: usize> Default for FunctionalArray<T, N> {
    fn default() -> Self {
        FunctionalArray {
            rev_vec: Default::default(),
            start: 0,
            end: 0,
        }
    }
}

impl<T: Clone + serde::Serialize, const N: usize> serde::Serialize for FunctionalArray<T, N> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeSeq;

        let mut seq = serializer.serialize_seq(Some(self.len()))?;
        for elt in self.iter() {
            seq.serialize_element(elt)?;
        }
        seq.end()
    }
}

impl<'de, T: Clone + serde::Deserialize<'de>, const N: usize> serde::Deserialize<'de>
    for FunctionalArray<T, N>
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let vec: Vec<T> = Vec::deserialize(deserializer)?;
        Ok(FunctionalArray::collect(vec.into_iter()))
    }
}

impl<T: Clone, const N: usize> FunctionalArray<T, N> {
    /// Create a new `FunctionalArray` out of a double-ended iterator.
    ///
    /// `FunctionalArray` doesn't implement `FromIterator` because for efficient
    /// creation it needs to iterate in reverse.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use nickel_lang_funcarray::FunctionalArray;
    /// let arr = FunctionalArray::<_, 32>::collect([0, 1, 2, 3, 4, 5].into_iter());
    /// assert_eq!(arr[0], 0);
    /// assert_eq!(arr[5], 5);
    /// ```
    pub fn collect<I: DoubleEndedIterator<Item = T>>(iter: I) -> Self {
        let rev_vec: Vector<T, N> = iter.rev().collect();
        Self {
            end: rev_vec.len(),
            rev_vec,
            start: 0,
        }
    }

    /// The number of elements in this array.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use nickel_lang_funcarray::FunctionalArray;
    /// let arr = FunctionalArray::<_, 32>::collect([0, 1, 2, 3, 4, 5].into_iter());
    /// assert_eq!(arr.len(), 6);
    /// ```
    pub fn len(&self) -> usize {
        self.end - self.start
    }

    /// Returns `true` if the length is zero.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use nickel_lang_funcarray::FunctionalArray;
    /// let arr = FunctionalArray::<_, 32>::collect([0, 1, 2, 3, 4, 5].into_iter());
    /// assert!(!arr.is_empty());
    /// assert!(FunctionalArray::<i32, 32>::default().is_empty());
    /// ```
    pub fn is_empty(&self) -> bool {
        self.end == self.start
    }

    /// Gets an element at a given index, or `None` if `idx` is out-of-bounds.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use nickel_lang_funcarray::FunctionalArray;
    /// let arr = FunctionalArray::<_, 32>::collect([0, 1, 2, 3, 4, 5].into_iter());
    /// assert_eq!(arr.get(2), Some(&2));
    /// assert_eq!(arr.get(6), None);
    /// ```
    pub fn get(&self, idx: usize) -> Option<&T> {
        self.end
            .checked_sub(idx)
            .and_then(|i| i.checked_sub(1))
            .and_then(|i| self.rev_vec.get(i))
    }

    /// Adds an element to the beginning of this array.
    ///
    /// Runs in time complexity `O(log n)` where `n` is the array length.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use nickel_lang_funcarray::FunctionalArray;
    /// let mut arr = FunctionalArray::<_, 32>::collect([0, 1, 2, 3, 4, 5].into_iter());
    /// arr.push_front(6);
    /// assert_eq!(arr.into_iter().collect::<Vec<_>>(), vec![6, 0, 1, 2, 3, 4, 5]);
    /// ```
    pub fn push_front(&mut self, elt: T) {
        // If `end` is already at the end of `rev_vec` then this is a cheap no-op.
        self.rev_vec.truncate(self.end);
        self.rev_vec.push(elt);
        self.end += 1;
    }

    /// Removes and returns the element at the beginning of this array, or
    /// `None` if we're empty.
    ///
    /// Runs in time complexity `O(log self.len())`.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use nickel_lang_funcarray::FunctionalArray;
    /// let mut arr = FunctionalArray::<_, 32>::collect([0, 1, 2, 3, 4, 5].into_iter());
    /// assert_eq!(arr.pop_front(), Some(0));
    /// assert_eq!(arr.into_iter().collect::<Vec<_>>(), vec![1, 2, 3, 4, 5]);
    /// ```
    pub fn pop_front(&mut self) -> Option<T> {
        if self.is_empty() {
            None
        } else {
            self.rev_vec.truncate(self.end);
            self.end -= 1;
            self.rev_vec.pop()
        }
    }

    /// Returns an iterator over references to array elements.
    pub fn iter(&self) -> impl Iterator<Item = &'_ T> {
        self.into_iter()
    }

    /// Prepends another array to the beginning of this array.
    ///
    /// Runs in time complexity `O(other.len() + log self.len())`. In particular,
    /// you should arrange things so that `self` is long and `other` is short.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use nickel_lang_funcarray::FunctionalArray;
    /// let mut arr = FunctionalArray::<_, 32>::collect([0, 1, 2, 3, 4, 5].into_iter());
    /// let other = FunctionalArray::<_, 32>::collect([7, 6].into_iter());
    /// arr.prepend(other);
    /// assert_eq!(arr.into_iter().collect::<Vec<_>>(), vec![7, 6, 0, 1, 2, 3, 4, 5]);
    /// ```
    pub fn prepend(&mut self, other: Self) {
        // If `end` is already at the end of `rev_vec` then this is a cheap no-op.
        self.rev_vec.truncate(self.end);
        self.end += other.len();
        self.rev_vec.extend(other.rev_vec);
    }

    pub fn prepend_iter(&mut self, other: impl DoubleEndedIterator<Item = T>) {
        // If `end` is already at the end of `rev_vec` then this is a cheap no-op.
        self.rev_vec.truncate(self.end);
        let len_before = self.rev_vec.len();
        self.rev_vec.extend(other.rev());
        self.end += self.rev_vec.len() - len_before;
    }

    /// Replace this array by the subslice from index `from` (inclusive) to index `to` (exclusive).
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use nickel_lang_funcarray::FunctionalArray;
    /// let mut arr = FunctionalArray::<_, 32>::collect([0, 1, 2, 3, 4, 5].into_iter());
    /// arr.slice(1, 5);
    /// assert_eq!(arr.clone().into_iter().collect::<Vec<_>>(), vec![1, 2, 3, 4]);
    /// arr.slice(1, 3);
    /// assert_eq!(arr.into_iter().collect::<Vec<_>>(), vec![2, 3]);
    /// ```
    // We can't use the index trait here, because we'd have to return a &Self.
    pub fn slice(&mut self, from: usize, to: usize) {
        assert!(from <= to);
        assert!(to <= self.len());
        let old_end = self.end;
        self.end = old_end - from;
        self.start = old_end - to;
    }
}

impl<T: Clone, const N: usize> IntoIterator for FunctionalArray<T, N> {
    type Item = T;
    type IntoIter = std::iter::Take<RevIntoIter<T, N>>;

    fn into_iter(self) -> Self::IntoIter {
        if let Some(last_idx) = self.end.checked_sub(1) {
            let len = self.len();
            self.rev_vec.into_rev_iter_starting_at(last_idx).take(len)
        } else {
            // Avoid `into_rev_iter_starting_at` because it's inclusive of its
            // index and will fail for an empty vec.
            self.rev_vec.into_rev_iter().take(0)
        }
    }
}

impl<'a, T: Clone, const N: usize> IntoIterator for &'a FunctionalArray<T, N> {
    type Item = &'a T;
    type IntoIter = std::iter::Take<RevIter<'a, T, N>>;

    fn into_iter(self) -> Self::IntoIter {
        if let Some(last_idx) = self.end.checked_sub(1) {
            self.rev_vec.rev_iter_starting_at(last_idx).take(self.len())
        } else {
            // Avoid `rev_iter_starting_at` because it's inclusive of its
            // index and will fail for an empty vec.
            self.rev_vec.rev_iter().take(0)
        }
    }
}

impl<T: Clone, const N: usize> Index<usize> for FunctionalArray<T, N> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        self.get(index).expect("index out of bounds")
    }
}
