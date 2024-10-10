use std::ops::Index;

use crate::{
    vector::{IntoIter, Iter},
    Const, ValidBranchingConstant,
};

use super::Vector;

/// A sliced [`Vector`].
///
/// This is implemented internally as a tree, and the parameter `N` controls its
/// branching factor. For performance, it should always be a power of 2. Values
/// between `8` and `64` are pretty reasonable.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Slice<T, const N: usize>
where
    Const<N>: ValidBranchingConstant,
{
    vec: Vector<T, N>,
    // Our slice involves the range of indices [start, end), like most slicing.
    // But since we work in reverse, our "first" element is at `end - 1`.
    start: usize,
    end: usize,
}

impl<T, const N: usize> Default for Slice<T, N>
where
    Const<N>: ValidBranchingConstant,
{
    fn default() -> Self {
        Slice {
            vec: Default::default(),
            start: 0,
            end: 0,
        }
    }
}

impl<T: Clone + serde::Serialize, const N: usize> serde::Serialize for Slice<T, N>
where
    Const<N>: ValidBranchingConstant,
{
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
    for Slice<T, N>
where
    Const<N>: ValidBranchingConstant,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let vec: Vec<T> = Vec::deserialize(deserializer)?;
        Ok(vec.into_iter().collect())
    }
}

impl<T: Clone, const N: usize> Slice<T, N>
where
    Const<N>: ValidBranchingConstant,
{
    /// The number of elements in this array.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use nickel_lang_vector::Slice;
    /// let arr = Slice::<_, 32>::from_iter([0, 1, 2, 3, 4, 5]);
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
    /// # use nickel_lang_vector::Slice;
    /// let arr = Slice::<_, 32>::from_iter([0, 1, 2, 3, 4, 5]);
    /// assert!(!arr.is_empty());
    /// assert!(Slice::<i32, 32>::default().is_empty());
    /// ```
    pub fn is_empty(&self) -> bool {
        self.end == self.start
    }

    /// Gets an element at a given index, or `None` if `idx` is out-of-bounds.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use nickel_lang_vector::Slice;
    /// let arr = Slice::<_, 32>::from_iter([0, 1, 2, 3, 4, 5]);
    /// assert_eq!(arr.get(2), Some(&2));
    /// assert_eq!(arr.get(6), None);
    /// ```
    pub fn get(&self, idx: usize) -> Option<&T> {
        self.start.checked_add(idx).and_then(|i| self.vec.get(i))
    }

    /// Adds an element to the end of this array.
    ///
    /// Runs in time complexity `O(log n)` where `n` is the array length.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use nickel_lang_vector::Slice;
    /// let mut arr = Slice::<_, 32>::from_iter([0, 1, 2, 3, 4, 5]);
    /// arr.push(6);
    /// assert_eq!(arr.into_iter().collect::<Vec<_>>(), vec![0, 1, 2, 3, 4, 5, 6]);
    /// ```
    pub fn push(&mut self, elt: T) {
        // If `end` is already at the end of `vec` then this is a cheap no-op.
        self.vec.truncate(self.end);
        self.vec.push(elt);
        self.end += 1;
    }

    /// Removes and returns the element at the end of this array, or
    /// `None` if we're empty.
    ///
    /// Runs in time complexity `O(log self.len())`.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use nickel_lang_vector::Slice;
    /// let mut arr = Slice::<_, 32>::from_iter([0, 1, 2, 3, 4, 5]);
    /// assert_eq!(arr.pop(), Some(5));
    /// assert_eq!(arr.into_iter().collect::<Vec<_>>(), vec![0, 1, 2, 3, 4]);
    /// ```
    pub fn pop(&mut self) -> Option<T> {
        if self.is_empty() {
            None
        } else {
            self.vec.truncate(self.end);
            self.end -= 1;
            self.vec.pop()
        }
    }

    /// Returns an iterator over references to array elements.
    pub fn iter(&self) -> impl Iterator<Item = &'_ T> {
        self.into_iter()
    }

    /// Replace this array by the subslice from index `from` (inclusive) to index `to` (exclusive).
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use nickel_lang_vector::Slice;
    /// let mut arr = Slice::<_, 32>::from_iter([0, 1, 2, 3, 4, 5]);
    /// arr.slice(1, 5);
    /// assert_eq!(arr.clone().into_iter().collect::<Vec<_>>(), vec![1, 2, 3, 4]);
    /// arr.slice(1, 3);
    /// assert_eq!(arr.into_iter().collect::<Vec<_>>(), vec![2, 3]);
    /// ```
    // We can't use the index trait here, because we'd have to return a &Self.
    pub fn slice(&mut self, from: usize, to: usize) {
        assert!(from <= to);
        assert!(to <= self.len());
        self.end = self.start + to;
        self.start += from;
    }
}

impl<T: Clone, const N: usize> IntoIterator for Slice<T, N>
where
    Const<N>: ValidBranchingConstant,
{
    type Item = T;
    type IntoIter = std::iter::Take<IntoIter<T, N>>;

    fn into_iter(self) -> Self::IntoIter {
        let len = self.len();
        self.vec.into_iter_starting_at(self.start).take(len)
    }
}

impl<'a, T: Clone, const N: usize> IntoIterator for &'a Slice<T, N>
where
    Const<N>: ValidBranchingConstant,
{
    type Item = &'a T;
    type IntoIter = std::iter::Take<Iter<'a, T, N>>;

    fn into_iter(self) -> Self::IntoIter {
        let len = self.len();
        self.vec.iter_starting_at(self.start).take(len)
    }
}

impl<T: Clone, const N: usize> Extend<T> for Slice<T, N>
where
    Const<N>: ValidBranchingConstant,
{
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        self.vec.truncate(self.end);
        self.vec.extend(iter);
        self.end = self.vec.len();
    }
}

impl<T: Clone, const N: usize> Index<usize> for Slice<T, N>
where
    Const<N>: ValidBranchingConstant,
{
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        self.get(index).expect("index out of bounds")
    }
}

impl<T: Clone, const N: usize> FromIterator<T> for Slice<T, N>
where
    Const<N>: ValidBranchingConstant,
{
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let vec: Vector<_, N> = iter.into_iter().collect();
        Self {
            start: 0,
            end: vec.len(),
            vec,
        }
    }
}
