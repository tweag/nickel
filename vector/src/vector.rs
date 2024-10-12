use std::{iter::Peekable, ops::Index, rc::Rc};

use imbl_sized_chunks::Chunk;

use crate::{Const, ValidBranchingConstant};

// In principle we could decouple the size of the interior nodes from the size of the leaves.
// This might make sense when `T` is large, because the interior nodes are always pointer-sized.
type Interior<T, const N: usize> = Chunk<Rc<Node<T, N>>, N>;
type ChunkIter<T, const N: usize> = imbl_sized_chunks::sized_chunk::Iter<T, N>;
type InteriorChunkIter<T, const N: usize> = ChunkIter<Rc<Node<T, N>>, N>;

// Can we improve the memory layout? We like N to be a power of 2 for
// performance (because it allows adjusting the indices using just bitwise
// operations), but it would also be cool if the total size of the node were a
// nice round number (so it would exactly fit in a small integer number of cache
// lines). The discriminant makes it hard to have both of these at once.
// Since we always know (based on the tree height) which type we *expect* a node
// to have, we could use a `union` instead of an `enum` (at the cost of lots of
// unsafe code).
//
// `N` must be a power of 2; this is important for efficiency because it allows
// the use of bitwise operations for a lot of things. I tried allowing `N` to
// be arbitrary, hoping that the compiler would be smart enough to do the fast
// thing when `N` is a power of 2. It wasn't.
//
// It would be nice to encode the power-of-2 restriction more efficiently (for
// example, by parametrizing with `B` and setting `N = 1 << B`). This sort of
// needs the `generic_const_exprs` feature to work, though.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Node<T, const N: usize> {
    Leaf { data: Chunk<T, N> },
    Interior { children: Interior<T, N> },
}

/// `idx` is the global index into the root node, and we are some
/// possibly-intermediate node at height `height` (where the leaf is at height
/// zero). Which of our children does the global index belong to?
fn extract_index<const N: usize>(idx: usize, height: u8) -> usize {
    let shifted: usize = idx >> (N.ilog2() * u32::from(height));
    shifted & (N - 1)
}

impl<T: Clone, const N: usize> Node<T, N>
where
    Const<N>: ValidBranchingConstant,
{
    /// An inefficient but correct (and simple) method for computing the length
    /// of this subtree. We cache the length in the top-level vector, so this is
    /// only used for sanity-checks.
    fn len(&self) -> usize {
        match self {
            Node::Leaf { data } => data.len(),
            Node::Interior { children } => {
                // This could be faster if we used the tree height to compute
                // the size of the packed part of the tree. But this function is
                // only used for checking invariants, so speed isn't important.
                children.iter().map(|c| c.len()).sum()
            }
        }
    }

    /// If this node is at height `height`, try to get the element at the given
    /// index.
    fn get(&self, height: u8, idx: usize) -> Option<&T> {
        match self {
            Node::Leaf { data } => {
                debug_assert_eq!(height, 0);
                data.get(idx & (N - 1))
            }
            Node::Interior { children } => {
                let bucket_idx = extract_index::<N>(idx, height);
                children
                    .get(bucket_idx)
                    .and_then(|child| child.get(height - 1, idx))
            }
        }
    }

    /// Set the element at the given index.
    ///
    /// The index is allowed to point to the uninitialized slot just past the
    /// end of the initialized part, but it must point to a valid index within
    /// this node (i.e. if this node is full then it can't point past the end).
    ///
    /// Panics if the index is invalid.
    fn set(&mut self, height: u8, idx: usize, elt: T) {
        match self {
            Node::Leaf { data } => {
                let idx = idx & (N - 1);
                debug_assert_eq!(height, 0);
                debug_assert!(idx <= data.len());
                if idx < data.len() {
                    data.set(idx, elt);
                } else {
                    data.push_back(elt);
                }
            }
            Node::Interior { children } => {
                let bucket_idx = extract_index::<N>(idx, height);
                assert!(height >= 1);
                assert!(bucket_idx <= children.len());
                if bucket_idx < children.len() {
                    Rc::make_mut(&mut children[bucket_idx]).set(height - 1, idx, elt);
                } else {
                    let mut leaf = Chunk::new();
                    leaf.push_back(elt);

                    let mut child = Node::Leaf { data: leaf };
                    for _ in 1..height {
                        let mut children = Chunk::new();
                        children.push_back(Rc::new(child));
                        child = Node::Interior { children };
                    }
                    children.push_back(Rc::new(child));
                }
            }
        }
    }

    /// Deletes and returns the last element of this subtree (which is assumed to be non-empty).
    ///
    /// Returns true if popping made this subtree empty.
    fn pop(&mut self) -> (T, bool) {
        match self {
            Node::Leaf { data } => {
                debug_assert!(!data.is_empty());
                let ret = data.pop_back();
                (ret, data.is_empty())
            }
            Node::Interior { children } => {
                let (ret, child_empty) =
                    Rc::make_mut(children.last_mut().expect("empty interior node")).pop();
                if child_empty {
                    children.pop_back();
                }
                (ret, children.is_empty())
            }
        }
    }

    /// Shrinks the length of this subtree to `len`.
    ///
    /// Assumes that the length is less than this node's current length.
    fn truncate(&mut self, height: u8, len: usize) {
        match self {
            Node::Leaf { data } => {
                debug_assert!(height == 0);
                data.drop_right(len);
            }
            Node::Interior { children } => {
                // If `len` is small enough, we may just want to drop some children
                // and their entire subtrees.
                let max_child_len = N.pow(u32::from(height));
                let num_full_children = len / max_child_len;
                let extra = len % max_child_len;
                if extra > 0 {
                    children.drop_right(num_full_children + 1);
                    Rc::make_mut(&mut children[num_full_children]).truncate(height - 1, extra);
                } else {
                    children.drop_right(num_full_children);
                }
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Vector<T, const N: usize>
where
    Const<N>: ValidBranchingConstant,
{
    root: Option<Rc<Node<T, N>>>,
    length: usize,
    // TODO: could save some space by taking 8 bits out of length
    height: u8,
}

#[derive(Debug, Clone)]
pub struct Iter<'a, T, const N: usize>
where
    Const<N>: ValidBranchingConstant,
{
    stack: Vec<std::slice::Iter<'a, Rc<Node<T, N>>>>,
    leaf: std::slice::Iter<'a, T>,
}

impl<'a, T, const N: usize> Iterator for Iter<'a, T, N>
where
    Const<N>: ValidBranchingConstant,
{
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(ret) = self.leaf.next() {
            Some(ret)
        } else {
            let height = self.stack.len();
            let mut next = loop {
                match self.stack.last_mut() {
                    Some(iter) => {
                        if let Some(next) = iter.next() {
                            break next;
                        } else {
                            self.stack.pop();
                        }
                    }
                    None => {
                        return None;
                    }
                }
            };

            let cur_len = self.stack.len();
            for _ in cur_len..height {
                let Node::Interior { children } = next.as_ref() else {
                    unreachable!();
                };
                let mut children_iter = children.iter();
                next = children_iter.next().expect("empty interior node");
                self.stack.push(children_iter);
            }

            let Node::Leaf { data } = next.as_ref() else {
                unreachable!();
            };
            debug_assert!(!data.is_empty());
            self.leaf = data.iter();
            self.leaf.next()
        }
    }
}

pub struct IntoIter<T, const N: usize> {
    stack: Vec<InteriorChunkIter<T, N>>,
    leaf: ChunkIter<T, N>,
}

impl<T: Clone, const N: usize> Iterator for IntoIter<T, N> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(ret) = self.leaf.next() {
            Some(ret)
        } else {
            let height = self.stack.len();
            let mut next = loop {
                match self.stack.last_mut() {
                    Some(iter) => {
                        if let Some(next) = iter.next() {
                            break next;
                        } else {
                            self.stack.pop();
                        }
                    }
                    None => {
                        return None;
                    }
                }
            };

            let cur_len = self.stack.len();
            for _ in cur_len..height {
                let Node::Interior { children } = Rc::unwrap_or_clone(next) else {
                    unreachable!();
                };
                let mut children_iter = children.into_iter();
                next = children_iter.next().expect("empty interior node");
                self.stack.push(children_iter);
            }

            let Node::Leaf { data } = Rc::unwrap_or_clone(next) else {
                unreachable!();
            };
            debug_assert!(!data.is_empty());
            self.leaf = data.into_iter();
            self.leaf.next()
        }
    }
}

impl<T: Clone, const N: usize> Extend<T> for Vector<T, N>
where
    Const<N>: ValidBranchingConstant,
{
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        // Make the iterator peekable, because we need to check if there's an
        // element remaining before we mutate the tree to make room for it.
        let mut iter = iter.into_iter().peekable();

        // Extends a node from an iterator, but does not increase the height of
        // the node. If the node fills up, the iterator may not be fully consumed.
        //
        // Returns the number of elements consumed from the iterator.
        fn extend_rec<T: Clone, I: Iterator<Item = T>, const N: usize>(
            iter: &mut Peekable<I>,
            node: &mut Interior<T, N>,
            height: u8,
        ) -> usize {
            debug_assert!(height >= 1);
            let mut consumed = 0;

            if height == 1 {
                // If there's a leaf that isn't filled, fill it.
                if let Some(last_child) = node.last_mut() {
                    // Usually, we assert that there's a last child because the
                    // interior nodes are guaranteed to be non-empty. But within
                    // this function we sometimes create empty interior nodes to
                    // be filled later.
                    // TODO: can avoid the clone if it's already full
                    let Node::Leaf { data } = Rc::make_mut(last_child) else {
                        unreachable!();
                    };
                    let old_len = data.len();
                    data.extend(iter.take(N - data.len()));
                    consumed += data.len() - old_len;
                }

                while !node.is_full() && iter.peek().is_some() {
                    let data: Chunk<T, N> = iter.take(N).collect();
                    consumed += data.len();
                    node.push_back(Rc::new(Node::Leaf { data }));
                }
            } else {
                if let Some(child) = node.last_mut() {
                    let Node::Interior { children } = Rc::make_mut(child) else {
                        unreachable!();
                    };

                    consumed += extend_rec(iter, children, height - 1);
                }

                while !node.is_full() && iter.peek().is_some() {
                    let mut new_child: Interior<T, N> = Chunk::new();
                    consumed += extend_rec(iter, &mut new_child, height - 1);
                    node.push_back(Rc::new(Node::Interior {
                        children: new_child,
                    }));
                }
            }

            consumed
        }

        if iter.peek().is_some() && self.root.is_none() {
            self.root = Some(Rc::new(Node::Leaf {
                data: Chunk::default(),
            }));
        }
        while iter.peek().is_some() {
            // Unwrap: we ensured that if the iterator has anything, the root is present.
            let consumed = match Rc::make_mut(self.root.as_mut().unwrap()) {
                Node::Leaf { data } => {
                    let old_len = data.len();
                    data.extend((&mut iter).take(N - data.len()));
                    data.len() - old_len
                }
                Node::Interior { children } => extend_rec(&mut iter, children, self.height),
            };
            self.length += consumed;

            // Check if there's more left in the iterator, and add a level if there is.
            if iter.peek().is_some() {
                self.add_level();
            }
        }
    }
}

fn height_for_length<const N: usize>(length: usize) -> u8 {
    // Length zero through N has height zero, length N + 1 through N^2 has height 1, etc.
    // The unwrap is fine unless someone has a usize that's more than 256 bits.
    length.saturating_sub(1).max(1).ilog(N).try_into().unwrap()
}

impl<T, const N: usize> Vector<T, N>
where
    Const<N>: ValidBranchingConstant,
{
    pub fn new() -> Self {
        Self {
            root: None,
            length: 0,
            height: 0,
        }
    }

    pub fn len(&self) -> usize {
        self.length
    }

    pub fn is_empty(&self) -> bool {
        self.length == 0
    }
}

impl<T: Clone, const N: usize> Vector<T, N>
where
    Const<N>: ValidBranchingConstant,
{
    fn is_packed(&self) -> bool {
        fn is_packed_rec<T: Clone, const N: usize>(n: &Node<T, N>, right_most: bool) -> bool {
            match n {
                Node::Leaf { data } => data.is_full() || right_most,
                Node::Interior { children } => {
                    if let Some((tail, others)) = children.split_last() {
                        others.iter().all(|n| is_packed_rec(n, false)) && is_packed_rec(tail, true)
                    } else {
                        debug_assert!(false, "empty node");
                        false
                    }
                }
            }
        }

        match &self.root {
            None => true,
            Some(root) => is_packed_rec(root, true),
        }
    }

    pub fn check_invariants(&self) {
        assert!(self.is_packed());
        assert_eq!(self.length, self.root.as_ref().map_or(0, |root| root.len()));
        if let Some(root) = self.root.as_ref() {
            if let Node::Interior { children } = root.as_ref() {
                assert!(children.len() > 1);
            }
        }
        assert_eq!(self.height, height_for_length::<N>(self.len()));
    }

    fn is_full(&self) -> bool {
        self.root.is_none() || self.length == N.pow(u32::from(self.height) + 1)
    }

    pub fn get(&self, idx: usize) -> Option<&T> {
        self.root.as_ref().and_then(|r| r.get(self.height, idx))
    }

    // Increases the height of the tree by one, temporarily breaking the invariant that
    // the root must have at least two children.
    fn add_level(&mut self) {
        match &mut self.root {
            None => {
                // We don't increment height in this case: height zero is used for both
                // the empty vector and a vector with just one leaf.
                // (Maybe we should use height 1 in the latter case?)
                self.root = Some(Rc::new(Node::Leaf { data: Chunk::new() }));
            }
            Some(root) => {
                let old_root = std::mem::replace(
                    root,
                    Rc::new(Node::Interior {
                        children: Chunk::new(),
                    }),
                );

                // TODO: maybe we can avoid the make_mut and the fallible destructuring?
                // It seems a little tricky to do so without increasing some ref-counts.
                let Node::Interior { children } = Rc::make_mut(root) else {
                    unreachable!();
                };
                children.push_back(old_root);
                self.height += 1;
            }
        }
    }

    pub fn push(&mut self, elt: T) {
        if self.is_full() {
            self.add_level();
        }
        let idx = self.len();
        // unwrap: self.add_level ensures that the root is non-empty
        Rc::make_mut(self.root.as_mut().unwrap()).set(self.height, idx, elt);
        self.length += 1;
    }

    pub fn pop(&mut self) -> Option<T> {
        if self.is_empty() {
            None
        } else {
            // Unwrap: if we aren't empty, we have a root.
            let root_mut = Rc::make_mut(self.root.as_mut().unwrap());
            let (ret, _empty) = root_mut.pop();
            self.length -= 1;

            // If we've shrunk the root down to a single child, reduce the tree height by 1.
            if let Node::Interior { children } = root_mut {
                if children.len() == 1 {
                    self.root = Some(children.pop_back());
                    self.height -= 1;
                }
            }
            Some(ret)
        }
    }

    pub fn truncate(&mut self, len: usize) {
        if len >= self.length {
            return;
        }

        let new_height = height_for_length::<N>(len);
        if new_height < self.height {
            // unwrap: if we were empty, we would have returned at the `len >= self.length` check.
            let mut new_root = self.root.as_ref().unwrap();
            for _ in new_height..self.height {
                let Node::Interior { children } = new_root.as_ref() else {
                    unreachable!();
                };
                new_root = children.first().expect("empty interior node");
            }
            self.root = Some(Rc::clone(new_root));
            self.height = new_height;
        }

        // unwrap: if we were empty, we would have returned at the `len >= self.length` check.
        Rc::make_mut(self.root.as_mut().unwrap()).truncate(self.height, len);

        self.length = len;
    }

    pub fn iter(&self) -> Iter<'_, T, N> {
        self.into_iter()
    }

    pub fn iter_starting_at(&self, idx: usize) -> Iter<'_, T, N> {
        if idx == self.len() {
            return Iter {
                stack: Vec::new(),
                leaf: [].iter(),
            };
        }
        if idx > self.len() {
            panic!("out of bounds");
        }

        let mut stack = Vec::with_capacity(self.height.into());
        // unwrap: if we got past the initial tests on `idx`, we must be non-empty
        // and so we have a root
        let mut node = self.root.as_ref().unwrap().as_ref();
        let mut height = self.height;

        while let Node::Interior { children } = node {
            let bucket_idx = extract_index::<N>(idx, height);
            let mut node_iter = children[bucket_idx..].iter();

            // expect: we've checked that `idx` is strictly less than the length,
            // so this interior iterator should be non-empty also.
            node = node_iter.next().expect("empty interior node");
            stack.push(node_iter);

            height = height.checked_sub(1).expect("invalid height");
        }

        let Node::Leaf { data } = node else {
            unreachable!();
        };
        Iter {
            stack,
            leaf: data[(idx & (N - 1))..].iter(),
        }
    }

    pub fn into_iter_starting_at(self, mut idx: usize) -> IntoIter<T, N> {
        if idx == self.len() {
            return IntoIter {
                stack: Vec::new(),
                leaf: Chunk::new().into_iter(),
            };
        }
        if idx > self.len() {
            panic!("out of bounds");
        }

        let mut stack = Vec::with_capacity(self.height.into());
        // unwrap: if we got past the initial tests on `idx`, we must be non-empty
        // and so we have a root
        let mut node = Rc::unwrap_or_clone(self.root.unwrap());
        let mut height = self.height;

        while let Node::Interior { mut children } = node {
            let bucket_idx = extract_index::<N>(idx, height);
            children.drop_left(bucket_idx);
            let mut node_iter = children.into_iter();
            node = Rc::unwrap_or_clone(node_iter.next().expect("empty interior node"));
            stack.push(node_iter);

            height = height.checked_sub(1).expect("invalid height");
        }

        let Node::Leaf { mut data } = node else {
            unreachable!();
        };
        idx &= N - 1;
        data.drop_left(idx);
        IntoIter {
            stack,
            leaf: data.into_iter(),
        }
    }
}

impl<'a, T, const N: usize> IntoIterator for &'a Vector<T, N>
where
    Const<N>: ValidBranchingConstant,
{
    type Item = &'a T;
    type IntoIter = Iter<'a, T, N>;

    fn into_iter(self) -> Self::IntoIter {
        let mut stack = Vec::with_capacity(self.height.into());
        let Some(root) = &self.root else {
            return Iter {
                stack,
                leaf: [].iter(),
            };
        };

        let mut node = root.as_ref();
        while let Node::Interior { children } = node {
            let mut node_iter = children.iter();
            node = node_iter.next().expect("empty interior node");
            stack.push(node_iter);
        }

        let Node::Leaf { data } = node else {
            unreachable!();
        };
        Iter {
            stack,
            leaf: data.iter(),
        }
    }
}

impl<T: Clone, const N: usize> IntoIterator for Vector<T, N>
where
    Const<N>: ValidBranchingConstant,
{
    type Item = T;
    type IntoIter = IntoIter<T, N>;

    fn into_iter(self) -> Self::IntoIter {
        let mut stack = Vec::with_capacity(self.height.into());
        let Some(root) = self.root else {
            return IntoIter {
                stack,
                leaf: Chunk::new().into_iter(),
            };
        };

        let mut node = Rc::unwrap_or_clone(root);
        while let Node::Interior { children } = node {
            let mut node_iter = children.into_iter();
            node = Rc::unwrap_or_clone(node_iter.next().expect("empty interior node"));
            stack.push(node_iter);
        }

        let Node::Leaf { data } = node else {
            unreachable!();
        };
        IntoIter {
            stack,
            leaf: data.into_iter(),
        }
    }
}

impl<T, const N: usize> Default for Vector<T, N>
where
    Const<N>: ValidBranchingConstant,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Clone, const N: usize> FromIterator<T> for Vector<T, N>
where
    Const<N>: ValidBranchingConstant,
{
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut ret = Vector::default();
        ret.extend(iter);
        ret
    }
}

impl<T: Clone, const N: usize> Index<usize> for Vector<T, N>
where
    Const<N>: ValidBranchingConstant,
{
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        self.get(index).expect("index out of range")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic() {
        let mut vec = Vector::<u32, 2>::new();
        vec.check_invariants();
        vec.push(1);
        assert_eq!(vec.get(0), Some(&1));
        assert_eq!(vec.get(1), None);
        vec.check_invariants();

        vec.push(2);
        vec.check_invariants();
        vec.push(3);
        vec.check_invariants();
        assert_eq!(vec.get(0), Some(&1));
        assert_eq!(vec.get(1), Some(&2));
        assert_eq!(vec.get(2), Some(&3));
        assert_eq!(vec.get(3), None);

        let mut iter = vec.iter();
        assert_eq!(iter.next(), Some(&1));
        assert_eq!(iter.next(), Some(&2));
        assert_eq!(iter.next(), Some(&3));
        assert_eq!(iter.next(), None);

        assert_eq!(vec.iter().copied().collect::<Vec<_>>(), vec![1, 2, 3]);

        assert_eq!(vec.pop(), Some(3));
        vec.check_invariants();
        vec.push(3);
        vec.check_invariants();

        vec.extend([1, 2, 3]);
        vec.check_invariants();
        let mut iter = vec.iter();
        assert_eq!(iter.next(), Some(&1));
        assert_eq!(iter.next(), Some(&2));
        assert_eq!(iter.next(), Some(&3));
        assert_eq!(iter.next(), Some(&1));
        assert_eq!(iter.next(), Some(&2));
        assert_eq!(iter.next(), Some(&3));
        assert_eq!(iter.next(), None);

        assert_eq!(6, vec.len());
        assert_eq!(
            vec.iter().copied().collect::<Vec<_>>(),
            vec![1, 2, 3, 1, 2, 3]
        );
        assert_eq!(vec.into_iter().collect::<Vec<_>>(), vec![1, 2, 3, 1, 2, 3]);
    }
}
