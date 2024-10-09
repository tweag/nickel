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
// It would be nice to force `N` to be a power of 2 (for example, by
// parametrizing with `B` and setting `N = 1 << B`). This sort of needs
// the `generic_const_exprs` feature to work, though.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Node<T, const N: usize> {
    Leaf { data: Chunk<T, N> },
    Interior { children: Interior<T, N> },
}

fn extract_index<const N: usize>(idx: usize, height: u8) -> usize {
    let shifted: usize = idx >> (N.ilog2() * u32::from(height));
    shifted & (N - 1)
}

impl<T: Clone, const N: usize> Node<T, N>
where
    Const<N>: ValidBranchingConstant,
{
    fn len(&self) -> usize {
        match self {
            Node::Leaf { data } => data.len(),
            Node::Interior { children } => {
                // TODO: this could be faster if we also stored the tree height. We could calculate
                // the size of the packed part without iterating over it.
                children.iter().map(|c| c.len()).sum()
            }
        }
    }

    pub fn get(&self, height: u8, idx: usize) -> Option<&T> {
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

    // `idx` can point at an existing index, or one past the end.
    pub fn set(&mut self, elt: T, idx: usize, height: u8) {
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
                    Rc::make_mut(&mut children[bucket_idx]).set(elt, idx, height - 1);
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

    // Returns true if popping made this subtree empty.
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

    // Assumes that the length is less than this node's current length.
    fn truncate(&mut self, len: usize, height: u8) {
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
                    Rc::make_mut(&mut children[num_full_children]).truncate(extra, height - 1);
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
    // TODO: we currently allocate for an empty vector. Try to avoid it.
    root: Rc<Node<T, N>>,
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

// We haven't implemented DoubleEndedIterator; implementing a reverse iterator
// -- while less flexible -- is simpler. The extra thing you'd get from
// DoubleEndedIterator is the ability to alternate taking from the beginning and
// the end.
#[derive(Debug, Clone)]
pub struct RevIter<'a, T, const N: usize>
where
    Const<N>: ValidBranchingConstant,
{
    inner: Iter<'a, T, N>,
}

impl<'a, T, const N: usize> Iterator for RevIter<'a, T, N>
where
    Const<N>: ValidBranchingConstant,
{
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(ret) = self.inner.leaf.next_back() {
            Some(ret)
        } else {
            let height = self.inner.stack.len();
            let mut next = loop {
                match self.inner.stack.last_mut() {
                    Some(iter) => {
                        if let Some(next) = iter.next_back() {
                            break next;
                        } else {
                            self.inner.stack.pop();
                        }
                    }
                    None => {
                        return None;
                    }
                }
            };

            let cur_len = self.inner.stack.len();
            for _ in cur_len..height {
                let Node::Interior { children } = next.as_ref() else {
                    unreachable!();
                };
                let mut children_iter = children.iter();
                next = children_iter.next_back().expect("empty interior node");
                self.inner.stack.push(children_iter);
            }

            let Node::Leaf { data } = next.as_ref() else {
                unreachable!();
            };
            debug_assert!(!data.is_empty());
            self.inner.leaf = data.iter();
            self.inner.leaf.next_back()
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

pub struct RevIntoIter<T, const N: usize> {
    inner: IntoIter<T, N>,
}

impl<T: Clone, const N: usize> Iterator for RevIntoIter<T, N> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(ret) = self.inner.leaf.next_back() {
            Some(ret)
        } else {
            let height = self.inner.stack.len();
            let mut next = loop {
                match self.inner.stack.last_mut() {
                    Some(iter) => {
                        if let Some(next) = iter.next_back() {
                            break next;
                        } else {
                            self.inner.stack.pop();
                        }
                    }
                    None => {
                        return None;
                    }
                }
            };

            let cur_len = self.inner.stack.len();
            for _ in cur_len..height {
                let Node::Interior { children } = Rc::unwrap_or_clone(next) else {
                    unreachable!();
                };
                let mut children_iter = children.into_iter();
                next = children_iter.next_back().expect("empty interior node");
                self.inner.stack.push(children_iter);
            }

            let Node::Leaf { data } = Rc::unwrap_or_clone(next) else {
                unreachable!();
            };
            debug_assert!(!data.is_empty());
            self.inner.leaf = data.into_iter();
            self.inner.leaf.next_back()
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

        while iter.peek().is_some() {
            let consumed = match Rc::make_mut(&mut self.root) {
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
            root: Rc::new(Node::Leaf { data: Chunk::new() }),
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

        is_packed_rec(&self.root, true)
    }

    pub fn check_invariants(&self) {
        assert!(self.is_packed());
        assert_eq!(self.length, self.root.len());
        if let Node::Interior { children } = self.root.as_ref() {
            assert!(children.len() > 1);
        }
        assert_eq!(self.height, height_for_length::<N>(self.len()));
    }

    fn is_full(&self) -> bool {
        self.length == N.pow(u32::from(self.height) + 1)
    }

    pub fn get(&self, idx: usize) -> Option<&T> {
        self.root.get(self.height, idx)
    }

    // Increases the height of the tree by one, temporarily breaking the invariant that
    // the root must have at least two children.
    fn add_level(&mut self) {
        let old_root = std::mem::replace(
            &mut self.root,
            Rc::new(Node::Interior {
                children: Chunk::new(),
            }),
        );

        // TODO: maybe we can avoid the make_mut and the fallible destructuring?
        // It seems a little tricky to do so without increasing some ref-counts.
        let Node::Interior { children } = Rc::make_mut(&mut self.root) else {
            unreachable!();
        };
        children.push_back(old_root);
        self.height += 1;
    }

    pub fn push(&mut self, elt: T) {
        if self.is_full() {
            self.add_level();
        }
        let idx = self.len();
        Rc::make_mut(&mut self.root).set(elt, idx, self.height);
        self.length += 1;
    }

    pub fn pop(&mut self) -> Option<T> {
        if self.is_empty() {
            None
        } else {
            let root_mut = Rc::make_mut(&mut self.root);
            let (ret, _empty) = root_mut.pop();
            self.length -= 1;

            // If we've shrunk the root down to a single child, reduce the tree height by 1.
            if let Node::Interior { children } = root_mut {
                if children.len() == 1 {
                    self.root = children.pop_back();
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
            let mut new_root = &self.root;
            for _ in new_height..self.height {
                let Node::Interior { children } = new_root.as_ref() else {
                    unreachable!();
                };
                new_root = children.first().expect("empty interior node");
            }
            self.root = Rc::clone(new_root);
            self.height = new_height;
        }

        Rc::make_mut(&mut self.root).truncate(len, self.height);

        self.length = len;
    }

    pub fn iter(&self) -> Iter<'_, T, N> {
        self.into_iter()
    }

    pub fn rev_iter(&self) -> RevIter<'_, T, N> {
        let mut stack = Vec::with_capacity(self.height.into());
        let mut node = self.root.as_ref();

        while let Node::Interior { children } = node {
            let mut node_iter = children.iter();
            node = node_iter.next_back().expect("empty interior node");
            stack.push(node_iter);
        }

        let Node::Leaf { data } = node else {
            unreachable!();
        };
        RevIter {
            inner: Iter {
                stack,
                leaf: data.iter(),
            },
        }
    }

    pub fn rev_iter_starting_at(&self, idx: usize) -> RevIter<'_, T, N> {
        let mut stack = Vec::with_capacity(self.height.into());
        let mut node = self.root.as_ref();
        let mut height = self.height;

        while let Node::Interior { children } = node {
            let bucket_idx = extract_index::<N>(idx, height);
            let mut node_iter = children[..=bucket_idx].iter();
            node = node_iter.next_back().expect("empty interior node");
            stack.push(node_iter);

            height = height.checked_sub(1).expect("invalid height");
        }

        let Node::Leaf { data } = node else {
            unreachable!();
        };
        RevIter {
            inner: Iter {
                stack,
                leaf: data[..=(idx & (N - 1))].iter(),
            },
        }
    }

    pub fn into_rev_iter(self) -> RevIntoIter<T, N> {
        let mut stack = Vec::with_capacity(self.height.into());
        let mut node = Rc::unwrap_or_clone(self.root);

        while let Node::Interior { children } = node {
            let mut node_iter = children.into_iter();
            node = Rc::unwrap_or_clone(node_iter.next_back().expect("empty interior node"));
            stack.push(node_iter);
        }

        let Node::Leaf { data } = node else {
            unreachable!();
        };
        RevIntoIter {
            inner: IntoIter {
                stack,
                leaf: data.into_iter(),
            },
        }
    }

    pub fn into_rev_iter_starting_at(self, mut idx: usize) -> RevIntoIter<T, N> {
        let mut stack = Vec::with_capacity(self.height.into());
        let mut node = Rc::unwrap_or_clone(self.root);
        let mut height = self.height;

        while let Node::Interior { mut children } = node {
            let bucket_idx = extract_index::<N>(idx, height);
            children.drop_right(bucket_idx + 1);
            let mut node_iter = children.into_iter();
            node = Rc::unwrap_or_clone(node_iter.next_back().expect("empty interior node"));
            stack.push(node_iter);

            height = height.checked_sub(1).expect("invalid height");
        }

        let Node::Leaf { mut data } = node else {
            unreachable!();
        };
        idx &= N - 1;
        data.drop_right(idx + 1);
        RevIntoIter {
            inner: IntoIter {
                stack,
                leaf: data.into_iter(),
            },
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
        let mut node = self.root.as_ref();

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
        let mut node = Rc::unwrap_or_clone(self.root);

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
