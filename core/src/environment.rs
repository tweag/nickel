//! An environment for storing variables with scopes.
use std::cell::RefCell;
use std::collections::{hash_map, HashMap};
use std::hash::Hash;
use std::iter::FromIterator;
use std::marker::PhantomData;
use std::ptr::NonNull;
use std::rc::Rc;

use crate::metrics::{increment, sample};

/// An environment as a linked-list of hashmaps.
///
/// Each node of the linked-list corresponds to what is called
/// "a layer", where only the current layer can be modified, the
/// previous ones are only accessible for lookup.
///
/// For the generic parameters, `K` is the type for the environment
/// keys, and `V` are their value.
///
/// The linked list is composed of the current layer and the previous layers.
/// The current layer is stored as an `Rc<Hashmap>`. It is inserted in the previous layers
/// by cloning.
/// The insertion is made by trying to get the current layer as mutable using
/// [`Rc::get_mut`]. If it can, it means it is the only owner of this layer
/// in the environment, allowing it to mutate it. If it cannot, it means that
/// the current has been cloned and inserted in the previous environment already,
/// so it can safely be reset as a new hashmap.
/// The previous layers are set in order from the most recent one to the oldest.
#[derive(Debug, PartialEq)]
pub struct Environment<K: Hash + Eq, V: PartialEq> {
    current: Rc<HashMap<K, V>>,
    previous: RefCell<Option<Rc<Environment<K, V>>>>,
}

impl<K: Hash + Eq, V: PartialEq> Clone for Environment<K, V> {
    /// Clone has to create a new environment, while ensuring that previous
    /// defined layers are accessible but not modifiable anymore.
    /// For that, it checks if the current Environment has already be cloned,
    /// and if it wasn't, it sets the `current` has the new head of `previous`.
    /// Then a clone is an empty `current` and the clone of `self.previous`.
    fn clone(&self) -> Self {
        increment!("Environment::clone");
        if !self.current.is_empty() && !self.was_cloned() {
            sample!(
                "Environment.curr_layer_size_at_clone",
                self.current.len() as f64
            );
            self.previous.replace_with(|old| {
                Some(Rc::new(Environment {
                    current: self.current.clone(),
                    previous: RefCell::new(old.clone()),
                }))
            });
        }
        Self {
            current: Rc::new(HashMap::new()),
            previous: self.previous.clone(),
        }
    }
}

impl<K: Hash + Eq, V: PartialEq> Default for Environment<K, V> {
    fn default() -> Self {
        Self {
            current: Rc::new(HashMap::new()),
            previous: RefCell::new(None),
        }
    }
}

impl<K: Hash + Eq, V: PartialEq> Environment<K, V> {
    /// Creates a new empty Environment.
    pub fn new() -> Self {
        Self::default()
    }

    /// Inserts a key-value pair into the Environment.
    pub fn insert(&mut self, key: K, value: V) {
        increment!("Environment::insert");
        if self.was_cloned() {
            self.current = Rc::new(HashMap::new());
        }
        Rc::get_mut(&mut self.current).unwrap().insert(key, value);
    }

    /// Tries to find the value of a key in the Environment.
    pub fn get(&self, key: &K) -> Option<&V> {
        increment!("Environment::get");

        let mut layer_count = 0;
        let r = self.iter_layers().find_map(|hmap| {
            sample!("Environment.hashmap_size_get", hmap.len() as f64);
            layer_count += 1;
            hmap.get(key)
        });
        sample!("Environment.get_layers_traversed", layer_count as f64);
        r
    }

    /// Creates an iterator that visits all layers from the most recent one to the oldest.
    /// The element iterator type is `Rc<HashMap<K, V>>`.
    pub fn iter_layers(&self) -> EnvLayerIter<'_, K, V> {
        EnvLayerIter {
            env: if !self.was_cloned() {
                Some(NonNull::from(self))
            } else {
                // if was cloned, current is the same as first of previous (that cannot be empty
                // then)
                self.previous
                    .borrow()
                    .as_ref()
                    // SAFETY: created from Rc, so cannot be null
                    .map(|prev| unsafe { NonNull::new_unchecked(Rc::as_ptr(prev) as *mut _) })
            },
            _marker: PhantomData,
        }
    }

    /// Creates an iterator that visits all elements from the Environment, from the oldest layer to
    /// the most recent one. It uses this order, so calling `collect` on this iterator to create a
    /// hashmap would have the same values as the Environment. The element iterator type is `(&'env
    /// K, &'env V)`, with `'env` being the lifetime of the Environment.
    pub fn iter_elems(&self) -> EnvElemIter<'_, K, V> {
        let mut env: Vec<NonNull<HashMap<K, V>>> = self
            .iter_layers()
            // SAFETY: Rc::as_ptr never returnes null
            .map(|hmap| unsafe { NonNull::new_unchecked(Rc::as_ptr(hmap) as *mut _) })
            .collect();
        // SAFETY: by design, env cannot be empty, and coming from an Rc, it is well aligned and initialized
        let current_map = unsafe { env.pop().unwrap().as_ref() }.iter();
        EnvElemIter { env, current_map }
    }

    /// Creates an iterator that visits all elements from the Environment, from the current layer to
    /// the oldest one. If values are present multiple times, only the most recent one appears.
    /// [`iter_elems`] should be preferred, since it does not need to create an intermediary
    /// hashmap. The element iterator type is `(&'env K, &'env V)`, with `'env` being the lifetime
    /// of the Environment.
    ///
    /// [`iter_elems`]: Environment::iter_elems
    ///
    pub fn iter(&self) -> EnvIter<'_, K, V> {
        EnvIter {
            collapsed_map: self.iter_elems().collect::<HashMap<_, _>>().into_iter(),
        }
    }

    /// Checks if `current` has been cloned. If it has, it is present both in current and in
    /// previous, making it Rc strong count bigger than 1.
    fn was_cloned(&self) -> bool {
        Rc::strong_count(&self.current) > 1
    }

    /// Checks quickly if two environments are obviously equal (when their components are
    /// physically equal as pointers or obviously equal such as being both empty).
    pub(crate) fn ptr_eq(this: &Self, that: &Self) -> bool {
        let prev_layers_eq = match (&*this.previous.borrow(), &*that.previous.borrow()) {
            (Some(ptr_this), Some(ptr_that)) => Rc::ptr_eq(ptr_this, ptr_that),
            (None, None) => true,
            _ => false,
        };

        let curr_layers_eq = (this.current.is_empty() && that.current.is_empty())
            || Rc::ptr_eq(&this.current, &that.current);

        prev_layers_eq && curr_layers_eq
    }
}

impl<K: Hash + Eq, V: PartialEq> FromIterator<(K, V)> for Environment<K, V> {
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        Self {
            current: Rc::new(HashMap::from_iter(iter)),
            previous: RefCell::new(None),
        }
    }
}

impl<K: Hash + Eq, V: PartialEq> Extend<(K, V)> for Environment<K, V> {
    fn extend<T: IntoIterator<Item = (K, V)>>(&mut self, iter: T) {
        // if can mut. borrow current, then we just extend, otherwise it means
        // it was cloned, and we recreate a new map from iter for current
        match Rc::get_mut(&mut self.current) {
            Some(current) => current.extend(iter),
            None => self.current = Rc::new(HashMap::from_iter(iter)),
        }
    }
}

/// An iterator over the layers of `Environment`.
///
/// Created by the [`iter_layers`] method on [`Environment`].
///
/// [`iter_layers`]: Environment::iter_layers
///
pub struct EnvLayerIter<'a, K: 'a + Hash + Eq, V: 'a + PartialEq> {
    env: Option<NonNull<Environment<K, V>>>,
    _marker: PhantomData<&'a Environment<K, V>>,
}

impl<'a, K: 'a + Hash + Eq, V: 'a + PartialEq> Iterator for EnvLayerIter<'a, K, V> {
    type Item = &'a Rc<HashMap<K, V>>;

    fn next(&mut self) -> Option<Self::Item> {
        // SAFETY: NonNull being in an option, we know it cannot be null and can be dereferenceable
        self.env.map(|env| unsafe {
            let res = &env.as_ref().current;
            self.env = env
                .as_ref()
                .previous
                .borrow()
                .as_ref()
                // SAFETY: can safely create NonNull from Rc
                .map(|prev| NonNull::new_unchecked(Rc::as_ptr(prev) as *mut _));
            res
        })
    }
}

/// An iterator over all the elements inside the `Environment`, from the oldest layer to the current
/// one.
///
/// Created by the [`Environment::iter_elems`] method.
///
pub struct EnvElemIter<'a, K: 'a + Hash + Eq, V: 'a + PartialEq> {
    env: Vec<NonNull<HashMap<K, V>>>,
    current_map: std::collections::hash_map::Iter<'a, K, V>,
}

impl<'a, K: 'a + Hash + Eq, V: 'a + PartialEq> Iterator for EnvElemIter<'a, K, V> {
    type Item = (&'a K, &'a V);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.current_map.next() {
                Some(res) => return Some(res),
                None => self.current_map = unsafe { self.env.pop()?.as_ref() }.iter(),
            }
        }
    }
}

/// An iterator over the elements of an `Environment`, from current layer to oldest one.
/// Keys are guaranteed to appear only once, with actual value.
///
/// Created by the [`iter`] method on [`Environment`].
///
/// [`iter`]: Environment::iter
///
pub struct EnvIter<'a, K: 'a + Hash + Eq, V: 'a + PartialEq> {
    collapsed_map: hash_map::IntoIter<&'a K, &'a V>,
}

impl<'a, K: 'a + Hash + Eq, V: 'a + PartialEq> Iterator for EnvIter<'a, K, V> {
    type Item = (&'a K, &'a V);

    fn next(&mut self) -> Option<Self::Item> {
        self.collapsed_map.next()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    impl<K: Hash + Eq, V: PartialEq> Environment<K, V> {
        pub fn depth(&self) -> usize {
            1 + self.previous.borrow().as_ref().map_or(0, |p| p.depth())
        }
    }

    #[test]
    fn test_env_base() {
        let mut env_base = Environment::new();
        env_base.insert(1, 'a');
        assert_eq!(env_base.get(&1), Some(&'a'));
        assert_eq!(env_base.get(&5), None);
        assert_eq!(env_base.depth(), 1);
    }

    #[test]
    fn test_clone() {
        let mut env_base = Environment::new();
        env_base.insert(1, 'a');

        let mut env2 = env_base.clone();
        env2.insert(2, 'b');
        assert_eq!(env2.get(&1), Some(&'a'));
        assert_eq!(env2.get(&2), Some(&'b'));
        env_base.insert(3, 'c');
        assert_eq!(env2.get(&3), None);
        assert_eq!(env_base.get(&3), Some(&'c'));
        env_base.insert(2, 'z');
        assert_eq!(env_base.get(&2), Some(&'z'));

        assert_eq!(env_base.depth(), 2);
        assert_eq!(env2.depth(), 2);
    }

    #[test]
    fn test_deepness() {
        let mut env_base = Environment::<u8, char>::new();
        assert_eq!(env_base.depth(), 1);

        let mut env2 = env_base.clone();
        assert_eq!(env_base.depth(), 1);
        assert_eq!(env_base.depth(), 1);

        env2.insert(1, 'a');
        let env3 = env2.clone();
        assert_eq!(env_base.depth(), 1);
        assert_eq!(env2.depth(), 2);
        assert_eq!(env3.depth(), 2);

        let env4 = env_base.clone();
        assert_eq!(env_base.depth(), 1);
        assert_eq!(env4.depth(), 1);

        env_base.insert(1, 'z');
        assert_eq!(env_base.depth(), 1);
        assert_eq!(env2.depth(), 2);
        assert_eq!(env3.depth(), 2);
        assert_eq!(env4.depth(), 1);

        let env5 = env_base.clone();
        assert_eq!(env_base.depth(), 2);
        assert_eq!(env2.depth(), 2);
        assert_eq!(env3.depth(), 2);
        assert_eq!(env4.depth(), 1);
        assert_eq!(env5.depth(), 2);
    }

    #[test]
    fn test_iter_layer() {
        let mut env_base = Environment::<u8, char>::new();
        assert_eq!(env_base.iter_layers().count(), 1);
        env_base.insert(1, 'a');
        assert_eq!(env_base.iter_layers().count(), 1);
        assert_eq!(env_base.iter_layers().next().unwrap().get(&1), Some(&'a'));
        assert_eq!(env_base.iter_layers().nth(1), None);
        let _ = env_base.clone();
        assert_eq!(env_base.iter_layers().count(), 1);
        env_base.insert(2, 'b');
        assert_eq!(env_base.iter_layers().count(), 2);
        let _ = env_base.clone();
        let mut iter = env_base.iter_layers();
        let map1 = iter.next().unwrap();
        assert_eq!(map1.get(&2), Some(&'b'));
        assert_eq!(map1.get(&1), None);
        let map2 = iter.next().unwrap();
        assert_eq!(map2.get(&1), Some(&'a'));
        assert_eq!(map2.get(&2), None);
        assert!(iter.next().is_none());
    }

    #[test]
    fn test_iter_elem() {
        let env_base = Environment::<u8, char>::new();
        assert!(env_base.iter_elems().next().is_none());

        let mut env_base = Environment::new();
        env_base.insert(1, 'a');
        env_base.insert(2, 'b');
        let mut iter_elems = env_base.iter_elems();
        assert!(iter_elems.next().is_some());
        assert!(iter_elems.next().is_some());
        assert!(iter_elems.next().is_none());
        assert!({
            let mut vec: Vec<_> = env_base.iter_elems().collect();
            vec.sort_unstable();
            vec == vec![(&1, &'a'), (&2, &'b')]
        });

        let mut env2 = env_base.clone();
        env_base.insert(1, 'z');
        env2.insert(1, 'y');
        assert_eq!(env_base.iter_elems().count(), 3);
        assert_eq!(env2.iter_elems().count(), 3);
        assert_eq!(env_base.iter_elems().nth(2), Some((&1, &'z')));
        assert_eq!(env2.iter_elems().nth(2), Some((&1, &'y')));

        let mut iter_elems_base = env_base.iter_elems();
        let _ = iter_elems_base.next();
        let mut iter_elems2 = env2.iter_elems();
        let _ = iter_elems_base.next();
        let _ = iter_elems2.next();

        let mut env_base = Environment::new();
        env_base.insert(1, 'a');
        env_base.insert(2, 'b');
        let _ = env_base.clone();
        env_base.insert(1, 'z');
        env_base.insert(3, 'c');
        let hmap: HashMap<_, _> = env_base.iter_elems().collect();
        assert_eq!(hmap.len(), 3);
        assert_eq!(hmap[&1], &'z');
        assert_eq!(hmap[&2], &'b');
        assert_eq!(hmap[&3], &'c');
    }
}
