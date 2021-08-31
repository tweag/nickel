use std::cell::RefCell;
use std::collections::{hash_map, HashMap};
use std::hash::Hash;
use std::iter::FromIterator;
use std::marker::PhantomData;
use std::ptr::NonNull;
use std::rc::Rc;

#[derive(Debug, PartialEq, Default)]
pub struct Environment<K: Hash + Eq, V: PartialEq> {
    current: Rc<HashMap<K, V>>,
    previous: RefCell<Option<Rc<Environment<K, V>>>>,
}

impl<K: Hash + Eq, V: PartialEq> Clone for Environment<K, V> {
    fn clone(&self) -> Self {
        if !self.current.is_empty() && !self.was_cloned() {
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

impl<K: Hash + Eq, V: PartialEq> Environment<K, V> {
    pub fn new() -> Self {
        Self {
            current: Rc::new(HashMap::new()),
            previous: RefCell::new(None),
        }
    }

    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        if self.was_cloned() {
            self.current = Rc::new(HashMap::new());
        }
        Rc::get_mut(&mut self.current).unwrap().insert(key, value)
    }

    pub fn get(&self, key: &K) -> Option<V>
    where
        V: Clone,
    {
        self.iter_layers().find_map(|hmap| hmap.get(key).cloned())
    }

    pub fn iter_layers(&self) -> EnvLayerIter<'_, K, V> {
        EnvLayerIter {
            env: if !self.was_cloned() {
                Some(NonNull::from(self))
            } else {
                // if was cloned, current is the same as first of previous (that cannot be empty then)
                self.previous
                    .borrow()
                    .as_ref()
                    // SAFETY: created from Rc, so cannot be null
                    .map(|prev| unsafe { NonNull::new_unchecked(Rc::as_ptr(prev) as *mut _) })
            },
            _marker: PhantomData,
        }
    }

    pub fn iter_elems(&self) -> EnvElemIter<'_, K, V> {
        let mut env: Vec<NonNull<HashMap<K, V>>> = self
            .iter_layers()
            // SAFETY: all NonNull::new_unchecked comes from pointers created from Rc, so cannot be null
            .map(|hmap| unsafe { NonNull::new_unchecked(Rc::as_ptr(&hmap) as *mut _) })
            .collect();
        // SAFETY: by design, env cannot be empty, and coming from an Rc, it is well aligned and initialized
        let current_map = unsafe { env.pop().unwrap().as_ref() }.iter();
        EnvElemIter { env, current_map }
    }

    pub fn iter(&self) -> EnvIter<'_, K, V> {
        EnvIter {
            collapsed_map: self.iter_elems().collect::<HashMap<_, _>>().into_iter(),
        }
    }

    fn was_cloned(&self) -> bool {
        Rc::strong_count(&self.current) > 1
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

/// ```compile_fail
/// let env = Environment::<char, char>::new();
/// let mut iter = env.iter_layers();
/// drop(env);
/// let _ = iter.next();
/// ```
///
/// ```compile_fail
/// let mut env = Environment::<char, char>::new();
/// let mut iter = env.iter_layers();
/// env.insert('a', 'a');
/// let _ = iter.next();
/// ```
pub struct EnvLayerIter<'a, K: 'a + Hash + Eq, V: 'a + PartialEq> {
    env: Option<NonNull<Environment<K, V>>>,
    _marker: PhantomData<&'a Environment<K, V>>,
}

impl<'a, K: 'a + Hash + Eq, V: 'a + PartialEq> Iterator for EnvLayerIter<'a, K, V> {
    type Item = Rc<HashMap<K, V>>;

    fn next(&mut self) -> Option<Self::Item> {
        // SAFETY: NonNull being in an option, we know it cannot be null and can be dereferencable
        self.env.map(|env| unsafe {
            let res = env.as_ref().current.clone();
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

/// ```compile_fail
/// let env = Environment::<char, char>::new();
/// let mut iter = env.iter_elems();
/// drop(env);
/// let _ = iter.next();
/// ```
///
/// ```compile_fail
/// let mut env = Environment::<char, char>::new();
/// let mut iter = env.iter_elems();
/// env.insert('a', 'a');
/// let _ = iter.next();
/// ```
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
        pub fn deepness(&self) -> usize {
            1 + self.previous.borrow().as_ref().map_or(0, |p| p.deepness())
        }
    }

    #[test]
    fn test_env_base() {
        let mut env_base = Environment::new();
        assert_eq!(env_base.insert(1, 'a'), None);
        assert_eq!(env_base.get(&1), Some('a'));
        assert_eq!(env_base.get(&5), None);
        assert_eq!(env_base.deepness(), 1);
    }

    #[test]
    fn test_clone() {
        let mut env_base = Environment::new();
        env_base.insert(1, 'a');

        let mut env2 = env_base.clone();
        assert_eq!(env2.insert(2, 'b'), None);
        assert_eq!(env2.get(&1), Some('a'));
        assert_eq!(env2.get(&2), Some('b'));
        env_base.insert(3, 'c');
        assert_eq!(env2.get(&3), None);
        assert_eq!(env_base.get(&3), Some('c'));
        assert_eq!(env_base.insert(2, 'z'), None);
        assert_eq!(env_base.get(&2), Some('z'));

        assert_eq!(env_base.deepness(), 2);
        assert_eq!(env2.deepness(), 2);
    }

    #[test]
    fn test_deepness() {
        let mut env_base = Environment::<u8, char>::new();
        assert_eq!(env_base.deepness(), 1);

        let mut env2 = env_base.clone();
        assert_eq!(env_base.deepness(), 1);
        assert_eq!(env_base.deepness(), 1);

        env2.insert(1, 'a');
        let env3 = env2.clone();
        assert_eq!(env_base.deepness(), 1);
        assert_eq!(env2.deepness(), 2);
        assert_eq!(env3.deepness(), 2);

        let env4 = env_base.clone();
        assert_eq!(env_base.deepness(), 1);
        assert_eq!(env4.deepness(), 1);

        env_base.insert(1, 'z');
        assert_eq!(env_base.deepness(), 1);
        assert_eq!(env2.deepness(), 2);
        assert_eq!(env3.deepness(), 2);
        assert_eq!(env4.deepness(), 1);

        let env5 = env_base.clone();
        assert_eq!(env_base.deepness(), 2);
        assert_eq!(env2.deepness(), 2);
        assert_eq!(env3.deepness(), 2);
        assert_eq!(env4.deepness(), 1);
        assert_eq!(env5.deepness(), 2);
    }

    #[test]
    fn test_iter_layer() {
        let mut env_base = Environment::<u8, char>::new();
        assert_eq!(env_base.iter_layers().count(), 1);
        env_base.insert(1, 'a');
        assert_eq!(env_base.iter_layers().count(), 1);
        assert_eq!(env_base.iter_layers().next().unwrap().get(&1), Some(&'a'));
        assert_eq!(env_base.iter_layers().skip(1).next(), None);
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
        assert_eq!(env_base.iter_elems().skip(2).next(), Some((&1, &'z')));
        assert_eq!(env2.iter_elems().skip(2).next(), Some((&1, &'y')));

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
