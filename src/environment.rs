use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::Hash;
use std::marker::PhantomData;
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
        self.current
            .get(key)
            .cloned()
            .or_else(|| self.previous.borrow().as_ref().and_then(|p| p.get(key)))
    }

    pub fn iter_layers(&self) -> EnvLayerIter<'_, K, V> {
        EnvLayerIter {
            env: if !self.was_cloned() {
                self
            } else {
                // if was cloned, current is the same as first of previous (that cannot be empty then)
                Rc::as_ptr(self.previous.borrow().as_ref().unwrap())
            },
            _marker: PhantomData,
        }
    }

    fn was_cloned(&self) -> bool {
        Rc::strong_count(&self.current) > 1
    }
}

pub struct EnvLayerIter<'a, K: 'a + Hash + Eq, V: 'a + PartialEq> {
    env: *const Environment<K, V>,
    _marker: PhantomData<&'a Environment<K, V>>,
}

impl<'a, K: 'a + Hash + Eq, V: 'a + PartialEq> Iterator for EnvLayerIter<'a, K, V> {
    type Item = Rc<HashMap<K, V>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.env.is_null() {
            return None;
        }
        // SAFETY: we checked that that self is not null on previous lines
        unsafe {
            let res = (*self.env).current.clone();
            self.env = (*self.env)
                .previous
                .borrow()
                .as_ref()
                .map_or(std::ptr::null(), Rc::as_ptr);
            Some(res)
        }
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
}
