use std::{
    collections::HashMap,
    hash::BuildHasherDefault,
    sync::{atomic::Ordering, Arc, PoisonError, RwLock},
};

use metrics::{
    atomics::AtomicU64, Counter, Gauge, Histogram, Key, KeyHasher, KeyName, SharedString, Unit,
};

#[derive(Default)]
pub(super) struct Registry {
    counters: RwLock<HashMap<Key, Arc<AtomicU64>, BuildHasherDefault<KeyHasher>>>,
    descriptions: RwLock<HashMap<KeyName, SharedString>>,
}

pub(super) struct Recorder {
    inner: Arc<Registry>,
}

impl Recorder {
    pub(super) fn install() -> Arc<Registry> {
        let registry = Arc::<Registry>::default();
        metrics::set_boxed_recorder(Box::new(Recorder {
            inner: registry.clone(),
        }))
        .expect("registering a metrics recorder shouldn't fail");
        registry
    }
}

impl metrics::Recorder for Recorder {
    fn describe_counter(&self, key: KeyName, _unit: Option<Unit>, description: SharedString) {
        self.inner
            .descriptions
            .write()
            .unwrap_or_else(PoisonError::into_inner)
            .entry(key)
            .or_insert(description);
    }

    fn describe_gauge(&self, _key: KeyName, _unit: Option<Unit>, _description: SharedString) {
        unimplemented!()
    }

    fn describe_histogram(&self, _key: KeyName, _unit: Option<Unit>, _description: SharedString) {
        unimplemented!()
    }

    fn register_counter(&self, key: &Key) -> Counter {
        let read = self
            .inner
            .counters
            .read()
            .unwrap_or_else(PoisonError::into_inner);
        if let Some(counter) = read.get(key) {
            Counter::from_arc(counter.clone())
        } else {
            drop(read);
            let mut write = self
                .inner
                .counters
                .write()
                .unwrap_or_else(PoisonError::into_inner);
            if let Some(counter) = write.get(key) {
                Counter::from_arc(counter.clone())
            } else {
                let counter = Arc::new(AtomicU64::new(0));
                write.insert(key.clone(), counter.clone());
                Counter::from_arc(counter)
            }
        }
    }

    fn register_gauge(&self, _key: &Key) -> Gauge {
        unimplemented!()
    }

    fn register_histogram(&self, _key: &Key) -> Histogram {
        unimplemented!()
    }
}

impl Registry {
    pub(super) fn report(&self) {
        for (key, counter) in self
            .counters
            .read()
            .unwrap_or_else(PoisonError::into_inner)
            .iter()
        {
            if let Some(description) = self
                .descriptions
                .read()
                .unwrap_or_else(PoisonError::into_inner)
                .get(key.name())
            {
                println!(
                    "{}: {}\n  {}",
                    key.name(),
                    description,
                    counter.load(Ordering::Relaxed)
                );
            } else {
                println!("{}: {}", key.name(), counter.load(Ordering::Relaxed));
            }
        }
    }
}
