use std::{
    collections::HashMap,
    hash::BuildHasherDefault,
    sync::{atomic::Ordering, Arc, PoisonError, RwLock},
};

use metrics::{
    atomics::AtomicU64, Counter, Gauge, Histogram, Key, KeyHasher, KeyName, SharedString, Unit,
};

/// The actual store for recorded metrics and their descriptions. We only need
/// to take the write lock once when creating a new metric, in all other cases
/// we only take a read lock. The `metrics` crate implements the necessary
/// trait [`::metrics::CounterFn`] for `Arc<AtomicU64>` and in any case expects its
/// counter types to be `Arc`s. For this reason, we use `Arc<AtomicU64>` as the
/// value type of the counters `HashMap`.
///
/// The `metrics` crate expects to be used in a thread safe manner, so follow
/// suit here.
#[derive(Default)]
pub(super) struct Registry {
    counters: RwLock<HashMap<Key, Arc<AtomicU64>, BuildHasherDefault<KeyHasher>>>,
    descriptions: RwLock<HashMap<KeyName, SharedString>>,
}

/// A metrics recorder utilizing [`Registry`] for storing metrics and their
/// descriptions. It currently only supports [`Counter`]s and will panic if
/// gauges or histograms are submitted.
pub(super) struct Recorder {
    inner: Arc<Registry>,
}

impl Recorder {
    /// Construct a `Recorder` and register it as the global metrics recorder.
    /// The return value is a reference to the metrics registry. This way we can
    /// retrieve the stored values later.
    ///
    /// This function should only be called once at the start of the CLI.
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
    /// Print out a report of all metrics that have been collected so far, with
    /// their description if available.
    ///
    /// We expect this function to be called once at the end of the CLI, but it
    /// should be safe to call it in the middle of a Nickel execution as well.
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
                eprintln!(
                    "{}: {}\n  {}",
                    key.name(),
                    description,
                    counter.load(Ordering::Relaxed)
                );
            } else {
                eprintln!("{}: {}", key.name(), counter.load(Ordering::Relaxed));
            }
        }
    }
}
