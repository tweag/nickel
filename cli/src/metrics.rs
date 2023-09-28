use std::{
    fmt::Display,
    sync::{atomic::Ordering, Arc},
};

use metrics::{Counter, Gauge, Key, KeyName, SharedString, Unit};
use metrics_util::registry::AtomicStorage;

/// A handle to the actual store for recorded metrics.
#[derive(Clone)]
pub(super) struct Recorder {
    registry: Arc<metrics_util::registry::Registry<Key, AtomicStorage>>,
}

impl Default for Recorder {
    fn default() -> Recorder {
        Recorder {
            registry: Arc::new(metrics_util::registry::Registry::atomic()),
        }
    }
}

impl Recorder {
    /// Construct a `Recorder` and register it as the global metrics recorder.
    /// The return value is a reference to the metrics registry. This way we can
    /// retrieve the stored values later.
    ///
    /// This function should only be called once at the start of the CLI.
    pub(super) fn install() -> Recorder {
        let recorder = Recorder::default();
        metrics::set_boxed_recorder(Box::new(recorder.clone()))
            .expect("registering a metrics recorder shouldn't fail");
        recorder
    }
}

impl metrics::Recorder for Recorder {
    fn describe_counter(&self, _key: KeyName, _unit: Option<Unit>, _description: SharedString) {
        unimplemented!()
    }

    fn describe_gauge(&self, _key: KeyName, _unit: Option<Unit>, _description: SharedString) {
        unimplemented!()
    }

    fn describe_histogram(&self, _key: KeyName, _unit: Option<Unit>, _description: SharedString) {
        unimplemented!()
    }

    fn register_counter(&self, key: &Key) -> Counter {
        self.registry
            .get_or_create_counter(key, |c| c.clone().into())
    }

    fn register_gauge(&self, key: &Key) -> Gauge {
        self.registry.get_or_create_gauge(key, |c| c.clone().into())
    }

    fn register_histogram(&self, key: &Key) -> metrics::Histogram {
        self.registry
            .get_or_create_histogram(key, |c| c.clone().into())
    }
}

struct BucketStatistics {
    min: f64,
    max: f64,
    avg: f64,
    count: usize,
}

impl BucketStatistics {
    fn new() -> BucketStatistics {
        BucketStatistics {
            min: f64::NAN,
            max: f64::NAN,
            avg: 0 as f64,
            count: 0,
        }
    }

    fn update(&mut self, data: &[f64]) {
        let mut sum = 0 as f64;

        for sample in data {
            self.min = self.min.min(*sample);
            self.max = self.max.max(*sample);
            sum += *sample;
        }

        self.avg = (self.count as f64) * self.avg + sum;
        self.count += data.len();
        self.avg /= self.count as f64;
    }
}

impl Default for BucketStatistics {
    fn default() -> Self {
        Self::new()
    }
}

impl Display for BucketStatistics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "min {}, max {}, avg {:.1}; samples {}",
            self.min, self.max, self.avg, self.count
        )
    }
}

impl Recorder {
    /// Print out a report of all metrics that have been collected so far, with
    /// their description if available.
    ///
    /// We expect this function to be called once at the end of the CLI, but it
    /// should be safe to call it in the middle of a Nickel execution as well.
    pub(super) fn report(&self) {
        self.registry.visit_counters(|key, counter| {
            eprintln!("{}: {}", key.name(), counter.load(Ordering::Relaxed))
        });
        self.registry.visit_histograms(|key, bucket| {
            let mut stats = BucketStatistics::new();
            bucket.data_with(|data| stats.update(data));
            eprintln!("{}: {}", key.name(), stats);
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn averge_tracking() {
        let mut overall_stats = BucketStatistics::new();
        let mut incremental_stats = BucketStatistics::new();

        let data = [3.0, 2.0, 6.0, 12.0, 56.0, 82.0, 202.0, 100.0, 29.0];
        overall_stats.update(&data);

        assert_eq!(overall_stats.count, data.len());
        assert_eq!(
            overall_stats.min,
            data.iter().copied().fold(f64::NAN, f64::min)
        );
        assert_eq!(
            overall_stats.max,
            data.iter().copied().fold(f64::NAN, f64::max)
        );
        assert_eq!(
            overall_stats.avg,
            data.iter().copied().sum::<f64>() / (data.len() as f64)
        );

        incremental_stats.update(&data[0..5]);
        incremental_stats.update(&data[5..]);
        assert_eq!(incremental_stats.min, overall_stats.min);
        assert_eq!(incremental_stats.max, overall_stats.max);
        assert_eq!(incremental_stats.avg, overall_stats.avg);
        assert_eq!(incremental_stats.count, overall_stats.count);
    }
}
