//! This module only contains macros wrapping those of the `metrics` crate, if
//! the `metrics` feature is enabled.

#[cfg(feature = "metrics")]
macro_rules! increment {
    ( $counter:expr ) => {
        $crate::metrics::increment!($counter, 1)
    };
    ( $counter:expr, $count:expr ) => {
        ::metrics::counter!($counter).increment($count)
    };
}

#[cfg(not(feature = "metrics"))]
macro_rules! increment {
    ( $( $args:expr ),+ ) => {};
}

#[cfg(feature = "metrics")]
macro_rules! sample {
    ( $counter:expr, $value:expr ) => {
        ::metrics::histogram!($counter).record($value)
    };
}

#[cfg(not(feature = "metrics"))]
macro_rules! sample {
    ( $( $args:expr ),+ ) => {};
}

#[cfg(feature = "metrics")]
macro_rules! measure_runtime {
    ( $counter:expr, $expr:expr ) => {{
        let start_time = ::std::time::Instant::now();
        let result = $expr;
        let duration = start_time.elapsed();
        ::metrics::counter!($counter).increment(duration.as_millis() as u64);
        result
    }};
}

#[cfg(not(feature = "metrics"))]
macro_rules! measure_runtime {
    ( $counter: expr, $expr: expr ) => {
        $expr
    };
}

pub(crate) use increment;
pub(crate) use measure_runtime;
pub(crate) use sample;
