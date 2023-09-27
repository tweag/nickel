//! This module only contains macros wrapping those of the `metrics` crate, if
//! the `metrics` feature is enabled.

#[cfg(feature = "metrics")]
macro_rules! increment {
    ( $counter:expr ) => {
        $crate::metrics::increment!($counter, 1)
    };
    ( $counter:expr, $count:expr ) => {
        ::metrics::counter!($counter, $count)
    };
}

#[cfg(not(feature = "metrics"))]
macro_rules! increment {
    ( $( $args:expr ),+ ) => {};
}

#[cfg(feature = "metrics")]
macro_rules! sample {
    ( $counter:expr, $value:expr ) => {
        ::metrics::histogram!($counter, $value)
    };
}

#[cfg(not(feature = "metrics"))]
macro_rules! sample {
    ( $( $args:expr ),+ ) => {};
}

pub(crate) use increment;
pub(crate) use sample;
