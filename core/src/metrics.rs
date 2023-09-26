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

pub(crate) use increment;
