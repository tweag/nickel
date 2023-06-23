use criterion::{criterion_main, Criterion};
use nickel_lang_utils::ncl_bench_group;
use pprof::criterion::{Output, PProfProfiler};

ncl_bench_group! {
    name = benches;
    config = Criterion::default().with_profiler(PProfProfiler::new(100, Output::Flamegraph(None)));
    {
        name = "fibonacci 10",
        path = "numeric/fibonacci",
        args = (10),
    }, {
        name = "pidigits 100",
        path = "numeric/pidigits",
        args = (100),
    }, {
        name = "sum 30",
        path = "numeric/reduce",
        subtest = "sum",
        args = (30),
    }, {
        name = "product 30",
        path = "numeric/reduce",
        subtest = "product",
        args = (30),
    }, {
        name = "scalar 10",
        path = "numeric/scalar",
        args = (10),
    }
}
criterion_main!(benches);
