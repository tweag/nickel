use criterion::{criterion_main, Criterion};
use nickel_lang_utils::ncl_bench_group;
use pprof::criterion::{Output, PProfProfiler};

ncl_bench_group! {
    name = benches;
    config = Criterion::default().with_profiler(PProfProfiler::new(100, Output::Flamegraph(None)));
    {
        name = "church 3",
        path = "functions/church",
        args = (3),
    }
}
criterion_main!(benches);
