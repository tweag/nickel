use criterion::{criterion_main, Criterion};
use nickel_lang_utilities::{ncl_bench_group, EvalMode};
use pprof::criterion::{Output, PProfProfiler};

ncl_bench_group! {
    name = benches;
    config = Criterion::default().with_profiler(PProfProfiler::new(100, Output::Flamegraph(None)));
    {
        name = "nixpkgs lists",
        path = "nixpkgs/lists",
        eval_mode = EvalMode::TypeCheck,
    }
}
criterion_main!(benches);
