use criterion::{criterion_group, criterion_main, Criterion};
use nickel_lang_utilities::{bench_terms, Bench, EvalMode};
use pprof::criterion::{Output, PProfProfiler};

pub fn benches() {
    let config =
        Criterion::default().with_profiler(PProfProfiler::new(100, Output::Flamegraph(None)));
    let mut criterion: Criterion<_> = config.configure_from_args();
    let target = bench_terms(vec![
        Bench::bench(
            "fibonacci 10",
            env!("CARGO_MANIFEST_DIR"),
            "numeric/fibonacci",
            None,
            10,
            EvalMode::Normal,
        ),
        Bench::bench(
            "pidigits 100",
            env!("CARGO_MANIFEST_DIR"),
            "numeric/pidigits",
            None,
            100,
            EvalMode::Normal,
        ),
        Bench::bench(
            "sum 30",
            env!("CARGO_MANIFEST_DIR"),
            "numeric/reduce",
            Some("sum"),
            30,
            EvalMode::Normal,
        ),
        Bench::bench(
            "product 30",
            env!("CARGO_MANIFEST_DIR"),
            "numeric/reduce",
            Some("product"),
            30,
            EvalMode::Normal,
        ),
        Bench::bench(
            "scalar 10",
            env!("CARGO_MANIFEST_DIR"),
            "numeric/scalar",
            None,
            10,
            EvalMode::Normal,
        ),
    ]);
    target(&mut criterion);
}
criterion_main!(benches);
