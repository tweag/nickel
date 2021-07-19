use criterion::{criterion_group, criterion_main, Criterion};
use pprof::criterion::{Output, PProfProfiler};
use utilities::{bench, EvalMode};

fn fibonacci(c: &mut Criterion) {
    bench(
        "fibonacci 10",
        env!("CARGO_MANIFEST_DIR"),
        "numeric/fibonacci",
        None,
        10,
        EvalMode::Normal,
        c,
    );
}

fn pidigits(c: &mut Criterion) {
    bench(
        "pidigits 100",
        env!("CARGO_MANIFEST_DIR"),
        "numeric/pidigits",
        None,
        100,
        EvalMode::Normal,
        c,
    );
}

fn sum(c: &mut Criterion) {
    bench(
        "sum 30",
        env!("CARGO_MANIFEST_DIR"),
        "numeric/reduce",
        Some("sum"),
        30,
        EvalMode::Normal,
        c,
    );
}

fn product(c: &mut Criterion) {
    bench(
        "product 30",
        env!("CARGO_MANIFEST_DIR"),
        "numeric/reduce",
        Some("product"),
        30,
        EvalMode::Normal,
        c,
    );
}

fn scalar(c: &mut Criterion) {
    bench(
        "scalar 10",
        env!("CARGO_MANIFEST_DIR"),
        "numeric/scalar",
        None,
        10,
        EvalMode::Normal,
        c,
    );
}

criterion_group! {
    name = benches;
    config = Criterion::default().with_profiler(PProfProfiler::new(100, Output::Flamegraph(None)));
    targets = fibonacci, pidigits, sum, product, scalar
}
criterion_main!(benches);
