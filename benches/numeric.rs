use criterion::{criterion_group, criterion_main, Criterion};

use utilities::{bench, EvalMode};

fn fibonacci(c: &mut Criterion) {
    bench("fibonacci 10", env!("CARGO_MANIFEST_DIR"), "numeric/fibonacci", None, 10, EvalMode::Normal, c);
}

fn pidigits(c: &mut Criterion) {
    bench("pidigits 5", env!("CARGO_MANIFEST_DIR"), "numeric/pidigits", None, 10, EvalMode::Normal, c);
}

fn sum(c: &mut Criterion) {
   bench("sum 100", env!("CARGO_MANIFEST_DIR"), "numeric/reduce", Some("sum"), 100, EvalMode::Normal, c);
}

fn product(c: &mut Criterion) {
   bench("product 100", env!("CARGO_MANIFEST_DIR"), "numeric/reduce", Some("product"), 100, EvalMode::Normal, c);
}

fn scalar(c: &mut Criterion) {
   bench("scalar 100", env!("CARGO_MANIFEST_DIR"), "numeric/scalar", None, 100, EvalMode::Normal, c);
}

criterion_group!(benches, fibonacci, pidigits, sum, product, scalar);
criterion_main!(benches);
