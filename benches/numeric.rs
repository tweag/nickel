use criterion::{criterion_group, criterion_main, Criterion};

mod common;
use common::{bench, EvalMode};

fn fibonacci(c: &mut Criterion) {
    bench("fibonacci 10", "numeric/fibonacci", None, 10, EvalMode::Normal, c);
}

// Waiting for nums stdlib
// fn pidigits(c: &mut Criterion) {
//     bench("pidigits 5", "numeric/pidigits", None, 10, EvalMode::Normal, c);
// }

// Waiting for lists stdlib
//fn sum(c: &mut Criterion) {
//    bench("sum 100", "numeric/reduce", Some("sum"), 100, EvalMode::Normal, c);
//}
//
//fn product(c: &mut Criterion) {
//    bench("product 100", "numeric/reduce", Some("product"), 100, EvalMode::Normal, c);
//}

// Waiting for lists stdlib
//fn scalar(c: &mut Criterion) {
//    bench("scalar 100", "numeric/scalar", None, 100, EvalMode::Normal, c);
//}

//criterion_group!(benches, fibonacci, pidigits, sum, product, scalar);
criterion_group!(benches, fibonacci);
criterion_main!(benches);
