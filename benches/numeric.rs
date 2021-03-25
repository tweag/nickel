use criterion::{criterion_group, criterion_main, Criterion};

mod common;
use common::{call_bench, EvalMode};

fn fibonacci(c: &mut Criterion) {
    call_bench("fibonacci 5", "numeric/fibonacci", None, 10, EvalMode::Normal, c);
}

// Waiting for nums stdlib
// fn pidigits(c: &mut Criterion) {
//     call_bench("pidigits 5", "numeric/pidigits", None, 10, EvalMode::Normal, c);
// }

// Waiting for lists stdlib
//fn sum(c: &mut Criterion) {
//    call_bench("sum 100", "numeric/reduce", Some("sum"), 100, EvalMode::Normal, c);
//}
//
//fn product(c: &mut Criterion) {
//    call_bench("product 100", "numeric/reduce", Some("product"), 100, EvalMode::Normal, c);
//}

// Waiting for lists stdlib
//fn scalar(c: &mut Criterion) {
//    call_bench("scalar 100", "numeric/scalar", None, 100, EvalMode::Normal, c);
//}

//criterion_group!(benches, fibonacci, pidigits, sum, product, scalar);
criterion_group!(benches, fibonacci);
criterion_main!(benches);
