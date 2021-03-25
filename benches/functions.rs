use criterion::{criterion_group, criterion_main, Criterion};

mod common;
use common::{call_bench, EvalMode};

fn church(c: &mut Criterion) {
    call_bench("church 5", "functions/church", None, 5, EvalMode::Normal, c);
}

criterion_group!(benches, church);
criterion_main!(benches);
