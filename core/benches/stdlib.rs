use criterion::{criterion_group, criterion_main, Criterion};

use nickel_lang_core::cache::CacheHub;
use nickel_lang_utils::bench::criterion_config;

pub fn typecheck_stdlib(c: &mut Criterion) {
    c.bench_function("typecheck stdlib", |b| {
        b.iter_batched(
            || {
                let mut cache = CacheHub::new();
                cache.load_stdlib().unwrap();
                cache
            },
            |mut cache| cache.typecheck_stdlib().unwrap(),
            criterion::BatchSize::LargeInput,
        )
    });
}

criterion_group!(
name = benches;
config = criterion_config();
targets = typecheck_stdlib
);
criterion_main!(benches);
