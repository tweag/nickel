use criterion::{criterion_group, criterion_main, Criterion};

use nickel_lang_core::cache::{Cache, ErrorTolerance};
use nickel_lang_utils::bench::criterion_config;

pub fn typecheck_stdlib(c: &mut Criterion) {
    let mut cache = Cache::new(ErrorTolerance::Strict);
    cache.load_stdlib().unwrap();
    let type_env = cache.mk_type_ctxt().unwrap();
    c.bench_function("typecheck stdlib", |b| {
        b.iter_batched(
            || cache.clone(),
            |mut c_local| c_local.typecheck_stdlib_(&type_env).unwrap(),
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
