use criterion::{criterion_group, criterion_main, Criterion};
use pprof::criterion::{Output, PProfProfiler};

use nickel_lang::cache::Cache;

pub fn typecheck_stdlib(c: &mut Criterion) {
    let mut cache = Cache::new();
    cache.load_stdlib().unwrap();
    let type_env = cache.mk_types_env().unwrap();
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
config = Criterion::default().with_profiler(PProfProfiler::new(100, Output::Flamegraph(None)));
targets = typecheck_stdlib
);
criterion_main!(benches);
