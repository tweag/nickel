use criterion::{criterion_group, criterion_main, Criterion};
use nickel_lang_core::{cache_new::SourceCache, driver, typecheck};
use pprof::criterion::{Output, PProfProfiler};

pub fn typecheck_stdlib(c: &mut Criterion) {
    let mut cache = SourceCache::new();
    driver::load_stdlib(&mut cache);

    let type_env = typecheck::Context::from_stdlib(&cache);
    c.bench_function("typecheck stdlib", |b| {
        b.iter_batched(
            || cache.clone(),
            |mut c_local| driver::typecheck_stdlib_(&mut c_local, &type_env).unwrap(),
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
