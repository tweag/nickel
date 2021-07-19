use criterion::{criterion_group, criterion_main, Criterion};
use nickel::term::Term;
use pprof::criterion::{Output, PProfProfiler};
use utilities::{bench_expect, EvalMode};

fn church(c: &mut Criterion) {
    let expect = |term| matches!(term, Term::Bool(true));
    bench_expect(
        "church 3",
        env!("CARGO_MANIFEST_DIR"),
        "functions/church",
        None,
        3,
        EvalMode::Normal,
        expect,
        c,
    );
}

criterion_group! {
    name = benches;
    config = Criterion::default().with_profiler(PProfProfiler::new(100, Output::Flamegraph(None)));
    targets = church
}
criterion_main!(benches);
