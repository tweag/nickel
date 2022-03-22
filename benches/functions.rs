use criterion::{criterion_group, criterion_main, Criterion};
use nickel_lang::term::Term;
use nickel_lang_utilities::{bench_terms, Bench, EvalMode};
use pprof::criterion::{Output, PProfProfiler};

pub fn benches() {
    let config =
        Criterion::default().with_profiler(PProfProfiler::new(100, Output::Flamegraph(None)));
    let mut criterion: Criterion<_> = config.configure_from_args();
    let expect = |term| matches!(term, Term::Bool(true));
    let target = bench_terms(vec![Bench::bench_expect(
        "church 3",
        env!("CARGO_MANIFEST_DIR"),
        "functions/church",
        None,
        3,
        EvalMode::Normal,
        expect,
    )]);
    target(&mut criterion);
}
criterion_main!(benches);
