use criterion::{criterion_group, criterion_main, Criterion};
use nickel_lang_utilities::{bench_terms, Bench, EvalMode};
use pprof::criterion::{Output, PProfProfiler};

pub fn benches() {
    let config =
        Criterion::default().with_profiler(PProfProfiler::new(100, Output::Flamegraph(None)));
    let mut criterion: Criterion<_> = config.configure_from_args();
    let target = bench_terms(vec![
        Bench::bench_args(
            "countLetters",
            env!("CARGO_MANIFEST_DIR"),
            "records/countLetters",
            None,
            vec![String::from(include_str!("lorem.txt"))],
            EvalMode::DeepSeq,
        ),
        Bench::bench_args(
            "merge",
            env!("CARGO_MANIFEST_DIR"),
            "records/merge",
            None,
            vec![String::from("500"), String::from("50")],
            EvalMode::DeepSeq,
        ),
    ]);
    target(&mut criterion);
}
criterion_main!(benches);
