use criterion::{criterion_group, criterion_main, Criterion};
use nickel_lang_utilities::{bench_args, EvalMode};
use pprof::criterion::{Output, PProfProfiler};

fn count_letters(c: &mut Criterion) {
    bench_args(
        "countLetters",
        env!("CARGO_MANIFEST_DIR"),
        "records/countLetters",
        None,
        vec![String::from(include_str!("lorem.txt"))],
        EvalMode::DeepSeq,
        c,
    );
}

fn merge(c: &mut Criterion) {
    bench_args(
        "merge",
        env!("CARGO_MANIFEST_DIR"),
        "records/merge",
        None,
        vec![String::from("500"), String::from("50")],
        EvalMode::DeepSeq,
        c,
    );
}

criterion_group! {
    name = benches;
    config = Criterion::default().with_profiler(PProfProfiler::new(100, Output::Flamegraph(None)));
    targets = count_letters, merge
}
criterion_main!(benches);
