use criterion::{criterion_main, Criterion};
use nickel_lang_utils::{bench::EvalMode, ncl_bench_group};
use pprof::criterion::{Output, PProfProfiler};

ncl_bench_group! {
    name = benches;
    config = Criterion::default().with_profiler(PProfProfiler::new(100, Output::Flamegraph(None)));
    {
            name = "countLetters",
            path = "records/countLetters",
            args = (include_str!("lorem.txt")),
            eval_mode = EvalMode::DeepSeq,
        }, {
            name = "merge",
            path = "records/merge",
            args = (500, 50),
            eval_mode = EvalMode::DeepSeq,
    }
}
criterion_main!(benches);
