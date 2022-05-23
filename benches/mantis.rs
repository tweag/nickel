use criterion::{criterion_main, Criterion};
use nickel_lang_utilities::{ncl_bench_group, EvalMode};
use pprof::criterion::{Output, PProfProfiler};

ncl_bench_group! {
    name = benches;
    config = Criterion::default().with_profiler(PProfProfiler::new(100, Output::Flamegraph(None)));
    {
        name = "mantis",
        path = "mantis/run",
        args = (r#"{namespace = "mantis-staging", job="miner", role=`miner}"#),
        eval_mode = EvalMode::DeepSeq,
    }, {
        name = "mantis serialize",
        path = "mantis/run",
        subtest = "serialize",
        args = (r#"{namespace = "mantis-staging", job="miner", role=`miner}"#),
    }
}
criterion_main!(benches);
