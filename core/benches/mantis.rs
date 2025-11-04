use criterion::criterion_main;
use nickel_lang_utils::{bench::EvalMode, bench::criterion_config, ncl_bench_group};

ncl_bench_group! {
    name = benches;
    config = criterion_config();
    {
        name = "mantis",
        path = "mantis/run",
        args = (r#"{namespace = "mantis-staging", job="miner", role='miner}"#),
        eval_mode = EvalMode::DeepSeq,
    }, {
        name = "mantis serialize",
        path = "mantis/run",
        subtest = "serialize",
        args = (r#"{namespace = "mantis-staging", job="miner", role='miner}"#),
    }
}
criterion_main!(benches);
