use criterion::criterion_main;
use nickel_lang_utils::{bench::criterion_config, bench::EvalMode, ncl_bench_group};

ncl_bench_group! {
    name = benches;
    config = criterion_config();
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
