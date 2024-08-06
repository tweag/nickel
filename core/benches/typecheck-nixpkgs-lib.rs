use criterion::criterion_main;
use nickel_lang_utils::{bench::criterion_config, bench::EvalMode, ncl_bench_group};

ncl_bench_group! {
    name = benches;
    config = criterion_config();
    {
        name = "nixpkgs lists",
        path = "nixpkgs/lists",
        eval_mode = EvalMode::TypeCheck,
    }
}
criterion_main!(benches);
