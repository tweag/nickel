use criterion::criterion_main;
use nickel_lang_utils::{bench::criterion_config, ncl_bench_group};

ncl_bench_group! {
    name = benches;
    config = criterion_config();
    {
        name = "round_trip",
        path = "serialization/main",
        subtest = "input",
    }
}
criterion_main!(benches);
