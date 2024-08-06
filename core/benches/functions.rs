use criterion::criterion_main;
use nickel_lang_utils::{bench::criterion_config, ncl_bench_group};

ncl_bench_group! {
    name = benches;
    config = criterion_config();
    {
        name = "church 3",
        path = "functions/church",
        args = (3),
    }
}
criterion_main!(benches);
