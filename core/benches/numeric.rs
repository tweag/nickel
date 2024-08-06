use criterion::criterion_main;
use nickel_lang_utils::{bench::criterion_config, ncl_bench_group};

ncl_bench_group! {
    name = benches;
    config = criterion_config();
    {
        name = "fibonacci 10",
        path = "numeric/fibonacci",
        args = (10),
    }, {
        name = "pidigits 100",
        path = "numeric/pidigits",
        args = (100),
    }, {
        name = "sum 30",
        path = "numeric/reduce",
        subtest = "sum",
        args = (30),
    }, {
        name = "product 30",
        path = "numeric/reduce",
        subtest = "product",
        args = (30),
    }, {
        name = "scalar 10",
        path = "numeric/scalar",
        args = (10),
    }
}
criterion_main!(benches);
