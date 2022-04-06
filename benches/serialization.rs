use criterion::{criterion_main, Criterion};
use nickel_lang_utilities::ncl_bench_group;
use pprof::criterion::{Output, PProfProfiler};

use std::path::PathBuf;

fn serialization_path(subpath: &str) -> String {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push(format!("benches/serialization/{}", subpath));
    path.to_string_lossy().to_string()
}

ncl_bench_group! {
    name = benches;
    config = Criterion::default().with_profiler(PProfProfiler::new(100, Output::Flamegraph(None)));
    {
        name = "round_trip",
        path = "serialization/main",
        // disabled because nickel does not manage dynamic imports.
        // args = (serialization_path("input.json")),
        // use subtest instead to have a function per `.json`
        subtest = "input",
    }
}
criterion_main!(benches);
