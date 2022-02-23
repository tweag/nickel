use criterion::{criterion_group, criterion_main, Criterion};
use nickel::program::Program;
use pprof::criterion::{Output, PProfProfiler};
use std::io::Cursor;
use std::path::PathBuf;

fn round_trip(c: &mut Criterion) {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("benches/serialization/input.json");

    let content = format!(
        "builtin.serialize `Json (import \"{}\")",
        path.to_string_lossy(),
    );

    let mut p = Program::new_from_source(Cursor::new(content), "serialize_round_trip").unwrap();
    c.bench_function("round_trip", |b| b.iter(|| p.eval().unwrap()));
}

criterion_group! {
    name = benches;
    config = Criterion::default().with_profiler(PProfProfiler::new(100, Output::Flamegraph(None)));
    targets = round_trip
}
criterion_main!(benches);
