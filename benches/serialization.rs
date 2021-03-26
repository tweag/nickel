use criterion::{criterion_group, criterion_main, Criterion};
use nickel::program::Program;
use std::io::Cursor;
use std::path::PathBuf;

fn round_trip(c: &mut Criterion) {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("benches/serialization/input.json");

    let content = format!(
        "builtins.serialize `Json (import \"{}\")",
        path.to_string_lossy(),
    );

    let mut p = Program::new_from_source(Cursor::new(content), "serialize_round_trip").unwrap();
    c.bench_function("round_trip", |b| b.iter(|| p.eval().unwrap()));
}

criterion_group!(benches, round_trip);
criterion_main!(benches);
