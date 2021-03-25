use criterion::Criterion;
use std::io::Cursor;
use nickel::program::Program;
use std::path::PathBuf;

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum EvalMode {
    Normal,
    DeepSeq,
}

pub fn call_bench(name: &str, subpath: &str, subtest: Option<&str>, iteration: u32, eval_mode: EvalMode, c: &mut Criterion) {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push(format!("benches/{}.ncl", subpath));

    let field_path = subtest.map(|s| format!(".{}", s)).unwrap_or_default();
    let content = format!("(import \"{}\"){}.run {}", path.to_string_lossy(), field_path, iteration);

    let content = if eval_mode == EvalMode::DeepSeq {
        format!("%deepSeq% ({}) true", content)
    }
    else
    {
        content
    };

    let mut p = Program::new_from_source(Cursor::new(content), name).unwrap();
    c.bench_function(name, |b| b.iter(|| p.eval().unwrap()));
}
