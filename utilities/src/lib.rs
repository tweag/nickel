//! Helpers for tests and benches.
use criterion::Criterion;
use nickel::error::Error;
use nickel::program::Program;
use nickel::term::Term;
use std::io::Cursor;
use std::path::PathBuf;

pub fn eval(s: impl std::string::ToString) -> Result<Term, Error> {
    let mut p = Program::new_from_source(Cursor::new(s.to_string()), "test").unwrap();
    p.eval()
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum EvalMode {
    Normal,
    DeepSeq,
}

pub fn bench(
    name: &str,
    base_dir: &str,
    subpath: &str,
    subtest: Option<&str>,
    iteration: u32,
    eval_mode: EvalMode,
    c: &mut Criterion,
) {
    bench_args(
        name,
        base_dir,
        subpath,
        subtest,
        vec![iteration.to_string()],
        eval_mode,
        c,
    )
}

pub fn bench_expect<F>(
    name: &str,
    base_dir: &str,
    subpath: &str,
    subtest: Option<&str>,
    iteration: u32,
    eval_mode: EvalMode,
    pred: F,
    c: &mut Criterion,
) where
    F: Fn(Term) -> bool,
{
    bench_args_expect(
        name,
        base_dir,
        subpath,
        subtest,
        vec![iteration.to_string()],
        eval_mode,
        pred,
        c,
    )
}

pub fn bench_args(
    name: &str,
    base_dir: &str,
    subpath: &str,
    subtest: Option<&str>,
    args: Vec<String>,
    eval_mode: EvalMode,
    c: &mut Criterion,
) {
    bench_args_expect(name, base_dir, subpath, subtest, args, eval_mode, |_| true, c)
}

pub fn bench_args_expect<F>(
    name: &str,
    base_dir: &str,
    subpath: &str,
    subtest: Option<&str>,
    args: Vec<String>,
    eval_mode: EvalMode,
    pred: F,
    c: &mut Criterion,
) where
    F: Fn(Term) -> bool,
{
    let mut path = PathBuf::from(base_dir);
    path.push(format!("benches/{}.ncl", subpath));

    let field_path = subtest.map(|s| format!(".{}", s)).unwrap_or_default();
    let content = format!(
        "(import \"{}\"){}.run {}",
        path.to_string_lossy(),
        field_path,
        args.into_iter()
            .map(|s| format!("({})", s))
            .collect::<Vec<String>>()
            .join(" ")
    );

    let content = if eval_mode == EvalMode::DeepSeq {
        format!("%deepSeq% ({}) true", content)
    } else {
        content
    };

    let mut p = Program::new_from_source(Cursor::new(content), name).unwrap();
    c.bench_function(name, |b| b.iter(|| assert!(pred(p.eval().unwrap()))));
}
