//! Helpers for tests and benches.
use codespan::Files;
use criterion::Criterion;
use nickel_lang::{
    error::{Error, ParseError},
    parser::{grammar, lexer},
    program::Program,
    term::{RichTerm, Term},
};

use std::{io::Cursor, path::PathBuf};

pub fn eval(s: impl std::string::ToString) -> Result<Term, Error> {
    let mut p = Program::new_from_source(Cursor::new(s.to_string()), "test").unwrap();
    p.eval().map(Term::from)
}

pub fn eval_file(f: &str) -> Result<Term, Error> {
    let path = format!("{}/../tests/{}", env!("CARGO_MANIFEST_DIR"), f);
    let mut p = Program::new_from_file(&path)
        .unwrap_or_else(|e| panic!("Could not create program from `{}`\n {}", path, e));
    p.eval().map(Term::from)
}

pub fn parse(s: &str) -> Result<RichTerm, ParseError> {
    let id = Files::new().add("<test>", String::from(s));

    grammar::TermParser::new()
        .parse_term(id, lexer::Lexer::new(&s))
        .map_err(|errs| errs.errors.first().unwrap().clone())
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
    bench_args_expect(
        name,
        base_dir,
        subpath,
        subtest,
        args,
        eval_mode,
        |_| true,
        c,
    )
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
        format!("%deep_seq% ({}) true", content)
    } else {
        content
    };

    c.bench_function(name, |b| {
        b.iter_batched_ref(
            || Program::new_from_source(Cursor::new(content.clone()), name).unwrap(),
            |p| assert!(pred(p.eval().map(Term::from).unwrap())),
            criterion::BatchSize::LargeInput,
        )
    });
}
