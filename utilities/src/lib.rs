//! Helpers for tests and benches.
use codespan::Files;
use criterion::Criterion;
use nickel_lang::{
    cache::{Cache, Envs, ErrorTolerance},
    error::{Error, ParseError},
    eval,
    parser::{grammar, lexer},
    program::Program,
    term::{RichTerm, Term},
    transform::import_resolution,
};

use std::{io::Cursor, path::PathBuf};

pub fn eval(s: impl std::string::ToString) -> Result<Term, Error> {
    let mut p = Program::new_from_source(Cursor::new(s.to_string()), "test").unwrap();
    p.eval().map(Term::from)
}

pub fn eval_file(f: &str) -> Result<Term, Error> {
    let mut p = program_from_test_fixture(f);
    p.eval().map(Term::from)
}

pub fn parse(s: &str) -> Result<RichTerm, ParseError> {
    let id = Files::new().add("<test>", String::from(s));

    grammar::TermParser::new()
        .parse_term(id, lexer::Lexer::new(&s))
        .map_err(|errs| errs.errors.first().unwrap().clone())
}

pub fn typecheck_fixture(f: &str) -> Result<(), Error> {
    let mut p = program_from_test_fixture(f);
    p.typecheck()
}

fn program_from_test_fixture(f: &str) -> Program {
    let path = format!("{}/../tests/{}", env!("CARGO_MANIFEST_DIR"), f);
    Program::new_from_file(&path)
        .unwrap_or_else(|e| panic!("Could not create program from `{}`\n {}", path, e))
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum EvalMode {
    Normal,
    DeepSeq,
    TypeCheck,
}

pub struct Bench<'b> {
    pub name: &'b str,
    pub base_dir: &'b str,
    pub subpath: &'b str,
    pub subtest: Option<&'b str>,
    pub args: Vec<String>,
    pub eval_mode: EvalMode,
}

impl<'b> Bench<'b> {
    pub fn bench(
        name: &'b str,
        base_dir: &'b str,
        subpath: &'b str,
        subtest: Option<&'b str>,
        iteration: u32,
        eval_mode: EvalMode,
    ) -> Self {
        Self::bench_args(
            name,
            base_dir,
            subpath,
            subtest,
            vec![iteration.to_string()],
            eval_mode,
        )
    }

    pub fn bench_args(
        name: &'b str,
        base_dir: &'b str,
        subpath: &'b str,
        subtest: Option<&'b str>,
        args: Vec<String>,
        eval_mode: EvalMode,
    ) -> Self {
        Self {
            name,
            base_dir,
            subpath,
            subtest,
            args,
            eval_mode,
        }
    }

    pub fn term(&self) -> RichTerm {
        let path = self.path();

        let field_path = self.subtest.map(|s| format!(".{}", s)).unwrap_or_default();
        let content = format!(
            "(import \"{}\"){}.run {}",
            path.to_string_lossy(),
            field_path,
            self.args
                .iter()
                .map(|s| format!("({})", s))
                .collect::<Vec<String>>()
                .join(" ")
        );

        let content = if self.eval_mode == EvalMode::DeepSeq {
            format!("%deep_seq% ({}) true", content)
        } else {
            content
        };
        parse(&content).unwrap()
    }

    pub fn path(&self) -> PathBuf {
        let mut path = PathBuf::from(self.base_dir);
        path.push(format!("benches/{}.ncl", self.subpath));
        path
    }
}

pub fn bench_terms<'r>(rts: Vec<Bench<'r>>) -> Box<dyn Fn(&mut Criterion) + 'r> {
    let mut cache = Cache::new(ErrorTolerance::Strict);
    let Envs {
        eval_env,
        type_ctxt,
    } = cache.prepare_stdlib().unwrap();
    Box::new(move |c: &mut Criterion| {
        rts.iter().for_each(|bench| {
            let t = bench.term();
            c.bench_function(bench.name, |b| {
                b.iter_batched(
                    || {
                        let mut cache = cache.clone();
                        let id = cache.add_file(bench.path()).unwrap();
                        let (t, _) =
                            import_resolution::resolve_imports(t.clone(), &mut cache).unwrap();
                        (cache, id, t)
                    },
                    |(mut c_local, id, t)| {
                        if bench.eval_mode == EvalMode::TypeCheck {
                            c_local.typecheck(id, &type_ctxt).unwrap();
                        } else {
                            c_local.prepare(id, &type_ctxt).unwrap();
                            eval::eval(t, &eval_env, &mut c_local).unwrap();
                        }
                    },
                    criterion::BatchSize::LargeInput,
                )
            });
        })
    })
}

#[macro_export]
macro_rules! ncl_bench {
    {
        name = $name:literal,
        $( base_dir = $base_dir:literal, )?
        path = $subpath:literal,
        $( subtest = $subtest:literal, )?
        $( args = ( $( $arg:expr ),* ),)?
        $( eval_mode = $eval_mode:path,)?
    } => {
        $crate::Bench {
            $( base_dir: $base_dir,)?
                $( subtest: Some($subtest),)?
                $( args: vec![ $( $arg.to_string() ),* ],)?
                $( eval_mode: $eval_mode,)?
                ..$crate::Bench {
                    name: $name,
                    subpath: $subpath,
                    base_dir: env!("CARGO_MANIFEST_DIR"),
                    subtest: None,
                    args: vec![],
                    eval_mode: $crate::EvalMode::Normal,
                }
        }
    }
}

#[macro_export]
macro_rules! ncl_bench_group {
    (name = $group_name:ident; config = $config:expr; $($b:tt),+ $(,)*) => {
        pub fn $group_name() {
            use nickel_lang::{
                cache::{Envs, Cache, ErrorTolerance, ImportResolver},
                eval::eval,
                transform::import_resolution::resolve_imports,
            };

            let mut c: criterion::Criterion<_> = $config
                .configure_from_args();
            let mut cache = Cache::new(ErrorTolerance::Strict);
            let Envs {eval_env, type_ctxt} = cache.prepare_stdlib().unwrap();
            $(
                let bench = $crate::ncl_bench!$b;
                let t = bench.term();
                c.bench_function(bench.name, |b| {
                    b.iter_batched(
                        || {
                            let mut cache = cache.clone();
                            let id = cache.add_file(bench.path()).unwrap();
                            let (t, _) =
                                resolve_imports(t.clone(), &mut cache).unwrap();
                            if bench.eval_mode == $crate::EvalMode::TypeCheck {
                                cache.parse(id).unwrap();
                                cache.resolve_imports(id).unwrap();
                            }
                            (cache, id, t)
                        },
                        |(mut c_local, id, t)| {
                            if bench.eval_mode == $crate::EvalMode::TypeCheck {
                                c_local.typecheck(id, &type_ctxt).unwrap();
                            } else {
                                c_local.prepare(id, &type_ctxt).unwrap();
                                eval(t, &eval_env, &mut c_local).unwrap();
                            }
                        },
                        criterion::BatchSize::LargeInput,
                        )
                });
             )+
        }
    }
}
