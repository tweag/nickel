use criterion::Criterion;
use nickel_lang_core::{
    cache::{Cache, Envs, ErrorTolerance},
    eval::{
        cache::{Cache as EvalCache, CacheImpl},
        VirtualMachine,
    },
    term::RichTerm,
    transform::import_resolution,
};

use std::path::PathBuf;

use crate::test_program::parse;

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
    pub fn new(
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

        let field_path = self.subtest.map(|s| format!(".{s}")).unwrap_or_default();
        let content = format!(
            "(import \"{}\"){}.run {}",
            path.to_string_lossy(),
            field_path,
            self.args
                .iter()
                .map(|s| format!("({s})"))
                .collect::<Vec<String>>()
                .join(" ")
        );

        let content = if self.eval_mode == EvalMode::DeepSeq {
            format!("%deep_seq% ({content}) true")
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
    let mut eval_cache = CacheImpl::new();
    let Envs {
        eval_env,
        type_ctxt,
    } = cache.prepare_stdlib(&mut eval_cache).unwrap();
    Box::new(move |c: &mut Criterion| {
        rts.iter().for_each(|bench| {
            let t = bench.term();
            c.bench_function(bench.name, |b| {
                b.iter_batched(
                    || {
                        let mut cache = cache.clone();
                        let id = cache.add_file(bench.path()).unwrap();
                        let t = import_resolution::strict::resolve_imports(t.clone(), &mut cache)
                            .unwrap()
                            .transformed_term;
                        (cache, id, t)
                    },
                    |(mut c_local, id, t)| {
                        if bench.eval_mode == EvalMode::TypeCheck {
                            c_local.typecheck(id, &type_ctxt).unwrap();
                        } else {
                            c_local.prepare(id, &type_ctxt).unwrap();

                            VirtualMachine::new_with_cache(
                                c_local,
                                eval_cache.clone(),
                                std::io::sink(),
                            )
                            .with_initial_env(eval_env.clone())
                            .eval(t)
                            .unwrap();
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
        $crate::bench::Bench {
            $( base_dir: $base_dir,)?
                $( subtest: Some($subtest),)?
                $( args: vec![ $( $arg.to_string() ),* ],)?
                $( eval_mode: $eval_mode,)?
                ..$crate::bench::Bench {
                    name: $name,
                    subpath: $subpath,
                    base_dir: env!("CARGO_MANIFEST_DIR"),
                    subtest: None,
                    args: vec![],
                    eval_mode: $crate::bench::EvalMode::Normal,
                }
        }
    }
}

#[macro_export]
macro_rules! ncl_bench_group {
    (name = $group_name:ident; config = $config:expr; $($b:tt),+ $(,)*) => {
        pub fn $group_name() {
            use nickel_lang_core::{
                cache::{Envs, Cache, ErrorTolerance, ImportResolver},
                eval::{VirtualMachine, cache::{CacheImpl, Cache as EvalCache}},
                transform::import_resolution::strict::resolve_imports,
                error::report::{report, ColorOpt, ErrorFormat},
            };

            let mut c: criterion::Criterion<_> = $config
                .configure_from_args();
            let mut cache = Cache::new(ErrorTolerance::Strict);
            let mut eval_cache = CacheImpl::new();
            let Envs {eval_env, type_ctxt} = cache.prepare_stdlib(&mut eval_cache).unwrap();
            $(
                let bench = $crate::ncl_bench!$b;
                let t = bench.term();
                c.bench_function(bench.name, |b| {
                    b.iter_batched(
                        || {
                            let mut cache = cache.clone();
                            let id = cache.add_file(bench.path()).unwrap();
                            let t = resolve_imports(t.clone(), &mut cache)
                                .unwrap()
                                .transformed_term;
                            if bench.eval_mode == $crate::bench::EvalMode::TypeCheck {
                                cache.parse(id).unwrap();
                                cache.resolve_imports(id).unwrap();
                            }
                            (cache, id, t)
                        },
                        |(mut c_local, id, t)| {
                            if bench.eval_mode == $crate::bench::EvalMode::TypeCheck {
                                c_local.typecheck(id, &type_ctxt).unwrap();
                            } else {
                                c_local.prepare(id, &type_ctxt).unwrap();

                                let mut vm = VirtualMachine::new_with_cache(
                                    c_local,
                                    eval_cache.clone(),
                                    std::io::sink()
                                )
                                .with_initial_env(eval_env.clone());

                                if let Err(e) = vm.eval(t) {
                                    report(
                                        vm.import_resolver_mut(),
                                        e,
                                        ErrorFormat::Text,
                                        ColorOpt::default(),
                                    );
                                    panic!("Error during bench evaluation");
                                }
                            }
                        },
                        criterion::BatchSize::LargeInput,
                        )
                });
             )+
        }
    }
}
