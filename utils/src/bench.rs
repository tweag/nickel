use criterion::Criterion;
use nickel_lang_core::term::RichTerm;

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
            "(import {:?}){}.run {}",
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
        parse(&content).unwrap_or_else(|err| panic!("Failed parsing {path:?}: {err:?}"))
    }

    pub fn path(&self) -> PathBuf {
        PathBuf::from_iter([
            self.base_dir,
            "benches",
            format!("{}.ncl", self.subpath).as_str(),
        ])
    }
}

/// Create a `Criterion` config. Uses `PProfProfiler` when `pprof` is enabled on Unix systems.
pub fn criterion_config() -> Criterion {
    let config = Criterion::default();
    #[cfg(all(target_family = "unix", feature = "pprof"))]
    let config = config.with_profiler(pprof::criterion::PProfProfiler::new(
        100,
        pprof::criterion::Output::Flamegraph(None),
    ));
    config
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
                cache::{CacheHub, ImportResolver, InputFormat},
                eval::{VirtualMachine, cache::{CacheImpl, Cache as EvalCache}},
                transform::import_resolution::strict::resolve_imports,
                typecheck::TypecheckMode,
                error::report::{report, ColorOpt, ErrorFormat},
            };

            let mut c: criterion::Criterion<_> = $config
                .configure_from_args();
            let mut cache = CacheHub::new();
            let mut eval_cache = CacheImpl::new();
            cache.prepare_stdlib().unwrap();
            let eval_env = cache.mk_eval_env(&mut eval_cache);
            $(
                let bench = $crate::ncl_bench!$b;
                let runner = bench.term();
                c.bench_function(bench.name, |b| {
                    b.iter_batched(
                        || {
                            let mut runner = runner.clone();
                            let mut cache = if matches!(bench.eval_mode, $crate::bench::EvalMode::TypeCheck) {
                                // For typechecking, we need to have the stdlib loaded in the AST
                                // cache, which isn't provided by `clone_for_eval()`. At this point
                                // it's just simpler to populate the cache from scratch.
                                let mut cache = CacheHub::new();
                                cache.prepare_stdlib().unwrap();
                                cache
                            }
                            else {
                                cache.clone_for_eval()
                            };

                            let id = cache.sources.add_file(bench.path(), InputFormat::Nickel).unwrap();

                            if matches!(bench.eval_mode, $crate::bench::EvalMode::TypeCheck) {
                                cache.parse_to_term(id, nickel_lang_core::cache::InputFormat::Nickel).unwrap();
                            } else{
                                cache.prepare_eval_only(id).unwrap();
                                runner = resolve_imports(runner, &mut cache).unwrap().transformed_term;
                            }

                            (cache, id, runner)
                        },
                        |(mut c_local, id, runner)| {
                            if matches!(bench.eval_mode, $crate::bench::EvalMode::TypeCheck) {
                                c_local.typecheck(id, TypecheckMode::Walk).unwrap();
                            } else {
                                let mut vm = VirtualMachine::new_with_cache(
                                    c_local,
                                    eval_cache.clone(),
                                    std::io::sink(),
                                    nickel_lang_core::error::NullReporter {},
                                )
                                .with_initial_env(eval_env.clone());

                                if let Err(e) = vm.eval(runner) {
                                    report(
                                        &mut vm.import_resolver_mut().files().clone(),
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
