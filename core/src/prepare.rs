use crate::{
    cache_new::{CacheKey, ParsedEntry, ParsedState, SourceCache, SourceState},
    environment::Environment as GenericEnvironment,
    error::{Error, ImportError, ParseErrors},
    eval::{
        self,
        cache::{Cache, CacheIndex},
        Closure,
    },
    identifier::Ident,
    parser::{parse_multi, InputFormat},
    source::{Source, SourcePath},
    stdlib::{self, StdlibModule},
    term::RichTerm,
    typecheck,
};

#[allow(type_alias_bounds)] // TODO: Look into this warning.
pub type Environment = GenericEnvironment<Ident, CacheIndex>;

/// The initial environments for evaluation and typechecking.
#[derive(Debug, Clone)]
pub struct InitialEnvs {
    /// The eval environment.
    pub eval_env: Environment,
    /// The typing context.
    pub type_ctxt: typecheck::Context,
}

impl typecheck::Context {
    pub fn from_stdlib(cache: &SourceCache) -> Self {
        typecheck::mk_initial_ctxt(stdlib_terms(cache).as_slice()).unwrap()
    }
}

impl InitialEnvs {
    pub fn new() -> Self {
        InitialEnvs {
            eval_env: Environment::new(),
            type_ctxt: typecheck::Context::new(),
        }
    }

    /// Build an initial environment from the standard library modules loaded in the
    /// [SourceCache]. Note that this function will not load standard library modules into
    /// the cache or process them if they are not already loaded.
    pub fn from_stdlib<C: Cache>(cache: &mut SourceCache, eval_cache: &mut C) -> Self {
        let stdlib_terms = stdlib_terms(cache);

        let type_ctxt = typecheck::mk_initial_ctxt(stdlib_terms.as_slice()).unwrap();

        let mut eval_env = Environment::new();
        for (module, key) in cache.stdlib_modules().iter().copied() {
            // The internals module needs special treatment: it's required to be a record
            // literal, and its bindings are added directly to the environment
            if let stdlib::StdlibModule::Internals = module {
                let result = eval::env_add_record(
                    eval_cache,
                    &mut eval_env,
                    Closure::atomic_closure(
                        cache
                            .term_owned(key)
                            .expect("The standard library is parsed whenever it is loaded"),
                    ),
                );
                if let Err(eval::EnvBuildError::NotARecord(rt)) = result {
                    panic!(
                        "InitialEnvs::from_stdlib: \
                        expected the stdlib module {} to be a record, got {:?}",
                        cache.source_path(key),
                        rt
                    )
                }
            } else {
                eval::env_add(
                    eval_cache,
                    &mut eval_env,
                    module.name().into(),
                    cache
                        .term_owned(key)
                        .expect("The standard library is parsed whenever it is loaded"),
                    Environment::new(),
                )
            }
        }

        Self {
            eval_env,
            type_ctxt,
        }
    }
}

impl Default for InitialEnvs {
    fn default() -> Self {
        Self::new()
    }
}

/// Parse a source and populate the corresponding entry in the cache, or do
/// nothing if the entry has already been parsed.
pub fn parse(cache: &mut SourceCache, cache_key: CacheKey) -> Result<ParseErrors, ParseErrors> {
    match cache.get(cache_key) {
        SourceState::Added => {
            let (term, parse_errors) = parse_multi(cache, cache_key, InputFormat::default())?;
            cache.record_parse_errors(cache_key, parse_errors.clone());
            cache.set_parsed(
                cache_key,
                ParsedEntry {
                    term,
                    state: ParsedState::Parsed,
                },
            );
            Ok(parse_errors)
        }
        _ => Ok(cache
            .get_parse_errors(cache_key)
            .expect("Any parsed entry should have a corresponding entry in parse_errors")
            .clone()),
    }
}

pub fn parse_nocache(
    cache: &SourceCache,
    cache_key: CacheKey,
) -> Result<(RichTerm, ParseErrors), ParseErrors> {
    todo!()
}

pub fn resolve_imports(cache: &mut SourceCache, main: CacheKey) -> Result<(), Error> {
    todo!()
}

pub fn resolve_imports_lax(
    cache: &mut SourceCache,
    main: CacheKey,
) -> (Vec<CacheKey>, Vec<ImportError>) {
    todo!()
}

pub fn typecheck(
    cache: &mut SourceCache,
    main: CacheKey,
    envs: &typecheck::Context,
) -> Result<(), Error> {
    todo!()
}

pub fn transform(cache: &mut SourceCache, main: CacheKey) -> Result<(), Error> {
    todo!()
}

pub fn transform_inner(cache: &mut SourceCache, main: CacheKey) -> Result<(), Error> {
    todo!()
}

pub fn prepare(cache: &mut SourceCache, main: CacheKey, envs: &InitialEnvs) -> Result<(), Error> {
    parse(cache, main).strictly()?; // TODO(vkleen): does this always need to be strict? I think so,
                                    // but let's check
    resolve_imports(cache, main)?;
    typecheck(cache, main, &envs.type_ctxt)?;
    transform(cache, main)?;
    Ok(())
}

/// Load and parse the Nickel standard library
pub fn load_stdlib(cache: &mut SourceCache) {
    for module in stdlib::modules() {
        let key = cache.insert(
            SourcePath::Std(module),
            Source::Memory {
                source: module.content().to_string(),
            },
        );
        parse(cache, key)
            .strictly()
            .expect("The standard library should always parse correctly");
    }
}

/// Run program transformations on the loaded stdlib modules in `cache` to prepare for evaluation
pub fn prepare_stdlib(cache: &mut SourceCache) -> Result<(), Error> {
    let stdlib_modules: Vec<_> = cache.stdlib_modules().to_vec();
    for (module, key) in stdlib_modules {
        if let stdlib::StdlibModule::Internals = module {
            transform_inner(cache, key)?;
        } else {
            transform(cache, key)?;
        }
    }
    Ok(())
}

/// Typecheck the standard library. Currently only used in the test suite.
pub fn typecheck_stdlib(cache: &mut SourceCache) -> Result<(), Error> {
    // We have a small bootstraping problem: to typecheck the initial environment, we already
    // need an initial evaluation environment, since stdlib parts may reference each other. But
    // typechecking is performed before program transformations, so this environment is not
    // final one. We have to create a temporary initial environment just for typechecking, which is
    // dropped right after. However:
    // 1. The stdlib is meant to stay relatively light.
    // 2. Typechecking the standard library ought to occur only during development. Once the
    //    stdlib is stable, we won't have to typecheck it at every execution.
    let initial_ctxt = typecheck::Context::from_stdlib(cache);
    typecheck_stdlib_(cache, &initial_ctxt)
}

/// This is function is here to decouple building the stdlib typechecking context from actually
/// typechecking the stdlib. This way they can be benchmarked seperately.
pub fn typecheck_stdlib_(
    cache: &mut SourceCache,
    initial_ctxt: &typecheck::Context,
) -> Result<(), Error> {
    for id in cache.stdlib_keys() {
        typecheck(cache, id, initial_ctxt)?;
    }
    Ok(())
}

pub fn stdlib_terms(cache: &SourceCache) -> Vec<(StdlibModule, RichTerm)> {
    cache
        .stdlib_modules()
        .iter()
        .copied()
        .map(|(module, key)| {
            (
                module,
                cache
                    .term_owned(key)
                    .expect("The standard library is parsed whenever it is loaded"),
            )
        })
        .collect()
}

pub trait ParseResultExt {
    type Errors;
    fn strictly(self) -> Result<(), Self::Errors>;
}

impl ParseResultExt for Result<ParseErrors, ParseErrors> {
    type Errors = ParseErrors;

    fn strictly(self) -> Result<(), Self::Errors> {
        let e = self?;
        if !e.no_errors() {
            Err(e)
        } else {
            Ok(())
        }
    }
}
