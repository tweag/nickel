use std::{
    ffi::OsStr,
    path::{Path, PathBuf},
    rc::Rc,
};

use serde::Deserialize;

use crate::{
    cache_new::{CacheKey, ParsedEntry, ParsedState, SourceCache, SourceState},
    environment::Environment as GenericEnvironment,
    error::{Error, ImportError, ParseError, ParseErrors},
    eval::{
        self,
        cache::{Cache, CacheIndex},
        Closure,
    },
    identifier::Ident,
    parser::{self, lexer::Lexer, ErrorTolerantParser},
    position::TermPos,
    source::{Source, SourcePath},
    stdlib::{self, StdlibModule},
    term::{array::Array, RichTerm, Term},
    typecheck,
};

#[cfg(feature = "nix-experimental")]
use crate::nix_ffi;

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
pub fn parse(
    cache: &mut SourceCache,
    cache_key: CacheKey,
    format: InputFormat,
) -> Result<ParseErrors, ParseErrors> {
    match cache.get(cache_key) {
        SourceState::Added => {
            let (term, parse_errors) = parse_multi_nocache(cache, cache_key, format)?;
            cache.set_parsed(
                cache_key,
                ParsedEntry {
                    term,
                    state: ParsedState::Parsed { parse_errors },
                },
            );
            Ok(parse_errors)
        }
        SourceState::Parsed(ParsedEntry {
            state: ParsedState::Parsed { parse_errors },
            ..
        }) => Ok(parse_errors.clone()),
        _ => Ok(ParseErrors::none()),
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
    parse(cache, main, InputFormat::default()).strictly()?; // TODO(vkleen): does this always need to be strict? I think so,
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
        parse(cache, key, InputFormat::Nickel)
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

/// Supported input formats.
#[derive(Default, Clone, Copy, Eq, Debug, PartialEq)]
pub enum InputFormat {
    #[default]
    Nickel,
    Json,
    Yaml,
    Toml,
    #[cfg(feature = "nix-experimental")]
    Nix,
    Raw,
}

impl InputFormat {
    /// Returns an [InputFormat] based on the file extension of a path.
    pub fn from_path(path: &Path) -> Option<InputFormat> {
        match path.extension().and_then(OsStr::to_str) {
            Some("ncl") => Some(InputFormat::Nickel),
            Some("json") => Some(InputFormat::Json),
            Some("yaml") | Some("yml") => Some(InputFormat::Yaml),
            Some("toml") => Some(InputFormat::Toml),
            #[cfg(feature = "nix-experimental")]
            Some("nix") => Some(InputFormat::Nix),
            Some("txt") => Some(InputFormat::Raw),
            _ => None,
        }
    }
}

/// Resolve an import.
///
/// Read and store the content of an import, put it in the file cache (or get it from there if
/// it is cached) and return the corresponding cache key.
///
/// TODO(vkleen) check this description
/// The term and the path are provided only if the import is processed for the first time.
/// Indeed, at import resolution phase, the term of an import encountered for the first time is
/// queued to be processed (e.g. having its own imports resolved). The path is needed to
/// resolve nested imports relatively to this parent. Only after this processing the term is
/// inserted back in the cache. On the other hand, if it has been resolved before, it is
/// already transformed in the cache and does not need further processing.
pub fn resolve_import(
    cache: &mut SourceCache,
    path: impl AsRef<OsStr>,
    parent: Option<CacheKey>,
    pos: &TermPos,
) -> Result<CacheKey, ImportError> {
    // `parent` is the file that did the import. We first look in its containing directory.
    let mut parent_path = parent
        .and_then(|p| PathBuf::try_from(cache.source_path(p)).ok())
        .unwrap_or_default();
    parent_path.pop();

    let possible_parents: Vec<PathBuf> = std::iter::once(parent_path)
        .chain(cache.import_paths().map(PathBuf::from))
        .collect();

    // Try to import from all possibilities, taking the first one that succeeds.
    let (cache_key, path_buf) = possible_parents
        .iter()
        .find_map(|parent| {
            let mut path_buf = parent.clone();
            path_buf.push(path.as_ref());
            cache
                .load_from_filesystem(&path_buf)
                .ok()
                .map(|x| (x, path_buf))
        })
        .ok_or_else(|| {
            let parents = possible_parents
                .iter()
                .map(|p| p.to_string_lossy())
                .collect::<Vec<_>>();
            ImportError::IOError(
                path.as_ref().to_string_lossy().into_owned(),
                format!("could not find import (looked in [{}])", parents.join(", ")),
                *pos,
            )
        })?;

    if let Some(parent) = parent {
        cache.record_import(parent, cache_key);
        cache.record_rev_import(cache_key, parent);
    }

    parse(
        cache,
        cache_key,
        InputFormat::from_path(&path_buf).unwrap_or_default(),
    )
    .map_err(|err| ImportError::ParseErrors(err, *pos))?;

    Ok(cache_key)
}

/// Parse a source file in the [SourceCache], supporting multiple input formats. This function does
/// not update the state of the cache entry.
pub fn parse_multi_nocache(
    cache: &SourceCache,
    cache_key: CacheKey,
    format: InputFormat,
) -> Result<(RichTerm, ParseErrors), ParseError> {
    let attach_pos = |t: RichTerm| -> RichTerm {
        let pos: TermPos =
            crate::position::RawSpan::from_codespan(cache_key, cache.source_span(cache_key)).into();
        t.with_pos(pos)
    };

    let buf = cache.source(cache_key);

    match format {
        InputFormat::Nickel => {
            let (t, parse_errs) =
                parser::grammar::TermParser::new().parse_tolerant(cache_key, Lexer::new(buf))?;

            Ok((t, parse_errs))
        }
        InputFormat::Json => serde_json::from_str(cache.source(cache_key))
            .map(|t| (attach_pos(t), ParseErrors::default()))
            .map_err(|err| ParseError::from_serde_json(err, cache_key, cache)),
        InputFormat::Yaml => {
            // YAML files can contain multiple documents. If there is only
            // one we transparently deserialize it. If there are multiple,
            // we deserialize the file as an array.
            let de = serde_yaml::Deserializer::from_str(cache.source(cache_key));
            let mut terms = de
                .map(|de| {
                    RichTerm::deserialize(de)
                        .map(attach_pos)
                        .map_err(|err| (ParseError::from_serde_yaml(err, cache_key)))
                })
                .collect::<Result<Vec<_>, _>>()?;

            if terms.is_empty() {
                unreachable!(
                    "serde always produces at least one document, \
                        the empty string turns into `null`"
                )
            } else if terms.len() == 1 {
                Ok((
                    terms.pop().expect("we just checked the length"),
                    ParseErrors::default(),
                ))
            } else {
                Ok((
                    attach_pos(
                        Term::Array(
                            Array::new(Rc::from(terms.into_boxed_slice())),
                            Default::default(),
                        )
                        .into(),
                    ),
                    ParseErrors::default(),
                ))
            }
        }
        InputFormat::Toml => toml::from_str(cache.source(cache_key))
            .map(|t| (attach_pos(t), ParseErrors::default()))
            .map_err(|err| (ParseError::from_toml(err, cache_key))),
        #[cfg(feature = "nix-experimental")]
        InputFormat::Nix => {
            let json = nix_ffi::eval_to_json(cache.source(cache_key))
                .map_err(|e| ParseError::from_nix(e.what(), cache_key))?;
            serde_json::from_str(&json)
                .map(|t| (attach_pos(t), ParseErrors::default()))
                .map_err(|err| ParseError::from_serde_json(err, cache_key, cache))
        }
        InputFormat::Raw => Ok((
            attach_pos(Term::Str(cache.source(cache_key).into()).into()),
            ParseErrors::default(),
        )),
    }
}
