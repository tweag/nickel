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
    term::{
        array::Array,
        record::{Field, RecordData},
        RichTerm, SharedTerm, Term,
    },
    transform::{
        self,
        import_resolution::{strict, tolerant},
    },
    typ::UnboundTypeVariableError,
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

pub trait Strictly {
    type Success;
    type Errors;
    fn strictly(self) -> Result<Self::Success, Self::Errors>;
}

impl Strictly for Result<ParseErrors, ParseErrors> {
    type Success = ();
    type Errors = ParseErrors;

    fn strictly(self) -> Result<Self::Success, Self::Errors> {
        let e = self?;
        if !e.no_errors() {
            Err(e)
        } else {
            Ok(())
        }
    }
}

impl Strictly for tolerant::ResolveResult {
    type Success = strict::ResolveResult;
    type Errors = ImportError;

    fn strictly(self) -> Result<Self::Success, Self::Errors> {
        let tolerant::ResolveResult {
            transformed_term,
            resolved_keys,
            mut import_errors,
        } = self;
        if let Some(e) = import_errors.pop() {
            Err(e)
        } else {
            Ok(strict::ResolveResult {
                transformed_term,
                resolved_keys,
            })
        }
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
                    state: ParsedState::Parsed {
                        parse_errors: parse_errors.clone(),
                    },
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

pub fn resolve_imports(cache: &mut SourceCache, cache_key: CacheKey) -> tolerant::ResolveResult {
    let ParsedEntry { term, state } = cache
        .get(cache_key)
        .parsed()
        .expect("Source should be parsed before import resolution is attempted");
    if matches!(
        state,
        ParsedState::ImportsResolved | ParsedState::Typechecked { .. } | ParsedState::Transformed
    ) {
        return tolerant::ResolveResult {
            transformed_term: term.clone(),
            resolved_keys: Vec::new(),
            import_errors: Vec::new(),
        };
    }

    let tolerant::ResolveResult {
        transformed_term,
        resolved_keys: pending,
        import_errors: mut all_import_errors,
    } = tolerant::resolve_imports(term.clone(), cache);
    cache.set_parsed(
        cache_key,
        ParsedEntry {
            term: transformed_term.clone(),
            state: ParsedState::ImportsResolved,
        },
    );

    let mut done = Vec::new();
    for key in pending {
        let tolerant::ResolveResult {
            mut resolved_keys,
            mut import_errors,
            ..
        } = resolve_imports(cache, key);
        done.push(key);
        done.append(&mut resolved_keys);
        all_import_errors.append(&mut import_errors);
    }

    tolerant::ResolveResult {
        transformed_term,
        resolved_keys: done,
        import_errors: all_import_errors,
    }
}

/// Typecheck an entry of the cache and update its state accordingly, or do nothing if the
/// entry has already been typechecked. Requires that the corresponding source has been parsed
/// and its imports resolved.
/// If the source contains imports which have been resolved, recursively typecheck on the imports too.
pub fn typecheck(
    cache: &mut SourceCache,
    cache_key: CacheKey,
    context: &typecheck::Context,
) -> Result<(), Error> {
    let state = cache
        .get(cache_key)
        .parsed()
        .expect("Source should be parsed before typechecking is attempted");
    match state {
        ParsedEntry {
            state: ParsedState::Typechecked { .. },
            ..
        }
        | ParsedEntry {
            state: ParsedState::Transformed,
            ..
        } => Ok(()),
        ParsedEntry {
            term,
            state: ParsedState::ImportsResolved,
        }
        | ParsedEntry {
            term,
            state: ParsedState::Parsed { .. },
        } => {
            let wildcards = typecheck::type_check(term, context.clone(), cache)?;
            cache.set_parsed_state(cache_key, ParsedState::Typechecked { wildcards });

            for import in cache.get_imports(cache_key).collect::<Vec<_>>().into_iter() {
                typecheck(cache, import, context)?;
            }

            Ok(())
        }
    }
}

/// Apply program transformations to an entry of the cache, and update its state accordingly,
/// or do nothing if the entry has already been transformed. Require that the corresponding
/// source has been parsed.
/// If the source contains imports which hav ebeen resolved, recursively perform transformations on the imports too.
pub fn transform(cache: &mut SourceCache, cache_key: CacheKey) -> Result<(), Error> {
    let entry = cache
        .get(cache_key)
        .parsed()
        .expect("Source should be parsed before transformations are attempted");
    if matches!(entry.state, ParsedState::Transformed) {
        return Ok(());
    }

    let wildcards = match &entry.state {
        ParsedState::Typechecked { wildcards } => Some(wildcards),
        _ => None,
    };

    let term = transform::transform(entry.term.clone(), wildcards)
        .map_err(|e| Error::TypecheckError(e.into()))?;

    cache.set_parsed(
        cache_key,
        ParsedEntry {
            term,
            state: ParsedState::Transformed,
        },
    );

    for import in cache.get_imports(cache_key).collect::<Vec<_>>().into_iter() {
        transform(cache, import)?;
    }

    Ok(())
}

/// Apply program transformations to all the fields of a record.
///
/// Used to transform stdlib modules and other records loaded in the environment, when using
/// e.g. the `load` command of the REPL. If one just uses [Self::transform], the share normal
/// form transformation would add let bindings to a record entry `{ ... }`, turning it into
/// `let %0 = ... in ... in { ... }`. But stdlib entries are required to be syntactically
/// records.
///
/// Note that this requirement may be relaxed in the future by e.g. evaluating stdlib entries
/// before adding their fields to the initial environment.
///
/// # Preconditions
///
/// - the entry must syntactically be a record (`Record` or `RecRecord`). Otherwise, this
/// function panics
pub fn transform_inner(cache: &mut SourceCache, cache_key: CacheKey) -> Result<(), Error> {
    let entry = cache
        .get(cache_key)
        .parsed()
        .expect("Source should be parsed before transformations are attempted");
    if matches!(entry.state, ParsedState::Transformed) {
        return Ok(());
    }

    let wildcards = match &entry.state {
        ParsedState::Typechecked { wildcards } => Some(wildcards),
        _ => None,
    };

    let mut term = entry.term.clone();

    match SharedTerm::make_mut(&mut term.term) {
        Term::Record(RecordData { ref mut fields, .. }) => {
            let map_res: Result<_, UnboundTypeVariableError> = std::mem::take(fields)
                .into_iter()
                .map(|(id, field)| {
                    Ok((
                        id,
                        field.try_map_value(|v| transform::transform(v, wildcards))?,
                    ))
                })
                .collect();
            *fields = map_res.map_err(|e| Error::TypecheckError(e.into()))?;
        }
        Term::RecRecord(ref mut record, ref mut dyn_fields, ..) => {
            let map_res: Result<_, UnboundTypeVariableError> = std::mem::take(&mut record.fields)
                .into_iter()
                .map(|(id, field)| {
                    Ok((
                        id,
                        field.try_map_value(|v| transform::transform(v, wildcards))?,
                    ))
                })
                .collect();

            let dyn_fields_res: Result<_, UnboundTypeVariableError> = std::mem::take(dyn_fields)
                .into_iter()
                .map(|(id_t, mut field)| {
                    let value = field
                        .value
                        .take()
                        .map(|v| transform::transform(v, wildcards))
                        .transpose()?;

                    Ok((
                        transform::transform(id_t, wildcards)?,
                        Field { value, ..field },
                    ))
                })
                .collect();

            record.fields = map_res.map_err(|e| Error::TypecheckError(e.into()))?;
            *dyn_fields = dyn_fields_res.map_err(|e| Error::TypecheckError(e.into()))?;
        }
        _ => panic!("cache::transform_inner(): not a record"),
    }

    cache.set_parsed(
        cache_key,
        ParsedEntry {
            term,
            state: ParsedState::Transformed,
        },
    );

    for import in cache.get_imports(cache_key).collect::<Vec<_>>().into_iter() {
        transform(cache, import)?;
    }

    Ok(())
}

pub fn prepare(cache: &mut SourceCache, main: CacheKey, envs: &InitialEnvs) -> Result<(), Error> {
    parse(cache, main, InputFormat::default()).strictly()?; // TODO(vkleen): does this always need to be strict? I think so,
                                                            // but let's check
    resolve_imports(cache, main).strictly()?;
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
    load_stdlib(cache);
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
pub fn resolve_path(
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
        .or_else(|| {
            // If the import can't be found in the filesystem under an absolute path, try to see if
            // it made its way into the cache some other way as a last resort.
            let path_buf = PathBuf::try_from(path.as_ref()).ok()?;
            cache
                .find(&SourcePath::Path(path_buf.clone()))
                .map(|k| (k, path_buf))
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

    // We treat parse errors strictly, because we'll accumulate parse errors into import errors
    // anyways. Those are then passed up to the caller of `resolve_imports` to decide what to do
    // with them.
    parse(
        cache,
        cache_key,
        InputFormat::from_path(&path_buf).unwrap_or_default(),
    )
    .strictly()
    .map_err(|err| ImportError::ParseErrors(err, *pos))?;

    Ok(cache_key)
}

/// Parse a source file in the [SourceCache] as Nickel code. This function does
/// not update the state of the cache entry.
pub fn parse_nocache(
    cache: &SourceCache,
    cache_key: CacheKey,
) -> Result<(RichTerm, ParseErrors), ParseError> {
    parse_multi_nocache(cache, cache_key, InputFormat::default())
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
