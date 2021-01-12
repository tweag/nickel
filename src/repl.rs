//! The Nickel REPL.
//!
//! A backend designates a module which actually execute a sequence of REPL commands, while being
//! agnostic to the user interface and the presentation of the results.
//!
//! Dually, the frontend is the user-facing part, which may be a CLI, a web application, a
//! jupyter-kernel (which is not exactly user-facing, but still manages input/output and
//! formatting), etc.
use crate::cache::Cache;
use crate::error::{Error, EvalError, IOError};
use crate::term::{RichTerm, Term};
use crate::types::{AbsType, Types};
use crate::{eval, typecheck};
use simple_counter::*;
use std::ffi::OsStr;
use std::ffi::OsString;
use std::result::Result;

generate_counter!(InputNameCounter, usize);

/// Interface of the REPL backend.
pub trait REPL {
    /// Eval an expression.
    fn eval(&mut self, exp: &str) -> Result<Term, Error>;
    /// Load the content of a file in the environment.
    fn load(&mut self, path: impl AsRef<OsStr>) -> Result<RichTerm, Error>;
    /// Typecheck an expression and return the apparent type.
    fn typecheck(&mut self, exp: &str) -> Result<Types, Error>;
    /// Query the metadata of an expression.
    fn query(&mut self, exp: &str, path: Option<&str>) -> Result<Term, Error>;
}

/// Standard implementation of the REPL backend.
pub struct REPLImpl {
    /// The underlying cache, storing input, loaded files and parsed terms.
    cache: Cache,
    /// The eval environment. Contain the global environment containing the stdlib, plus
    /// declarations and loadings made in the REPL.
    eval_env: eval::Environment,
    /// The type environment, counterpart of the eval environment for typechecking. Entires are
    /// `TypeWrapper` for the ease of interacting with the typechecker, but there should not be any
    /// unification variable in it.
    type_env: typecheck::Environment,
}

impl REPLImpl {
    /// Create a new REPL with the stdlib processed, loaded in the environment, and the
    /// corresponding typing environment.
    pub fn new() -> Result<Self, Error> {
        let mut cache = Cache::new();
        cache.prepare_stdlib()?;

        let eval_env = cache.mk_global_env().unwrap();
        let type_env = typecheck::Envs::mk_global(&eval_env);

        Ok(REPLImpl {
            cache,
            eval_env,
            type_env,
        })
    }
}

impl REPL for REPLImpl {
    fn eval(&mut self, exp: &str) -> Result<Term, Error> {
        let file_id = self.cache.add_string(
            format!("repl-input-{}", InputNameCounter::next()),
            String::from(exp),
        );
        self.cache.prepare(file_id, &self.eval_env)?;
        Ok(eval::eval(
            self.cache.get_owned(file_id).expect(
                "repl::eval(): term was prepared for evaluation but it is missing from the term cache",
            ),
            &self.eval_env,
            &mut self.cache,
        )?)
    }

    fn load(&mut self, path: impl AsRef<OsStr>) -> Result<RichTerm, Error> {
        let file_id = self
            .cache
            .add_file(OsString::from(path.as_ref()))
            .map_err(IOError::from)?;
        self.cache.parse(file_id)?;
        let RichTerm { term, pos } = self.cache.get_ref(file_id).unwrap();

        // Check that the entry is a record, which is a precondition of transform_inner
        match term.as_ref() {
            Term::Record(_) | Term::RecRecord(_) => (),
            _ => {
                return Err(Error::EvalError(EvalError::Other(
                    String::from("load: expected a record"),
                    pos.clone(),
                )))
            }
        };
        self.cache.transform_inner(file_id).map_err(|err| {
            err.unwrap_error("load(): expected term to be parsed before transformation")
        })?;

        let term = self.cache.get_owned(file_id).unwrap();
        typecheck::Envs::env_add_term(&mut self.type_env, &term).unwrap();
        eval::env_add_term(&mut self.eval_env, term.clone()).unwrap();

        Ok(term)
    }

    fn typecheck(&mut self, exp: &str) -> Result<Types, Error> {
        let file_id = self.cache.add_tmp("<repl-typecheck>", String::from(exp));
        self.cache.parse(file_id)?;
        self.cache
            .typecheck_in_env(file_id, &self.type_env)
            .map_err(|cache_err| {
                cache_err.unwrap_error("repl: expected source to be parsed before typechecking")
            })?;

        Ok(
            typecheck::apparent_type(self.cache.get_ref(file_id).unwrap().as_ref())
                .unwrap_or(Types(AbsType::Dyn())),
        )
    }

    fn query(&mut self, exp: &str, path: Option<&str>) -> Result<Term, Error> {
        use crate::program;

        let file_id = self.cache.add_tmp("<repl-query>", String::from(exp));
        program::query(
            &mut self.cache,
            file_id,
            &self.eval_env,
            path.map(String::from),
        )
    }
}
