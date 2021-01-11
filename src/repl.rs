//! The Nickel REPL.
use crate::cache::Cache;
use crate::error::{
    Error, EvalError, IOError, ImportError, ParseError, ToDiagnostic, TypecheckError,
};
use crate::identifier::Ident;
use crate::parser::lexer::Lexer;
use crate::position::RawSpan;
use crate::stdlib as nickel_stdlib;
use crate::term::{RichTerm, Term};
use crate::typecheck::type_check;
use crate::types::{AbsType, Types};
use crate::{eval, parser, transformations, typecheck};
use codespan::{FileId, Files};
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use simple_counter::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ffi::OsStr;
use std::ffi::OsString;
use std::fs;
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::result::Result;

generate_counter!(InputNameCounter, usize);

/// Interface of the REPL.
pub trait REPL {
    fn eval(&mut self, exp: &str) -> Result<Term, Error>;
    fn load(&mut self, path: impl AsRef<OsStr>) -> Result<RichTerm, Error>;
    fn typecheck(&mut self, exp: &str) -> Result<Types, Error>;
    fn query(&self, exp: &str) -> Result<String, Error>;
}

/// Implementation of the REPL.
pub struct REPLImpl {
    cache: Cache,
    eval_env: eval::Environment,
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

        // Check that the entry is a record, otherwise transform_inner panics
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
        eval::env_add_term(&mut self.eval_env, self.cache.get_owned(file_id).unwrap()).unwrap();
        //TODO: typecheck::env_add_term(..)
        Ok(self.cache.get_owned(file_id).unwrap())
    }

    fn typecheck(&mut self, exp: &str) -> Result<Types, Error> {
        let file_id = self.cache.add_string(
            format!("repl-input-{}", InputNameCounter::next()),
            String::from(exp),
        );

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

    fn query(&self, exp: &str) -> Result<String, Error> {
        panic!("not implemented")
    }
}
