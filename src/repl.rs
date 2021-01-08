//! The Nickel REPL.
use crate::cache::Cache;
use crate::error::{Error, EvalError, ImportError, ParseError, ToDiagnostic, TypecheckError};
use crate::identifier::Ident;
use crate::parser::lexer::Lexer;
use crate::position::RawSpan;
use crate::stdlib as nickel_stdlib;
use crate::term::{RichTerm, Term};
use crate::typecheck::type_check;
use crate::types::Types;
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
    fn load(&mut self, path: impl AsRef<OsStr>) -> Result<Term, EvalError>;
    fn typecheck(&mut self, exp: &str) -> Result<Types, TypecheckError>;
    fn type_of(&mut self, exp: &str) -> Result<Types, Error>;
    fn query(&self, exp: &str) -> Result<String, Error>;
}

/// Implementation of the REPL.
pub struct REPLImpl {
    cache: Cache,
    eval_env: eval::Environment,
    type_env: typecheck::Environment,
}

impl REPLImpl {
    pub fn new() -> Self {
        REPLImpl {
            cache: Cache::new(),
            eval_env: eval::Environment::new(),
            type_env: typecheck::Environment::new(),
        }
    }
}

impl REPL for REPLImpl {
    fn eval(&mut self, exp: &str) -> Result<Term, Error> {
        let file_id = self.cache.add_string(
            format!("input-{}", InputNameCounter::next()),
            String::from(exp),
        );
        self.cache.prepare(file_id, &self.eval_env)?;
        Ok(eval::eval(
            self.cache.get_owned(file_id).expect(
                "repl::eval(): term was prepared for evaluation but is missing from the term cache",
            ),
            &self.eval_env,
            &mut self.cache,
        )?)
    }

    fn load(&mut self, path: impl AsRef<OsStr>) -> Result<Term, EvalError> {
        panic!("not implemented")
    }
    fn typecheck(&mut self, exp: &str) -> Result<Types, TypecheckError> {
        panic!("not implemented")
    }
    fn type_of(&mut self, exp: &str) -> Result<Types, Error> {
        panic!("not implemented")
    }
    fn query(&self, exp: &str) -> Result<String, Error> {
        panic!("not implemented")
    }
}
