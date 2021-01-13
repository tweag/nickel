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
    /// Required for error reporting on the frontend.
    fn cache_mut<'a>(&'a mut self) -> &'a mut Cache;
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
    /// Create a new empty REPL.
    pub fn new() -> Self {
        let mut cache = Cache::new();

        REPLImpl {
            cache,
            eval_env: eval::Environment::new(),
            type_env: typecheck::Environment::new(),
        }
    }

    /// Load and process the stdlib, and use it to populate the eval environment as well as the
    /// typing environment.
    pub fn load_stdlib(&mut self) -> Result<(), Error> {
        self.cache.prepare_stdlib()?;

        self.eval_env = self.cache.mk_global_env().unwrap();
        self.type_env = typecheck::Envs::mk_global(&self.eval_env);
        Ok(())
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

    fn cache_mut<'a>(&'a mut self) -> &'a mut Cache {
        &mut self.cache
    }
}

#[cfg(feature = "repl")]
pub mod rustyline_frontend {
    use super::*;
    use crate::program::report;
    use rustyline::completion::{Completer, FilenameCompleter, Pair};
    use rustyline::config::OutputStreamType;
    use rustyline::error::ReadlineError;
    use rustyline::highlight::{Highlighter, MatchingBracketHighlighter};
    use rustyline::hint::{Hinter, HistoryHinter};
    use rustyline::validate::{self, MatchingBracketValidator, Validator};
    use rustyline::{Cmd, CompletionType, Config, Context, EditMode, Editor, KeyEvent};

    /// Error when initializing the REPL.
    pub enum InitError {
        /// Unable to load, parse or typecheck the stdlib
        Stdlib,
    }

    pub fn config() -> Config {
        Config::builder()
            .history_ignore_space(true)
            .edit_mode(EditMode::Vi)
            .output_stream(OutputStreamType::Stdout)
            .build()
    }

    pub fn repl() -> Result<(), InitError> {
        let mut repl = REPLImpl::new();
        match repl.load_stdlib() {
            Ok(()) => (),
            Err(err) => {
                report(repl.cache_mut(), err);
                return Err(InitError::Stdlib);
            }
        }

        let config = config();
        let mut editor: Editor<()> = Editor::with_config(config);
        let prompt = format!("\x1b[1;32m{}\x1b[0m", "> ");

        loop {
            match editor.readline(&prompt) {
                Ok(line) if line.starts_with(":") => {
                    let cmd_end = line.find(" ").unwrap_or(line.len());
                    let command: String = line.chars().skip(1).take(cmd_end - 1).collect();
                    let arg: String = line.chars().skip(cmd_end + 1).collect();

                    let result = match command.as_str() {
                        "load" => repl.load(&arg).map(|term| match term.as_ref() {
                            Term::Record(map) | Term::RecRecord(map) => {
                                println!("Loaded {} symbol(s) in the environment.", map.len())
                            }
                            _ => (),
                        }),
                        "typecheck" => repl.typecheck(&arg).map(|types| println!("Ok: {}", types)),
                        "query" => {
                            println!("Not implemented");
                            Ok(())
                        }
                        "?" | "help" => {
                            help();
                            Ok(())
                        }
                        cmd => {
                            println!("Unknown command `{}`. Type ':?' or ':help' for a list of available commands", cmd);
                            Ok(())
                        }
                    };

                    if let Err(err) = result {
                        report(repl.cache_mut(), err);
                    }
                }
                Ok(line) => {
                    match repl.eval(&line) {
                        Ok(t) => println!("Ok: {}", t.shallow_repr()),
                        Err(err) => report(repl.cache_mut(), err),
                    };
                }
                Err(ReadlineError::Interrupted) => {
                    println!("Interrupted. Exiting");
                    break Ok(());
                }
                Err(ReadlineError::Eof) => {
                    println!("EOF");
                }
                Err(err) => {
                    report(
                        repl.cache_mut(),
                        Error::IOError(IOError(format!("{}", err))),
                    );
                }
            }
        }
    }

    fn help() {
        println!("Available commands: ? help query load typecheck");
    }
}
