//! Program handling, from file reading to evaluation.
//!
//! A program is Nickel source code loaded from an input. This module offers an interface to load a
//! program source, parse it, evaluate it and report errors.
//!
//! # Standard library
//!
//! Some essential functions required for evaluation, such as builtin contracts, are written in
//! pure Nickel. Standard library files must be record literals:
//!
//! ```text
//! {
//!     val1 = ...
//!     val2 = ...
//! }
//! ```
//!
//! These .ncl file are not actually distributed as files, instead they are embedded, as plain
//! text, in the Nickel executable. The embedding is done by way of the [stdlib
//! module](../stdlib/index.html), which exposes the standard library files as strings. The
//! embedded strings are then parsed by the functions in this module (see
//! [`mk_global_env`](./struct.Program.html#method.mk_global_env)).  Each such value is added to
//! the global environment before the evaluation of the program.
use crate::cache::*;
use crate::error::{Error, ParseError, ToDiagnostic};
use crate::identifier::Ident;
use crate::parser::lexer::Lexer;
use crate::term::{RichTerm, Term};
use crate::{eval, parser};
use codespan::FileId;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use std::ffi::OsString;
use std::io::{self, Read};
use std::result::Result;

/// A Nickel program.
///
/// Manage a file database, which stores the original source code of the program and eventually the
/// code of imported expressions, and a dictionary which stores corresponding parsed terms.
pub struct Program {
    /// The id of the program source in the file database.
    main_id: FileId,
    /// The cache holding the sources and parsed terms of the main source as well as imports.
    cache: Cache,
}

impl Program {
    /// Create a program by reading it from the standard input.
    pub fn new_from_stdin() -> std::io::Result<Program> {
        Program::new_from_source(io::stdin(), "<stdin>")
    }

    pub fn new_from_file(path: impl Into<OsString>) -> std::io::Result<Program> {
        let mut cache = Cache::new();
        let main_id = cache.add_file(path)?;

        Ok(Program { main_id, cache })
    }

    /// Create a program by reading it from a generic source.
    pub fn new_from_source<T, S>(source: T, source_name: S) -> std::io::Result<Program>
    where
        T: Read,
        S: Into<OsString> + Clone,
    {
        let mut cache = Cache::new();
        let main_id = cache.add_source(source_name, source)?;

        Ok(Program { main_id, cache })
    }

    /// Retrieve the parsed term and typecheck it, and generate a fresh global environment. Return
    /// both.
    fn prepare_eval(&mut self) -> Result<(RichTerm, eval::Environment), Error> {
        self.cache.prepare_stdlib()?;
        let global_env = self
            .cache
            .mk_global_env()
            .expect("program::prepare_eval(): expected event to be ready");
        Ok((
            self.cache.prepare_nocache(self.main_id, &global_env)?,
            global_env,
        ))
    }

    /// Parse if necessary, typecheck and then evaluate the program.
    pub fn eval(&mut self) -> Result<Term, Error> {
        let (t, global_env) = self.prepare_eval()?;
        eval::eval(t, &global_env, &mut self.cache).map_err(|e| e.into())
    }

    /// Same as `eval`, but proceeds to a full evaluation.
    pub fn eval_full(&mut self) -> Result<Term, Error> {
        let (t, global_env) = self.prepare_eval()?;
        eval::eval_full(t, &global_env, &mut self.cache).map_err(|e| e.into())
    }

    /// Wrapper for [`query`](./fn.query.html).
    pub fn query(&mut self, path: Option<String>) -> Result<Term, Error> {
        self.cache.prepare_stdlib()?;
        let global_env = self
            .cache
            .mk_global_env()
            .expect("program::prepare_eval(): expected event to be ready");
        query(&mut self.cache, self.main_id, &global_env, path)
    }

    /// Load, parse, and typecheck the program and the standard library, if not already done.
    pub fn typecheck(&mut self) -> Result<(), Error> {
        self.cache.parse(self.main_id)?;
        self.cache.load_stdlib()?;
        let global_env = self.cache.mk_global_env().expect("program::typecheck(): stdlib has been loaded but was not found in cache on mk_global_env()");
        self.cache
            .typecheck(self.main_id, &global_env)
            .map_err(|cache_err| {
                cache_err.unwrap_error("program::typecheck(): expected source to be parsed")
            })?;
        Ok(())
    }

    /// Wrapper for [`report`](./fn.report.html).
    pub fn report<E>(&mut self, error: E)
    where
        E: ToDiagnostic<FileId>,
    {
        report(&mut self.cache, error)
    }
}

/// Query the metadata of a path of a term in the cache.
///
/// The path is a list of dot separated identifiers. For example, querying `{a = {b  = ..}}` with
/// path `a.b` will return a "weak" (see below) evaluation of `b`.
///
/// "Weak" means that as opposed to normal evaluation, it does not try to unwrap the content of a
/// metavalue: the evaluation stops as soon as a metavalue is encountered, although the potential
/// term inside the meta-value is forced, so that the concrete value of the field may also be
/// reported when present.
//TODO: more robust implementation than `let x = (y.path) in %seq% x x`, with respect to e.g.
//error message in case of syntax error or missing file.
//TODO: also gather type information, such that `query a.b.c <<< '{ ... } : {a: {b: {c: Num}}}`
//would additionally report `type: Num` for example.
//TODO: not sure where this should go. It seems to embed too much logic to be in `Cache`, but is
//common to both `Program` and `REPL`. Leaving it here as a stand-alone function for now
pub fn query(
    cache: &mut Cache,
    file_id: FileId,
    global_env: &eval::Environment,
    path: Option<String>,
) -> Result<Term, Error> {
    cache.prepare(file_id, global_env)?;

    let t = if let Some(p) = path {
        // Parsing `y.path`. We `seq` it to force the evaluation of the underlying value,
        // which can be then showed to the user. The newline gives better messages in case of
        // errors.
        let source = format!("x.{}", p);
        let query_file_id = cache.add_tmp("<query>", source.clone());
        let new_term = parser::grammar::TermParser::new()
            .parse(query_file_id, Lexer::new(&source))
            .map_err(|err| ParseError::from_lalrpop(err, query_file_id))?;

        // Substituting `y` for `t`
        let mut env = eval::Environment::new();
        eval::env_add(
            &mut env,
            Ident::from("x"),
            cache.get_owned(file_id).unwrap(),
            eval::Environment::new(),
        );
        eval::subst(new_term, &eval::Environment::new(), &env)
    } else {
        cache.get_owned(file_id).unwrap()
    };

    Ok(eval::eval_meta(t, &global_env, cache)?)
}

/// Pretty-print an error.
///
/// This function is located here in `Program` because errors need a reference to `files` in
/// order to produce a diagnostic (see [`label_alt`](../error/fn.label_alt.html)).
//TODO: not sure where this should go. It seems to embed too much logic to be in `Cache`, but is
//common to both `Program` and `REPL`. Leaving it here as a stand-alone function for now
pub fn report<E>(cache: &mut Cache, error: E)
where
    E: ToDiagnostic<FileId>,
{
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();
    let contracts_id = cache.id_of("<stdlib/contracts.ncl>");
    let diagnostics = error.to_diagnostic(cache.files_mut(), contracts_id);

    let result = diagnostics.iter().try_for_each(|d| {
        codespan_reporting::term::emit(&mut writer.lock(), &config, cache.files_mut(), &d)
    });
    match result {
        Ok(()) => (),
        Err(err) => panic!(
            "Program::report: could not print an error on stderr: {}",
            err
        ),
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error::EvalError;
    use crate::parser::{grammar, lexer};
    use crate::position::TermPos;
    use codespan::Files;
    use std::io::Cursor;

    fn parse(s: &str) -> Option<RichTerm> {
        let id = Files::new().add("<test>", String::from(s));

        grammar::TermParser::new()
            .parse(id, lexer::Lexer::new(&s))
            .map(|mut t| {
                t.clean_pos();
                t
            })
            .map_err(|err| println!("{:?}", err))
            .ok()
    }

    fn eval_full(s: &str) -> Result<Term, Error> {
        let src = Cursor::new(s);

        let mut p = Program::new_from_source(src, "<test>").map_err(|io_err| {
            Error::EvalError(EvalError::Other(
                format!("IO error: {}", io_err),
                TermPos::None,
            ))
        })?;
        p.eval_full()
    }

    #[test]
    fn evaluation_full() {
        use crate::mk_record;
        use crate::term::make as mk_term;

        // Clean all the position information in a term.
        fn clean_pos(t: Term) -> Term {
            let mut tmp = RichTerm::new(t, TermPos::None);
            tmp.clean_pos();
            *tmp.term
        }

        let t = clean_pos(eval_full("[(1 + 1), (\"a\" ++ \"b\"), ([ 1, [1 + 2] ])]").unwrap());
        let mut expd = parse("[2, \"ab\", [1, [3]]]").unwrap();

        // String are parsed as StrChunks, but evaluated to Str, so we need to hack list a bit
        if let Term::List(ref mut data) = *expd.term {
            *data.get_mut(1).unwrap() = mk_term::string("ab");
        } else {
            panic!();
        }

        assert_eq!(t, *expd.term);

        let t = clean_pos(
            eval_full("let x = 1 in let y = 1 + x in let z = {foo.bar.baz = y} in z").unwrap(),
        );
        // Records are parsed as RecRecords, so we need to build one by hand
        let expd = mk_record!((
            "foo",
            mk_record!(("bar", mk_record!(("baz", Term::Num(2.0)))))
        ));
        assert_eq!(t, *expd.term);

        // /!\ [MAY OVERFLOW STACK]
        // Check that substitution do not replace bound variables. Before the fixing commit, this
        // example would go into an infinite loop, and stack overflow. If it does, this just means
        // that this test fails.
        eval_full("{y = fun x => x, x = fun y => y}").unwrap();
    }
}
