//! The Nickel REPL.
//!
//! A backend designates a module which actually executes a sequence of REPL commands, while being
//! agnostic to the user interface and the presentation of the results.
//!
//! Dually, the frontend is the user-facing part, which may be a CLI, a web application, a
//! jupyter-kernel (which is not exactly user-facing, but still manages input/output and
//! formatting), etc.
use crate::cache::{Cache, Envs, ErrorTolerance};
use crate::error::{Error, EvalError, IOError, ParseError, ParseErrors, ReplError};
use crate::eval::cache::Cache as EvalCache;
use crate::eval::VirtualMachine;
use crate::identifier::Ident;
use crate::parser::{grammar, lexer, ExtendedTerm};
use crate::term::{RichTerm, Term, Traverse};
use crate::transform::import_resolution;
use crate::types::Types;
use crate::{eval, transform, typecheck};
use codespan::FileId;
use simple_counter::*;
use std::ffi::{OsStr, OsString};
use std::io::Write;
use std::result::Result;
use std::str::FromStr;

#[cfg(feature = "repl")]
use rustyline::validate::{ValidationContext, ValidationResult};

generate_counter!(InputNameCounter, usize);

pub mod command;
pub mod query_print;
#[cfg(feature = "repl")]
pub mod rustyline_frontend;
#[cfg(feature = "repl-wasm")]
pub mod simple_frontend;
#[cfg(feature = "repl-wasm")]
pub mod wasm_frontend;

/// Result of the evaluation of an input.
pub enum EvalResult {
    /// The input has been evaluated to a term.
    Evaluated(RichTerm),
    /// The input was a toplevel let, which has been bound in the environment.
    Bound(Ident),
}

impl From<RichTerm> for EvalResult {
    fn from(t: RichTerm) -> Self {
        EvalResult::Evaluated(t)
    }
}

/// Interface of the REPL backend.
pub trait Repl {
    /// Evaluate an expression, which can be either a standard term or a toplevel let-binding.
    fn eval(&mut self, exp: &str) -> Result<EvalResult, Error>;
    /// Fully evaluate an expression, which can be either a standard term or a toplevel let-binding.
    fn eval_full(&mut self, exp: &str) -> Result<EvalResult, Error>;
    /// Load the content of a file in the environment. Return the loaded record.
    fn load(&mut self, path: impl AsRef<OsStr>) -> Result<RichTerm, Error>;
    /// Typecheck an expression and return its [apparent type][crate::typecheck::ApparentType].
    fn typecheck(&mut self, exp: &str) -> Result<Types, Error>;
    /// Query the metadata of an expression.
    fn query(&mut self, exp: &str) -> Result<Term, Error>;
    /// Required for error reporting on the frontend.
    fn cache_mut(&mut self) -> &mut Cache;
}

/// Standard implementation of the REPL backend.
pub struct ReplImpl<EC: EvalCache> {
    /// The parser, supporting toplevel let declaration.
    parser: grammar::ExtendedTermParser,
    /// The current environment (for evaluation and typing). Contain the initial environment with
    /// the stdlib, plus toplevel declarations and loadings made inside the REPL.
    env: Envs,
    /// The initial typing context, without the toplevel declarations made inside the REPL. Used to
    /// typecheck imports in a fresh environment.
    initial_type_ctxt: typecheck::Context,
    /// The state of the Nickel virtual machine, holding a cache of loaded files and parsed terms.
    vm: VirtualMachine<Cache, EC>,
}

impl<EC: EvalCache> ReplImpl<EC> {
    /// Create a new empty REPL.
    pub fn new() -> Self {
        ReplImpl {
            parser: grammar::ExtendedTermParser::new(),
            env: Envs::new(),
            initial_type_ctxt: typecheck::Context::new(),
            vm: VirtualMachine::new(Cache::new(ErrorTolerance::Strict)),
        }
    }

    /// Load and process the stdlib, and use it to populate the eval environment as well as the
    /// typing environment.
    pub fn load_stdlib(&mut self) -> Result<(), Error> {
        self.env = self.vm.prepare_stdlib()?;
        self.initial_type_ctxt = self.env.type_ctxt.clone();
        Ok(())
    }

    fn eval_(&mut self, exp: &str, eval_full: bool) -> Result<EvalResult, Error> {
        self.vm.reset();
        let eval_function = if eval_full {
            eval::VirtualMachine::eval_full
        } else {
            eval::VirtualMachine::eval
        };

        let file_id = self.vm.import_resolver_mut().add_string(
            format!("repl-input-{}", InputNameCounter::next()),
            String::from(exp),
        );

        let (term, parse_errs) = self
            .parser
            .parse_term_tolerant(file_id, lexer::Lexer::new(exp))?;

        if !parse_errs.no_errors() {
            return Err(parse_errs.into());
        }

        // Because we don't use the cache for input, we have to perform recursive import
        // resolution/typechecking/transformation by ourselves.
        //
        // `id` must be set to `None` for normal lets and to `Some(id_)` for top-level lets. In the
        // latter case, we need to update the current type environment before doing program
        // transformations in the case of a top-level let.
        fn prepare<EC: EvalCache>(
            repl_impl: &mut ReplImpl<EC>,
            id: Option<Ident>,
            t: RichTerm,
        ) -> Result<RichTerm, Error> {
            let (t, pending) =
                import_resolution::resolve_imports(t, repl_impl.vm.import_resolver_mut())?;
            for id in &pending {
                repl_impl
                    .vm
                    .import_resolver_mut()
                    .resolve_imports(*id)
                    .unwrap();
            }

            let wildcards = typecheck::type_check(
                &t,
                repl_impl.env.type_ctxt.clone(),
                repl_impl.vm.import_resolver(),
            )?;

            if let Some(id) = id {
                typecheck::env_add(
                    &mut repl_impl.env.type_ctxt.type_env,
                    id,
                    &t,
                    &repl_impl.env.type_ctxt.term_env,
                    repl_impl.vm.import_resolver(),
                );
                repl_impl
                    .env
                    .type_ctxt
                    .term_env
                    .0
                    .insert(id, (t.clone(), repl_impl.env.type_ctxt.term_env.clone()));
            }

            for id in &pending {
                repl_impl
                    .vm
                    .import_resolver_mut()
                    .typecheck(*id, &repl_impl.initial_type_ctxt)
                    .map_err(|cache_err| {
                        cache_err.unwrap_error("repl::eval_(): expected imports to be parsed")
                    })?;
            }

            let t = transform::transform(t, Some(&wildcards))
                .map_err(|err| Error::ParseErrors(err.into()))?;
            for id in &pending {
                repl_impl
                    .vm
                    .import_resolver_mut()
                    .transform(*id)
                    .unwrap_or_else(|_| panic!("repl::eval_(): expected imports to be parsed"));
            }

            Ok(t)
        }

        match term {
            ExtendedTerm::RichTerm(t) => {
                let t = prepare(self, None, t)?;
                Ok(eval_function(&mut self.vm, t, &self.env.eval_env)?.into())
            }
            ExtendedTerm::ToplevelLet(id, t) => {
                let t = prepare(self, Some(id), t)?;
                let local_env = self.env.eval_env.clone();
                eval::env_add(&mut self.vm.cache, &mut self.env.eval_env, id, t, local_env);
                Ok(EvalResult::Bound(id))
            }
        }
    }
}

impl<EC: EvalCache> Repl for ReplImpl<EC> {
    fn eval(&mut self, exp: &str) -> Result<EvalResult, Error> {
        self.eval_(exp, false)
    }

    fn eval_full(&mut self, exp: &str) -> Result<EvalResult, Error> {
        self.eval_(exp, true)
    }

    fn load(&mut self, path: impl AsRef<OsStr>) -> Result<RichTerm, Error> {
        let file_id = self
            .vm
            .import_resolver_mut()
            .add_file(OsString::from(path.as_ref()))
            .map_err(IOError::from)?;
        self.vm.import_resolver_mut().parse(file_id)?;
        let RichTerm { term, pos } = self.vm.import_resolver().get_ref(file_id).unwrap();

        // Check that the entry is a record, which is a precondition of transform_inner
        match term.as_ref() {
            Term::Record(..) | Term::RecRecord(..) => (),
            _ => {
                return Err(Error::EvalError(EvalError::Other(
                    String::from("load: expected a record"),
                    *pos,
                )))
            }
        };
        self.vm
            .import_resolver_mut()
            .transform_inner(file_id)
            .map_err(|err| {
                err.unwrap_error("load(): expected term to be parsed before transformation")
            })?;

        let term = self.vm.import_resolver().get_owned(file_id).unwrap();
        let (term, pending) =
            import_resolution::resolve_imports(term, self.vm.import_resolver_mut())?;
        for id in &pending {
            self.vm.import_resolver_mut().resolve_imports(*id).unwrap();
        }
        typecheck::env_add_term(
            &mut self.env.type_ctxt.type_env,
            &term,
            &self.env.type_ctxt.term_env,
            self.vm.import_resolver(),
        )
        .unwrap();
        eval::env_add_term(&mut self.vm.cache, &mut self.env.eval_env, term.clone()).unwrap();

        Ok(term)
    }

    fn typecheck(&mut self, exp: &str) -> Result<Types, Error> {
        let file_id = self
            .vm
            .import_resolver_mut()
            .add_tmp("<repl-typecheck>", String::from(exp));
        // We ignore non fatal errors while type checking.
        let (term, _) = self.vm.import_resolver().parse_nocache(file_id)?;
        let (term, pending) =
            import_resolution::resolve_imports(term, self.vm.import_resolver_mut())?;
        for id in &pending {
            self.vm.import_resolver_mut().resolve_imports(*id).unwrap();
        }
        let wildcards =
            typecheck::type_check(&term, self.env.type_ctxt.clone(), self.vm.import_resolver())?;
        // Substitute the wildcard types for their inferred types
        // We need to `traverse` the term, in case the type depends on inner terms that also contain wildcards
        let term = term
            .traverse(
                &|rt: RichTerm, _| -> Result<RichTerm, std::convert::Infallible> {
                    Ok(transform::substitute_wildcards::transform_one(
                        rt, &wildcards,
                    ))
                },
                &mut (),
                crate::term::TraverseOrder::TopDown,
            )
            .unwrap();

        Ok(typecheck::apparent_type(
            term.as_ref(),
            Some(&self.env.type_ctxt.type_env),
            Some(self.vm.import_resolver()),
        )
        .into())
    }

    fn query(&mut self, exp: &str) -> Result<Term, Error> {
        use crate::program;

        let file_id = self
            .vm
            .import_resolver_mut()
            .add_tmp("<repl-query>", String::from(exp));
        program::query(&mut self.vm, file_id, &self.env, None)
    }

    fn cache_mut(&mut self) -> &mut Cache {
        self.vm.import_resolver_mut()
    }
}

/// Error occurring when initializing the REPL.
pub enum InitError {
    /// Unable to load, parse or typecheck the stdlib
    Stdlib,
}

pub enum InputStatus {
    Complete(ExtendedTerm),
    Partial,
    Command,
    Failed(ParseErrors),
}

/// Validator enabling multiline input.
///
/// The behavior is the following:
/// - always end an input that starts with the command prefix `:`
/// - otherwise, try to parse the input. If an unexpected end of file error occurs, continue
///   the input in a new line. Otherwise, accept and end the input.
//TODO: the validator throws away the result of parsing, or the parse error, when accepting an
//input, meaning that the work is done a second time by the REPL. Validator's work could be
//reused. This overhead shouldn't be dramatic for the typical REPL input size, though.
#[cfg_attr(
    feature = "repl",
    derive(
        rustyline_derive::Completer,
        rustyline_derive::Helper,
        rustyline_derive::Highlighter,
        rustyline_derive::Hinter
    )
)]
pub struct InputParser {
    parser: grammar::ExtendedTermParser,
    /// Currently the parser expect a `FileId` to fill in location information. For this
    /// validator, this may be a dummy one, since for now location information is not used.
    file_id: FileId,
}

impl InputParser {
    pub fn new(file_id: FileId) -> Self {
        InputParser {
            parser: grammar::ExtendedTermParser::new(),
            file_id,
        }
    }

    pub fn parse(&self, input: &str) -> InputStatus {
        if input.starts_with(':') || input.trim().is_empty() {
            return InputStatus::Command;
        }

        let result = self
            .parser
            .parse_term_tolerant(self.file_id, lexer::Lexer::new(input));

        let partial = |pe| {
            matches!(
                pe,
                &ParseError::UnexpectedEOF(..) | &ParseError::UnmatchedCloseBrace(..)
            )
        };

        match result {
            Ok((t, e)) if e.no_errors() => InputStatus::Complete(t),
            Ok((_, e)) if e.errors.iter().all(partial) => InputStatus::Partial,
            Ok((_, e)) => InputStatus::Failed(e),
            Err(e) if partial(&e) => InputStatus::Partial,
            Err(err) => InputStatus::Failed(err.into()),
        }
    }
}

#[cfg(feature = "repl")]
impl rustyline::validate::Validator for InputParser {
    fn validate(&self, ctx: &mut ValidationContext<'_>) -> rustyline::Result<ValidationResult> {
        match self.parse(ctx.input()) {
            InputStatus::Partial => Ok(ValidationResult::Invalid(None)),
            _ => Ok(ValidationResult::Valid(None)),
        }
    }
}

/// Print the help message corresponding to a command, or show a list of available commands if
/// the argument is `None` or is not a command.
#[cfg(any(feature = "repl", feature = "repl-wasm"))]
pub fn print_help(out: &mut impl Write, arg: Option<&str>) -> std::io::Result<()> {
    use command::*;

    if let Some(arg) = arg {
        fn print_aliases(w: &mut impl Write, cmd: CommandType) -> std::io::Result<()> {
            let mut aliases = cmd.aliases().into_iter();

            if let Some(fst) = aliases.next() {
                write!(w, "Aliases: `{}`", fst)?;
                aliases.try_for_each(|alias| write!(w, ", `{}`", alias))?;
                writeln!(w)?;
            }

            writeln!(w)
        }

        match arg.parse::<CommandType>() {
            Ok(c @ CommandType::Help) => {
                writeln!(out, ":{} [command]", c)?;
                print_aliases(out, c)?;
                writeln!(
                    out,
                    "Prints a list of available commands or the help of the given command"
                )?;
            }
            Ok(c @ CommandType::Query) => {
                writeln!(out, ":{} <expression>", c)?;
                print_aliases(out, c)?;
                writeln!(out, "Print the metadata attached to an attribute")?;
            }
            Ok(c @ CommandType::Load) => {
                writeln!(out, ":{} <file>", c)?;
                print_aliases(out, c)?;
                write!(out,"Evaluate the content of <file> to a record and load its attributes in the environment.")?;
                writeln!(
                    out,
                    " Fail if the content of <file> doesn't evaluate to a record"
                )?;
            }
            Ok(c @ CommandType::Typecheck) => {
                writeln!(out, ":{} <expression>", c)?;
                print_aliases(out, c)?;
                writeln!(
                    out,
                    "Typecheck the given expression and print its top-level type"
                )?;
            }
            Ok(c @ CommandType::Print) => {
                writeln!(out, ":{} <expression>", c)?;
                print_aliases(out, c)?;
                writeln!(out, "Evaluate and print <expression> recursively")?;
            }
            Ok(c @ CommandType::Exit) => {
                writeln!(out, ":{}", c)?;
                print_aliases(out, c)?;
                writeln!(out, "Exit the REPL session")?;
            }
            Err(UnknownCommandError {}) => {
                writeln!(out, "Unknown command `{}`.", arg)?;
                writeln!(out, "Available commands: ? help query load typecheck")?;
            }
        };

        Ok(())
    } else {
        writeln!(out, "Available commands: help query load typecheck exit")
    }
}
