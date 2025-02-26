//! The Nickel REPL.
//!
//! A backend designates a module which actually executes a sequence of REPL commands, while being
//! agnostic to the user interface and the presentation of the results.
//!
//! Dually, the frontend is the user-facing part, which may be a CLI, a web application, a
//! jupyter-kernel (which is not exactly user-facing, but still manages input/output and
//! formatting), etc.
use crate::{
    bytecode::ast::AstAlloc,
    cache::{CacheHub, InputFormat, NotARecord, SourcePath},
    error::{Error, EvalError, IOError, NullReporter, ParseError, ParseErrors, ReplError},
    eval::{self, cache::Cache as EvalCache, Closure, VirtualMachine},
    files::FileId,
    identifier::LocIdent,
    parser::{grammar, lexer, ErrorTolerantParser},
    program::FieldPath,
    term::{record::Field, RichTerm},
    typ::Type,
    typecheck::TypecheckMode,
};

use simple_counter::*;

use std::{
    ffi::{OsStr, OsString},
    io::Write,
    result::Result,
    str::FromStr,
};

#[cfg(feature = "repl")]
use crate::{
    error::{
        report::{self, ColorOpt, ErrorFormat},
        IntoDiagnostics,
    },
    term::Term,
};
#[cfg(feature = "repl")]
use ansi_term::{Colour, Style};
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
#[derive(Debug, Clone)]
pub enum EvalResult {
    /// The input has been evaluated to a term.
    Evaluated(RichTerm),
    /// The input was a toplevel let, which has been bound in the environment.
    Bound(LocIdent),
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
    fn typecheck(&mut self, exp: &str) -> Result<Type, Error>;
    /// Query the metadata of an expression.
    fn query(&mut self, path: String) -> Result<Field, Error>;
    /// Required for error reporting on the frontend.
    fn cache_mut(&mut self) -> &mut CacheHub;
}

/// Standard implementation of the REPL backend.
pub struct ReplImpl<EC: EvalCache> {
    /// The current eval environment, including the stdlib and top-level lets.
    eval_env: eval::Environment,
    /// The state of the Nickel virtual machine, holding a cache of loaded files and parsed terms.
    vm: VirtualMachine<CacheHub, EC>,
}

impl<EC: EvalCache> ReplImpl<EC> {
    /// Create a new empty REPL.
    pub fn new(trace: impl Write + 'static) -> Self {
        ReplImpl {
            eval_env: eval::Environment::new(),
            vm: VirtualMachine::new(CacheHub::new(), trace, NullReporter {}),
        }
    }

    /// Load and process the stdlib, and use it to populate the eval environment as well as the
    /// typing environment.
    pub fn load_stdlib(&mut self) -> Result<(), Error> {
        self.vm.prepare_stdlib()?;
        self.eval_env = self.vm.mk_eval_env();
        Ok(())
    }

    fn eval_(&mut self, exp: &str, eval_full: bool) -> Result<EvalResult, Error> {
        self.vm.reset();

        let eval_function = if eval_full {
            eval::VirtualMachine::eval_full_closure
        } else {
            eval::VirtualMachine::eval_closure
        };

        let file_id = self.vm.import_resolver_mut().sources.add_string(
            SourcePath::ReplInput(InputNameCounter::next()),
            String::from(exp),
        );

        let id = self.vm.import_resolver_mut().prepare_repl(file_id)?.inner();
        // unwrap(): we've just prepared the term successfully, so it must be in cache
        let term = self.vm.import_resolver().terms.get_owned(file_id).unwrap();

        if let Some(id) = id {
            let current_env = self.eval_env.clone();
            eval::env_add(
                &mut self.vm.cache,
                &mut self.eval_env,
                id,
                term,
                current_env,
            );
            Ok(EvalResult::Bound(id))
        } else {
            Ok(eval_function(
                &mut self.vm,
                Closure {
                    body: term,
                    env: self.eval_env.clone(),
                },
            )?
            .body
            .into())
        }
    }

    #[cfg(feature = "repl")]
    fn report(&mut self, err: impl IntoDiagnostics, color_opt: ColorOpt) {
        let mut files = self.cache_mut().sources.files().clone();
        report::report(&mut files, err, ErrorFormat::Text, color_opt);
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
            .sources
            .add_file(OsString::from(path.as_ref()), InputFormat::Nickel)
            .map_err(IOError::from)?;

        self.vm.import_resolver_mut().prepare_repl(file_id)?;
        // self.vm
        //     .import_resolver_mut()
        //     .parse(file_id, InputFormat::Nickel)?;

        let term = self.vm.import_resolver().terms.get_owned(file_id).unwrap();
        let pos = term.pos;

        let Closure {
            body: term,
            env: new_env,
        } = self.vm.eval_closure(Closure {
            body: term,
            env: self.eval_env.clone(),
        })?;

        self.vm
            .import_resolver_mut()
            .add_repl_bindings(&term)
            .map_err(|NotARecord| {
                Error::EvalError(EvalError::Other(
                    String::from("load: expected a record"),
                    pos,
                ))
            })?;

        eval::env_add_record(
            &mut self.vm.cache,
            &mut self.eval_env,
            Closure {
                body: term.clone(),
                env: new_env,
            },
        )
        // unwrap(): if the call above succeeded, the term must be a record
        .unwrap();

        Ok(term)
    }

    fn typecheck(&mut self, exp: &str) -> Result<Type, Error> {
        let cache = self.vm.import_resolver_mut();

        let file_id = cache.replace_string(SourcePath::ReplTypecheck, String::from(exp));
        let _ = cache.parse(file_id, InputFormat::Nickel)?;

        cache
            .typecheck(file_id, TypecheckMode::Walk)
            .map_err(|cache_err| {
                cache_err.unwrap_error(
                    "repl::typecheck(): expected source to be parsed before typechecking",
                )
            })?;

        Ok(cache
            .type_of(file_id)
            .map_err(|cache_err| {
                cache_err.unwrap_error(
                    "repl::typecheck(): expected source to be parsed before retrieving the type",
                )
            })?
            .inner())
    }

    fn query(&mut self, path: String) -> Result<Field, Error> {
        self.vm.reset();

        let mut query_path = FieldPath::parse(self.vm.import_resolver_mut(), path)?;

        // remove(): this is safe because there is no such thing as an empty field path, at least
        // when it comes out of the parser. If `path` is empty, the parser will error out. Hence,
        // `FieldPath::parse` always returns a non-empty vector.
        let target = query_path.0.remove(0);

        let file_id = self
            .vm
            .import_resolver_mut()
            .replace_string(SourcePath::ReplQuery, target.label().into());

        self.vm.import_resolver_mut().prepare_repl(file_id)?;

        Ok(self.vm.query_closure(
            Closure {
                body: self.vm.import_resolver().terms.get_owned(file_id).unwrap(),
                env: self.eval_env.clone(),
            },
            &query_path,
        )?)
    }

    fn cache_mut(&mut self) -> &mut CacheHub {
        self.vm.import_resolver_mut()
    }
}

/// Error occurring when initializing the REPL.
pub enum InitError {
    /// Unable to load, parse or typecheck the stdlib
    Stdlib,
    /// An I/O or Linux Syscall (Errno) error happened while initializing the interactive terminal.
    ReadlineError(String),
}

pub enum InputStatus {
    Complete,
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
        rustyline_derive::Hinter
    )
)]
pub struct InputParser {
    parser: grammar::ExtendedTermParser,
    /// Currently the parser expect a `FileId` to fill in location information. For this
    /// validator, this may be a dummy one, since for now location information is not used.
    file_id: FileId,
    /// The allocator used to allocate AST nodes.
    ///
    /// Note that we just grow this allocator without ever freeing it. This should be fine for a
    /// REPL. If that is a problem, it's not very hard to periodically clear it.
    alloc: AstAlloc,
}

impl InputParser {
    pub fn new(file_id: FileId) -> Self {
        InputParser {
            parser: grammar::ExtendedTermParser::new(),
            file_id,
            alloc: AstAlloc::new(),
        }
    }

    pub fn parse(&self, input: &str) -> InputStatus {
        if input.starts_with(':') || input.trim().is_empty() {
            return InputStatus::Command;
        }

        let result =
            self.parser
                .parse_tolerant(&self.alloc, self.file_id, lexer::Lexer::new(input));

        let partial = |pe| {
            matches!(
                pe,
                &ParseError::UnexpectedEOF(..) | &ParseError::UnmatchedCloseBrace(..)
            )
        };

        match result {
            Ok((_, e)) if e.no_errors() => InputStatus::Complete,
            Ok((_, e)) if e.errors.iter().all(partial) => InputStatus::Partial,
            Ok((_, e)) => InputStatus::Failed(e),
            Err(e) if partial(&e) => InputStatus::Partial,
            Err(err) => InputStatus::Failed(err.into()),
        }
    }
}

#[cfg(feature = "repl")]
impl rustyline::highlight::Highlighter for InputParser {
    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &'s self,
        prompt: &'p str,
        _default: bool,
    ) -> std::borrow::Cow<'b, str> {
        let style = Style::new().fg(Colour::Green);
        std::borrow::Cow::Owned(style.paint(prompt).to_string())
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
                write!(w, "Aliases: `{fst}`")?;
                aliases.try_for_each(|alias| write!(w, ", `{alias}`"))?;
                writeln!(w)?;
            }

            writeln!(w)
        }

        match arg.parse::<CommandType>() {
            Ok(c @ CommandType::Help) => {
                writeln!(out, ":{c} [command]")?;
                print_aliases(out, c)?;
                writeln!(
                    out,
                    "Prints a list of available commands or the help of the given command"
                )?;
            }
            Ok(c @ CommandType::Query) => {
                writeln!(out, ":{c} <field path>")?;
                print_aliases(out, c)?;
                writeln!(out, "Print the metadata attached to a field")?;
                writeln!(
                    out,
                    "<field path> is a dot-separated sequence of identifiers pointing to a field. \
                    Fields can be quoted if they contain special characters, \
                    just like in normal Nickel source code.\n"
                )?;
                writeln!(out, "Examples:")?;
                writeln!(out, "- `:{c} std.array.any`")?;
                writeln!(out, "- `:{c} mylib.contracts.\"special#chars.\".bar`")?;
            }
            Ok(c @ CommandType::Load) => {
                writeln!(out, ":{c} <file>")?;
                print_aliases(out, c)?;
                writeln!(
                    out,
                    "Evaluate the content of <file> to a record \
                    and add its fields to the environment."
                )?;
                writeln!(
                    out,
                    "Fail if the content of <file> doesn't evaluate to a record."
                )?;
            }
            Ok(c @ CommandType::Typecheck) => {
                writeln!(out, ":{c} <expression>")?;
                print_aliases(out, c)?;
                writeln!(
                    out,
                    "Typecheck the given expression and print its top-level type"
                )?;
            }
            Ok(c @ CommandType::Print) => {
                writeln!(out, ":{c} <expression>")?;
                print_aliases(out, c)?;
                writeln!(out, "Evaluate and print <expression> recursively")?;
            }
            Ok(c @ CommandType::Exit) => {
                writeln!(out, ":{c}")?;
                print_aliases(out, c)?;
                writeln!(out, "Exit the REPL session")?;
            }
            Err(UnknownCommandError {}) => {
                writeln!(out, "Unknown command `{arg}`.")?;
                writeln!(
                    out,
                    "Available commands: ? {}",
                    CommandType::all().join(" ")
                )?;
            }
        };

        Ok(())
    } else {
        writeln!(out, "Available commands: help query load typecheck exit")
    }
}
