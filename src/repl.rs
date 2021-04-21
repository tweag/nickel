//! The Nickel REPL.
//!
//! A backend designates a module which actually executes a sequence of REPL commands, while being
//! agnostic to the user interface and the presentation of the results.
//!
//! Dually, the frontend is the user-facing part, which may be a CLI, a web application, a
//! jupyter-kernel (which is not exactly user-facing, but still manages input/output and
//! formatting), etc.
use crate::cache::Cache;
use crate::error::{Error, EvalError, IOError, ParseError, REPLError};
use crate::eval::Environment;
use crate::identifier::Ident;
use crate::parser::{grammar, lexer, ExtendedTerm};
use crate::term::{RichTerm, Term};
use crate::types::Types;
use crate::{eval, transformations, typecheck};
use codespan::FileId;
use simple_counter::*;
use std::ffi::{OsStr, OsString};
use std::result::Result;
use std::str::FromStr;
use std::{io, io::Write};

#[cfg(feature = "repl")]
use rustyline::validate::{ValidationContext, ValidationResult};

generate_counter!(InputNameCounter, usize);

/// Result of the evaluation of an input.
pub enum EvalResult {
    /// The input has been evaluated to a term.
    Evaluated(Term),
    /// The input was a toplevel let, which has been bound in the environment.
    Bound(Ident),
}

impl From<Term> for EvalResult {
    fn from(t: Term) -> Self {
        EvalResult::Evaluated(t)
    }
}

/// Interface of the REPL backend.
pub trait REPL {
    /// Evaluate an expression, which can be either a standard term or a toplevel let-binding.
    fn eval(&mut self, exp: &str) -> Result<EvalResult, Error>;
    /// Evaluate an expression fully, which can be either a standard term or a toplevel let-binding.
    fn eval_full(&mut self, exp: &str) -> Result<EvalResult, Error>;
    /// Load the content of a file in the environment. Return the loaded record.
    fn load(&mut self, path: impl AsRef<OsStr>) -> Result<RichTerm, Error>;
    /// Typecheck an expression and return its [apparent type](../typecheck/fn.apparent_type.html).
    fn typecheck(&mut self, exp: &str) -> Result<Types, Error>;
    /// Query the metadata of an expression.
    fn query(&mut self, exp: &str) -> Result<Term, Error>;
    /// Required for error reporting on the frontend.
    fn cache_mut(&mut self) -> &mut Cache;
}

/// Standard implementation of the REPL backend.
pub struct REPLImpl {
    /// The underlying cache, storing input, loaded files and parsed terms.
    cache: Cache,
    /// The parser, supporting toplevel let declaration.
    parser: grammar::ExtendedTermParser,
    /// The eval environment. Contain the global environment with the stdlib, plus toplevel
    /// declarations and loadings made inside the REPL.
    eval_env: eval::Environment,
    /// The typing environment, counterpart of the eval environment for typechecking. Entries are
    /// [`TypeWrapper`](../typecheck/enum.TypeWrapper.html) for the ease of interacting with the
    /// typechecker, but there are not any unification variable in it.
    type_env: typecheck::Environment,
}

impl REPLImpl {
    /// Create a new empty REPL.
    pub fn new() -> Self {
        REPLImpl {
            cache: Cache::new(),
            parser: grammar::ExtendedTermParser::new(),
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

fn generic_eval<F>(repl: &mut REPLImpl, eval_f: F, exp: &str) -> Result<EvalResult, Error>
where
    F: Fn(RichTerm, &Environment, &mut Cache) -> Result<Term, EvalError>,
{
    let file_id = repl.cache.add_string(
        format!("repl-input-{}", InputNameCounter::next()),
        String::from(exp),
    );

    match repl
        .parser
        .parse(file_id, lexer::Lexer::new(exp))
        .map_err(|err| ParseError::from_lalrpop(err, file_id))?
    {
        ExtendedTerm::RichTerm(t) => {
            typecheck::type_check_in_env(&t, &repl.type_env, &repl.cache)?;
            let t = transformations::transform(t, &mut repl.cache)?;
            Ok(eval_f(t, &repl.eval_env, &mut repl.cache)?.into())
        }
        ExtendedTerm::ToplevelLet(id, t) => {
            typecheck::type_check_in_env(&t, &repl.type_env, &repl.cache)?;
            typecheck::Envs::env_add(&mut repl.type_env, id.clone(), &t);

            let t = transformations::transform(t, &mut repl.cache)?;

            let local_env = repl.eval_env.clone();
            eval::env_add(&mut repl.eval_env, id.clone(), t, local_env);
            Ok(EvalResult::Bound(id))
        }
    }
}

impl REPL for REPLImpl {
    fn eval(&mut self, exp: &str) -> Result<EvalResult, Error> {
        generic_eval(self, eval::eval, exp)
    }

    fn eval_full(&mut self, exp: &str) -> Result<EvalResult, Error> {
        generic_eval(self, eval::eval_full, exp)
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
                    *pos,
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
        let term = self.cache.parse_nocache(file_id)?;
        typecheck::type_check_in_env(&term, &self.type_env, &self.cache)?;

        Ok(typecheck::apparent_type(
            term.as_ref(),
            Some(&typecheck::Envs::from_global(&self.type_env)),
        )
        .into())
    }

    fn query(&mut self, exp: &str) -> Result<Term, Error> {
        use crate::program;

        let file_id = self.cache.add_tmp("<repl-query>", String::from(exp));
        program::query(&mut self.cache, file_id, &self.eval_env, None)
    }

    fn cache_mut(&mut self) -> &mut Cache {
        &mut self.cache
    }
}

/// REPL commands helpers common to all frontends.
pub mod command {
    use super::*;
    use std::fmt;

    /// Available commands.
    #[derive(Copy, Clone, Eq, PartialEq, Debug)]
    pub enum CommandType {
        Load,
        Typecheck,
        Query,
        Print,
        Help,
        Exit,
    }

    /// A parsed command with corresponding argument(s). Required argument are checked for
    /// non-emptiness.
    #[derive(Clone, Eq, PartialEq, Debug)]
    pub enum Command {
        Load(OsString),
        Typecheck(String),
        Query(String),
        Print(String),
        Help(Option<String>),
        Exit,
    }

    pub struct UnknownCommandError {}

    /// Check that an argument is non-empty, or return an error with the given optional message.
    fn require_arg(cmd: CommandType, arg: &str, msg_opt: Option<&str>) -> Result<(), REPLError> {
        if arg.trim().is_empty() {
            Err(REPLError::MissingArg {
                cmd,
                msg_opt: msg_opt.map(String::from),
            })
        } else {
            Ok(())
        }
    }

    impl FromStr for CommandType {
        type Err = UnknownCommandError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            use CommandType::*;

            match s {
                "load" | "l" => Ok(Load),
                "typecheck" | "tc" => Ok(Typecheck),
                "query" | "q" => Ok(Query),
                "print" | "p" => Ok(Print),
                "help" | "?" | "h" => Ok(Help),
                "exit" | "e" => Ok(Exit),
                _ => Err(UnknownCommandError {}),
            }
        }
    }

    impl CommandType {
        /// Return the aliases of a command.
        pub fn aliases(&self) -> Vec<String> {
            use CommandType::*;

            match self {
                Load => vec![String::from("l")],
                Typecheck => vec![String::from("tc")],
                Query => vec![String::from("q")],
                Print => vec![String::from("p")],
                Help => vec![String::from("h"), String::from("?")],
                Exit => vec![String::from("e")],
            }
        }
    }

    impl std::fmt::Display for CommandType {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            use CommandType::*;

            match self {
                Load => write!(f, "load"),
                Typecheck => write!(f, "typecheck"),
                Query => write!(f, "query"),
                Print => write!(f, "print"),
                Help => write!(f, "help"),
                Exit => write!(f, "exit"),
            }
        }
    }

    impl FromStr for Command {
        type Err = REPLError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let cmd_end = s.find(' ').unwrap_or_else(|| s.len());
            let cmd_str: String = s.chars().take(cmd_end).collect();
            let cmd: CommandType = cmd_str
                .parse()
                .map_err(|_| REPLError::UnknownCommand(cmd_str.clone()))?;
            let arg: String = s.chars().skip(cmd_end + 1).collect();

            match cmd {
                CommandType::Load => {
                    require_arg(cmd, &arg, Some("Please provide a file to load"))?;
                    Ok(Command::Load(OsString::from(arg)))
                }
                CommandType::Typecheck => {
                    require_arg(cmd, &arg, None)?;
                    Ok(Command::Typecheck(arg))
                }
                CommandType::Query => {
                    require_arg(cmd, &arg, None)?;
                    Ok(Command::Query(arg))
                }
                CommandType::Print => {
                    require_arg(cmd, &arg, None)?;
                    Ok(Command::Print(arg))
                }
                CommandType::Exit => Ok(Command::Exit),
                CommandType::Help => {
                    let arg_opt = if arg.trim().is_empty() {
                        None
                    } else {
                        Some(String::from(arg.trim()))
                    };

                    Ok(Command::Help(arg_opt))
                }
            }
        }
    }

    impl Command {
        pub fn typ(&self) -> CommandType {
            use Command::*;

            match self {
                Load(..) => CommandType::Load,
                Typecheck(..) => CommandType::Typecheck,
                Query(..) => CommandType::Query,
                Print(..) => CommandType::Print,
                Help(..) => CommandType::Help,
                Exit => CommandType::Exit,
            }
        }
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
    Failed(ParseError),
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
            .parse(self.file_id, lexer::Lexer::new(input))
            .map_err(|err| ParseError::from_lalrpop(err, self.file_id));

        match result {
            Ok(t) => InputStatus::Complete(t),
            Err(ParseError::UnexpectedEOF(..)) | Err(ParseError::UnmatchedCloseBrace(..)) => {
                InputStatus::Partial
            }
            Err(err) => InputStatus::Failed(err),
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

/// Native terminal implementation of an REPL frontend using rustyline.
#[cfg(feature = "repl")]
pub mod rustyline_frontend {
    use super::command::Command;
    use super::*;

    use crate::program;
    use ansi_term::{Colour, Style};
    use rustyline::config::OutputStreamType;
    use rustyline::error::ReadlineError;
    use rustyline::{Config, EditMode, Editor};

    /// The config of rustyline's editor.
    pub fn config() -> Config {
        Config::builder()
            .history_ignore_space(true)
            .edit_mode(EditMode::Emacs)
            .output_stream(OutputStreamType::Stdout)
            .build()
    }

    /// Main loop of the REPL.
    pub fn repl() -> Result<(), InitError> {
        let mut repl = REPLImpl::new();

        match repl.load_stdlib() {
            Ok(()) => (),
            Err(err) => {
                program::report(repl.cache_mut(), err);
                return Err(InitError::Stdlib);
            }
        }

        let validator = InputParser::new(repl.cache_mut().add_tmp("<repl-input>", String::new()));

        let mut editor = Editor::with_config(config());
        editor.set_helper(Some(validator));
        let prompt = Style::new().fg(Colour::Green).paint("nickel> ").to_string();

        loop {
            let line = editor.readline(&prompt);

            if let Ok(line) = line.as_ref() {
                editor.add_history_entry(line.clone());
            }

            let mut stdout = std::io::stdout();

            match line {
                Ok(line) if line.trim().is_empty() => (),
                Ok(line) if line.starts_with(':') => {
                    let cmd = line.chars().skip(1).collect::<String>().parse::<Command>();
                    let result = match cmd {
                        Ok(Command::Load(path)) => {
                            repl.load(&path).map(|term| match term.as_ref() {
                                Term::Record(map) | Term::RecRecord(map) => {
                                    println!("Loaded {} symbol(s) in the environment.", map.len())
                                }
                                _ => (),
                            })
                        }
                        Ok(Command::Typecheck(exp)) => {
                            repl.typecheck(&exp).map(|types| println!("Ok: {}", types))
                        }
                        Ok(Command::Query(exp)) => repl.query(&exp).map(|t| {
                            query_print::write_query_result(
                                &mut stdout,
                                &t,
                                query_print::Attributes::default(),
                            )
                            .unwrap();
                        }),
                        Ok(Command::Print(exp)) => {
                            match repl.eval_full(&exp) {
                                Ok(EvalResult::Evaluated(t)) => println!("{}\n", t.deep_repr()),
                                Ok(EvalResult::Bound(_)) => (),
                                Err(err) => program::report(repl.cache_mut(), err),
                            };
                            Ok(())
                        }
                        Ok(Command::Help(arg)) => {
                            print_help(&mut std::io::stdout(), arg.as_deref()).unwrap();
                            Ok(())
                        }
                        Ok(Command::Exit) => {
                            println!("{}", Style::new().bold().paint("Exiting"));
                            return Ok(());
                        }
                        Err(err) => Err(Error::from(err)),
                    };

                    if let Err(err) = result {
                        program::report(repl.cache_mut(), err);
                    } else {
                        println!();
                    }
                }
                Ok(line) => {
                    match repl.eval(&line) {
                        Ok(EvalResult::Evaluated(t)) => println!("{}\n", t.shallow_repr()),
                        Ok(EvalResult::Bound(_)) => (),
                        Err(err) => program::report(repl.cache_mut(), err),
                    };
                }
                Err(ReadlineError::Eof) => {
                    println!("{}", Style::new().bold().paint("Ctrl+D. Exiting"));
                    break Ok(());
                }
                Err(ReadlineError::Interrupted) => (),
                Err(err) => {
                    program::report(
                        repl.cache_mut(),
                        Error::IOError(IOError(format!("{}", err))),
                    );
                }
            }
        }
    }
}

/// Simple UI-agnostic interface to the REPL, taking string inputs and returning string inputs.
#[cfg(feature = "repl-wasm")]
pub mod simple_frontend {
    use super::command::{Command, CommandType, UnknownCommandError};
    use super::*;
    use crate::error::ToDiagnostic;
    use crate::program;
    use codespan::FileId;
    use codespan_reporting::term::termcolor::Ansi;
    use std::io::{Cursor, Write};
    use wasm_bindgen::prelude::*;

    #[wasm_bindgen]
    #[derive(Clone, Copy, Eq, PartialEq)]
    pub enum WASMResultTag {
        Success = 0,
        Blank = 1,
        Partial = 2,
        Error = 3,
    }

    #[wasm_bindgen]
    pub struct WASMInitResult {
        msg: String,
        pub tag: WASMResultTag,
        state: REPLState,
    }

    impl WASMInitResult {
        fn mk_error(msg: String) -> WASMInitResult {
            WASMInitResult {
                msg,
                tag: WASMResultTag::Error,
                state: REPLState(REPLImpl::new()),
            }
        }
    }

    #[wasm_bindgen]
    impl WASMInitResult {
        #[wasm_bindgen(getter)]
        pub fn msg(&self) -> String {
            self.msg.clone()
        }

        pub fn repl(self) -> REPLState {
            self.state
        }
    }

    #[wasm_bindgen]
    pub struct WASMInputResult {
        msg: String,
        pub tag: WASMResultTag,
    }

    impl WASMInputResult {
        fn mk_error(msg: String) -> WASMInputResult {
            WASMInputResult {
                msg,
                tag: WASMResultTag::Error,
            }
        }
    }

    #[wasm_bindgen]
    impl WASMInputResult {
        #[wasm_bindgen(getter)]
        pub fn msg(&self) -> String {
            self.msg.clone()
        }
    }

    pub enum InputResult {
        Success(String),
        Blank,
        Partial,
    }

    impl From<InputResult> for WASMInputResult {
        fn from(ir: InputResult) -> Self {
            match ir {
                InputResult::Success(msg) => WASMInputResult {
                    msg,
                    tag: WASMResultTag::Success,
                },
                InputResult::Blank => WASMInputResult {
                    msg: String::new(),
                    tag: WASMResultTag::Blank,
                },
                InputResult::Partial => WASMInputResult {
                    msg: String::new(),
                    tag: WASMResultTag::Partial,
                },
            }
        }
    }

    #[wasm_bindgen]
    pub struct REPLState(REPLImpl);

    #[wasm_bindgen]
    pub fn wasm_init() -> WASMInitResult {
        init()
            .map(|repl| WASMInitResult {
                msg: String::new(),
                tag: WASMResultTag::Success,
                state: REPLState(repl),
            })
            .unwrap_or_else(WASMInitResult::mk_error)
    }

    #[wasm_bindgen]
    pub fn wasm_input(state: &mut REPLState, line: &str) -> WASMInputResult {
        input(&mut state.0, line)
            .map(WASMInputResult::from)
            .unwrap_or_else(WASMInputResult::mk_error)
    }

    pub fn init() -> Result<REPLImpl, String> {
        let mut repl = REPLImpl::new();

        match repl.load_stdlib() {
            Ok(()) => (),
            Err(err) => {
                return Err(err_to_str(repl.cache_mut(), err));
            }
        }

        Ok(repl)
    }

    pub fn err_to_str<E: ToDiagnostic<FileId>>(cache: &mut Cache, err: E) -> String {
        let mut buffer = Ansi::new(Cursor::new(Vec::new()));
        let config = codespan_reporting::term::Config::default();
        let contracts_id = cache.id_of("<stdlib/contracts.ncl>");
        let diagnostics = err.to_diagnostic(cache.files_mut(), contracts_id);

        diagnostics
            .iter()
            .try_for_each(|d| {
                codespan_reporting::term::emit(&mut buffer, &config, cache.files_mut(), &d)
            })
            .unwrap();

        String::from_utf8(buffer.into_inner().into_inner()).unwrap()
    }

    pub fn input<R: REPL>(repl: &mut R, line: &str) -> Result<InputResult, String> {
        if line.trim().is_empty() {
            Ok(InputResult::Blank)
        } else if line.starts_with(':') {
            let cmd = line.chars().skip(1).collect::<String>().parse::<Command>();
            let result = match cmd {
                Ok(Command::Load(_)) => {
                    return Err(String::from(":load is not enabled on the online REPL."));
                }
                Ok(Command::Typecheck(exp)) => repl
                    .typecheck(&exp)
                    .map(|types| InputResult::Success(format!("Ok: {}", types))),
                Ok(Command::Query(exp)) => repl.query(&exp).map(|t| {
                    let mut buffer = Cursor::new(Vec::<u8>::new());
                    query_print::write_query_result(
                        &mut buffer,
                        &t,
                        query_print::Attributes::default(),
                    )
                    .unwrap();
                    InputResult::Success(String::from_utf8(buffer.into_inner()).unwrap())
                }),
                Ok(Command::Print(exp)) => repl.eval_full(&exp).map(|res| match res {
                    EvalResult::Evaluated(t) => InputResult::Success(t.deep_repr()),
                    EvalResult::Bound(_) => InputResult::Blank,
                }),
                Ok(Command::Help(arg)) => {
                    let mut buffer = Cursor::new(Vec::<u8>::new());
                    simple_frontend::print_help(&mut buffer, arg.as_deref()).unwrap();
                    Ok(InputResult::Success(
                        String::from_utf8(buffer.into_inner()).unwrap(),
                    ))
                }
                Ok(Command::Exit) => Ok(InputResult::Success(String::from("Exiting"))),
                Err(err) => Err(Error::from(err)),
            };

            Ok(
                result
                    .unwrap_or_else(|err| InputResult::Success(err_to_str(repl.cache_mut(), err))),
            )
        } else {
            match repl.eval(&line) {
                Ok(EvalResult::Evaluated(t)) => {
                    Ok(InputResult::Success(format!("{}\n", t.shallow_repr())))
                }
                Ok(EvalResult::Bound(_)) => Ok(InputResult::Success(String::new())),
                Err(err) => Ok(InputResult::Success(err_to_str(repl.cache_mut(), err))),
            }
        }
    }
}

/// Rendering of the results of a metadata query.
pub mod query_print {
    use super::{io, Write};
    use crate::identifier::Ident;
    use crate::term::{MergePriority, MetaValue, Term};

    /// A query printer. The implementation may differ depending on the activation of markdown
    /// support.
    pub trait QueryPrinter {
        /// Print a metadata attribute.
        fn write_metadata(&self, out: &mut impl Write, attr: &str, value: &str) -> io::Result<()>;
        /// Print the documentation attribute.
        fn write_doc(&self, out: &mut impl Write, content: &str) -> io::Result<()>;
        /// Print the list of fields of a record.
        fn write_fields<'a, I>(&self, out: &mut impl Write, fields: I) -> io::Result<()>
        where
            I: Iterator<Item = &'a Ident>;
    }

    #[cfg(feature = "markdown")]
    pub struct MarkdownRenderer {
        skin: termimad::MadSkin,
    }

    pub struct SimpleRenderer {}

    /// Helper to render the result of the `query` sub-command without markdown support.
    impl QueryPrinter for SimpleRenderer {
        fn write_metadata(&self, out: &mut impl Write, attr: &str, value: &str) -> io::Result<()> {
            writeln!(out, "* {}: {}", attr, value)
        }

        fn write_doc(&self, out: &mut impl Write, content: &str) -> io::Result<()> {
            if content.find('\n').is_none() {
                self.write_metadata(out, "documentation", &content)
            } else {
                writeln!(out, "* documentation\n")?;
                writeln!(out, "{}", content)
            }
        }

        fn write_fields<'a, I>(&self, out: &mut impl Write, fields: I) -> io::Result<()>
        where
            I: Iterator<Item = &'a Ident>,
        {
            writeln!(out, "Available fields:")?;

            for field in fields {
                writeln!(out, " - {}", field)?;
            }

            Ok(())
        }
    }

    #[cfg(feature = "markdown")]
    impl MarkdownRenderer {
        pub fn new() -> Self {
            MarkdownRenderer {
                skin: termimad::MadSkin::default(),
            }
        }
    }

    #[cfg(feature = "markdown")]
    fn termimad_to_io(err: termimad::Error) -> io::Error {
        match err {
            termimad::Error::IO(err) => err,
            termimad::Error::Crossterm(err) => {
                io::Error::new(io::ErrorKind::Other, err.to_string())
            }
        }
    }

    /// Helper to render the result of the `query` sub-command with markdown support on the
    /// terminal.
    #[cfg(feature = "markdown")]
    impl QueryPrinter for MarkdownRenderer {
        fn write_metadata(&self, out: &mut impl Write, attr: &str, value: &str) -> io::Result<()> {
            use minimad::*;
            use termimad::*;

            let mut expander = OwningTemplateExpander::new();
            let template = TextTemplate::from("* **${attr}**: *${value}*");

            expander.set("attr", attr);
            expander.set("value", value);
            let text = expander.expand(&template);
            let (width, _) = terminal_size();
            let fmt_text = FmtText::from_text(&self.skin, text, Some(width as usize));
            write!(out, "{}", fmt_text)
        }

        fn write_doc(&self, out: &mut impl Write, content: &str) -> io::Result<()> {
            if content.find('\n').is_none() {
                self.skin
                    .write_text_on(out, &format!("* **documentation**: {}", content))
                    .map_err(termimad_to_io)
            } else {
                self.skin
                    .write_text_on(out, "* **documentation**\n\n")
                    .map_err(termimad_to_io)?;
                self.skin
                    .write_text_on(out, content)
                    .map_err(termimad_to_io)
            }
        }

        fn write_fields<'a, I>(&self, out: &mut impl Write, fields: I) -> io::Result<()>
        where
            I: Iterator<Item = &'a Ident>,
        {
            use minimad::*;
            use termimad::*;

            let (width, _) = terminal_size();
            let mut expander = OwningTemplateExpander::new();
            let template = TextTemplate::from("* ${field}");

            self.skin
                .write_text_on(out, "## Available fields")
                .map_err(termimad_to_io)?;

            for field in fields {
                expander.set("field", field.to_string());
                let text = expander.expand(&template);
                let fmt_text = FmtText::from_text(&self.skin, text, Some(width as usize));
                write!(out, "{}", fmt_text)?;
            }

            Ok(())
        }
    }

    /// Represent which metadata attributes are requested by a query.
    #[derive(Clone, Copy, Eq, PartialEq)]
    pub struct Attributes {
        pub doc: bool,
        pub contract: bool,
        pub default: bool,
        pub value: bool,
    }

    // By default, show all available metadata.
    impl Default for Attributes {
        fn default() -> Self {
            Attributes {
                doc: true,
                contract: true,
                default: true,
                value: true,
            }
        }
    }

    /// Print the result of a metadata query, which is a "weakly" evaluated term (see
    /// [`eval_meta`](../../eval/fn.eval_meta.html) and [`query`](../../program/fn.query.html)).
    ///
    /// Wrapper around [`print_query_result_`](./fn.print_query_result_) that selects an adapated
    /// query printer at compile time.
    pub fn write_query_result(
        out: &mut impl Write,
        term: &Term,
        selected_attrs: Attributes,
    ) -> io::Result<()> {
        #[cfg(feature = "markdown")]
        let renderer = MarkdownRenderer::new();

        #[cfg(not(feature = "markdown"))]
        let renderer = SimpleRenderer {};

        write_query_result_(out, term, selected_attrs, &renderer)
    }

    /// Print the result of a metadata query, which is a "weakly" evaluated term (see
    /// [`eval_meta`](../../eval/fn.eval_meta.html) and [`query`](../../program/fn.query.html)).
    fn write_query_result_<R: QueryPrinter>(
        out: &mut impl Write,
        term: &Term,
        selected_attrs: Attributes,
        renderer: &R,
    ) -> io::Result<()> {
        // Print a list the fields of a term if it is a record, or do nothing otherwise.
        fn write_fields<R: QueryPrinter>(
            out: &mut impl Write,
            renderer: &R,
            t: &Term,
        ) -> io::Result<()> {
            writeln!(out)?;
            match t {
                Term::Record(map) | Term::RecRecord(map) if !map.is_empty() => {
                    let mut fields: Vec<_> = map.keys().collect();
                    fields.sort();
                    renderer.write_fields(out, fields.into_iter())
                }
                Term::Record(_) | Term::RecRecord(_) => renderer.write_metadata(out, "value", "{}"),
                _ => Ok(()),
            }
        }

        match term {
            Term::MetaValue(meta) => {
                let mut found = false;
                if !meta.contracts.is_empty() && selected_attrs.contract {
                    let ctrs: Vec<String> = meta
                        .contracts
                        .iter()
                        // We use the original user-written type stored in the label. Using
                        // `ctr.types` instead is unreadable most of the time, as it can have been
                        // altered by closurizations or other run-time rewriting
                        .map(|ctr| ctr.label.types.to_string())
                        .collect();
                    renderer.write_metadata(out, "contract", &ctrs.join(","))?;
                    found = true;
                }

                match &meta {
                    MetaValue {
                        priority: MergePriority::Default,
                        value: Some(t),
                        ..
                    } if selected_attrs.default => {
                        renderer.write_metadata(out, "default", &t.as_ref().shallow_repr())?;
                        found = true;
                    }
                    MetaValue {
                        priority: MergePriority::Normal,
                        value: Some(t),
                        ..
                    } if selected_attrs.value => {
                        renderer.write_metadata(out, "value", &t.as_ref().shallow_repr())?;
                        found = true;
                    }
                    _ => (),
                }

                match meta.doc {
                    Some(ref s) if selected_attrs.doc => {
                        renderer.write_doc(out, s)?;
                        found = true;
                    }
                    _ => (),
                }

                if !found {
                    writeln!(out, "Requested metadata were not found for this value.")?;
                    meta.value
                        .iter()
                        .try_for_each(|rt| write_fields(out, renderer, rt.as_ref()))?;
                }

                meta.value
                    .iter()
                    .try_for_each(|rt| write_fields(out, renderer, rt.as_ref()))?;
            }
            t @ Term::Record(_) | t @ Term::RecRecord(_) => {
                writeln!(out, "No metadata found for this value.")?;
                write_fields(out, renderer, &t)?;
            }
            t => {
                writeln!(out, "jo metadata found for this value.\n")?;
                if selected_attrs.value {
                    renderer.write_metadata(out, "value", &t.shallow_repr())?;
                }
            }
        };

        Ok(())
    }
}
