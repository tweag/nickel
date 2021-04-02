//! The Nickel REPL.
//!
//! A backend designates a module which actually executes a sequence of REPL commands, while being
//! agnostic to the user interface and the presentation of the results.
//!
//! Dually, the frontend is the user-facing part, which may be a CLI, a web application, a
//! jupyter-kernel (which is not exactly user-facing, but still manages input/output and
//! formatting), etc.
use crate::cache::Cache;
use crate::error::{Error, EvalError, IOError};
use crate::error::{ParseError, REPLError};
use crate::identifier::Ident;
use crate::parser::{grammar, lexer, ExtendedTerm};
use crate::term::{RichTerm, Term};
use crate::types::Types;
use crate::{eval, transformations, typecheck};
use simple_counter::*;
use std::ffi::{OsStr, OsString};
use std::result::Result;
use std::str::FromStr;

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

impl REPL for REPLImpl {
    fn eval(&mut self, exp: &str) -> Result<EvalResult, Error> {
        let file_id = self.cache.add_string(
            format!("repl-input-{}", InputNameCounter::next()),
            String::from(exp),
        );

        match self
            .parser
            .parse(file_id, lexer::Lexer::new(exp))
            .map_err(|err| ParseError::from_lalrpop(err, file_id))?
        {
            ExtendedTerm::RichTerm(t) => {
                typecheck::type_check_in_env(&t, &self.type_env, &self.cache)?;
                let t = transformations::transform(t, &mut self.cache)?;
                Ok(eval::eval(t, &self.eval_env, &mut self.cache)?.into())
            }
            ExtendedTerm::ToplevelLet(id, t) => {
                typecheck::type_check_in_env(&t, &self.type_env, &self.cache)?;
                typecheck::Envs::env_add(&mut self.type_env, id.clone(), &t);

                let t = transformations::transform(t, &mut self.cache)?;

                let local_env = self.eval_env.clone();
                eval::env_add(&mut self.eval_env, id.clone(), t, local_env);
                Ok(EvalResult::Bound(id))
            }
        }
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
                Help(..) => CommandType::Help,
                Exit => CommandType::Exit,
            }
        }
    }
}

/// Native terminal implementation of an REPL frontend using rustyline.
#[cfg(feature = "repl")]
pub mod rustyline_frontend {
    use super::command::{Command, CommandType, UnknownCommandError};
    use super::*;

    use crate::error::ParseError;
    use crate::program;
    use ansi_term::{Colour, Style};
    use codespan::FileId;
    use rustyline::config::OutputStreamType;
    use rustyline::error::ReadlineError;
    use rustyline::validate::{ValidationContext, ValidationResult, Validator};
    use rustyline::{Config, EditMode, Editor};
    use rustyline_derive::{Completer, Helper, Highlighter, Hinter};

    /// Validator enabling multiline input.
    ///
    /// The behavior is the following:
    /// - always end an input that starts with the command prefix `:`
    /// - otherwise, try to parse the input. If an unexpected end of file error occurs, continue
    ///   the input in a new line. Otherwise, accept and end the input.
    //TODO: the validator throws away the result of parsing, or the parse error, when accepting an
    //input, meaning that the work is done a second time by the REPL. Validator's work could be
    //reused. This overhead shouldn't be dramatic for the typical REPL input size, though.
    #[derive(Completer, Helper, Highlighter, Hinter)]
    pub struct MultilineValidator {
        parser: grammar::ExtendedTermParser,
        /// Currently the parser expect a `FileId` to fill in location information. For this
        /// validator, this may be a dummy one, since for now location information is not used.
        file_id: FileId,
    }

    impl MultilineValidator {
        fn new(file_id: FileId) -> Self {
            MultilineValidator {
                parser: grammar::ExtendedTermParser::new(),
                file_id,
            }
        }
    }

    impl Validator for MultilineValidator {
        fn validate(&self, ctx: &mut ValidationContext<'_>) -> rustyline::Result<ValidationResult> {
            let input = ctx.input();

            if input.starts_with(':') || input.trim().is_empty() {
                return Ok(ValidationResult::Valid(None));
            }

            let result = self
                .parser
                .parse(self.file_id, lexer::Lexer::new(ctx.input()))
                .map_err(|err| ParseError::from_lalrpop(err, self.file_id));

            match result {
                Err(ParseError::UnexpectedEOF(..)) | Err(ParseError::UnmatchedCloseBrace(..)) => {
                    Ok(ValidationResult::Invalid(None))
                }
                _ => Ok(ValidationResult::Valid(None)),
            }
        }
    }

    /// Error occurring when initializing the REPL.
    pub enum InitError {
        /// Unable to load, parse or typecheck the stdlib
        Stdlib,
    }

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

        let validator =
            MultilineValidator::new(repl.cache_mut().add_tmp("<repl-input>", String::new()));

        let mut editor = Editor::with_config(config());
        editor.set_helper(Some(validator));
        let prompt = Style::new().fg(Colour::Green).paint("nickel> ").to_string();

        loop {
            let line = editor.readline(&prompt);

            if let Ok(line) = line.as_ref() {
                editor.add_history_entry(line.clone());
            }

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
                            query_print::print_query_result(&t, query_print::Attributes::default());
                        }),
                        Ok(Command::Help(arg)) => {
                            print_help(arg.as_deref());
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

    /// Print the help message corresponding to a command, or show a list of available commands if
    /// the argument is `None` or is not a command.
    fn print_help(arg: Option<&str>) {
        if let Some(arg) = arg {
            fn print_aliases(cmd: CommandType) {
                let mut aliases = cmd.aliases().into_iter();

                if let Some(fst) = aliases.next() {
                    print!("Aliases: `{}`", fst);
                    aliases.for_each(|alias| print!(", `{}`", alias));
                    println!();
                }

                println!();
            }

            match arg.parse::<CommandType>() {
                Ok(c @ CommandType::Help) => {
                    println!(":{} [command]", c);
                    print_aliases(c);
                    println!(
                        "Prints a list of available commands or the help of the given command"
                    );
                }
                Ok(c @ CommandType::Query) => {
                    println!(":{} <expression>", c);
                    print_aliases(c);
                    println!("Print the metadata attached to an attribute");
                }
                Ok(c @ CommandType::Load) => {
                    println!(":{} <file>", c);
                    print_aliases(c);
                    print!("Evaluate the content of <file> to a record and load its attributes in the environment.");
                    println!(" Fail if the content of <file> doesn't evaluate to a record");
                }
                Ok(c @ CommandType::Typecheck) => {
                    println!(":{} <expression>", c);
                    print_aliases(c);
                    println!("Typecheck the given expression and print its top-level type");
                }
                Ok(c @ CommandType::Exit) => {
                    println!(":{}", c);
                    print_aliases(c);
                    println!("Exit the REPL session");
                }
                Err(UnknownCommandError {}) => {
                    println!("Unknown command `{}`.", arg);
                    println!("Available commands: ? help query load typecheck");
                }
            }
        } else {
            println!("Available commands: help query load typecheck exit");
        }
    }
}

/// Rendering of the results of a metadata query.
pub mod query_print {
    use crate::identifier::Ident;
    use crate::term::{MergePriority, MetaValue, Term};

    /// A query printer. The implementation may differ depending on the activation of markdown
    /// support.
    pub trait QueryPrinter {
        /// Print a metadata attribute.
        fn print_metadata(&self, attr: &str, value: &str);
        /// Print the documentation attribute.
        fn print_doc(&self, content: &str);
        /// Print the list of fields of a record.
        fn print_fields<'a, I>(&self, fields: I)
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
        fn print_metadata(&self, attr: &str, value: &str) {
            println!("* {}: {}", attr, value);
        }

        fn print_doc(&self, content: &str) {
            if content.find('\n').is_none() {
                self.print_metadata("documentation", &content);
            } else {
                println!("* documentation\n");
                println!("{}", content);
            }
        }

        fn print_fields<'a, I>(&self, fields: I)
        where
            I: Iterator<Item = &'a Ident>,
        {
            println!("Available fields:");

            for field in fields {
                println!(" - {}", field);
            }
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

    /// Helper to render the result of the `query` sub-command with markdown support.
    #[cfg(feature = "markdown")]
    impl QueryPrinter for MarkdownRenderer {
        fn print_metadata(&self, attr: &str, value: &str) {
            use minimad::*;
            use termimad::*;

            let mut expander = OwningTemplateExpander::new();
            let template = TextTemplate::from("* **${attr}**: *${value}*");

            expander.set("attr", attr);
            expander.set("value", value);
            let text = expander.expand(&template);
            let (width, _) = terminal_size();
            let fmt_text = FmtText::from_text(&self.skin, text, Some(width as usize));
            print!("{}", fmt_text);
        }

        fn print_doc(&self, content: &str) {
            if content.find('\n').is_none() {
                self.skin
                    .print_text(&format!("* **documentation**: {}", content));
            } else {
                self.skin.print_text("* **documentation**\n\n");
                self.skin.print_text(content);
            }
        }

        fn print_fields<'a, I>(&self, fields: I)
        where
            I: Iterator<Item = &'a Ident>,
        {
            use minimad::*;
            use termimad::*;

            let (width, _) = terminal_size();
            let mut expander = OwningTemplateExpander::new();
            let template = TextTemplate::from("* ${field}");

            self.skin.print_text("## Available fields");

            for field in fields {
                expander.set("field", field.to_string());
                let text = expander.expand(&template);
                let fmt_text = FmtText::from_text(&self.skin, text, Some(width as usize));
                print!("{}", fmt_text);
            }
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
    pub fn print_query_result(term: &Term, selected_attrs: Attributes) {
        #[cfg(feature = "markdown")]
        let renderer = MarkdownRenderer::new();

        #[cfg(not(feature = "markdown"))]
        let renderer = SimpleRenderer {};

        print_query_result_(term, selected_attrs, &renderer)
    }

    /// Print the result of a metadata query, which is a "weakly" evaluated term (see
    /// [`eval_meta`](../../eval/fn.eval_meta.html) and [`query`](../../program/fn.query.html)).
    fn print_query_result_<R: QueryPrinter>(term: &Term, selected_attrs: Attributes, renderer: &R) {
        // Print a list the fields of a term if it is a record, or do nothing otherwise.
        fn print_fields<R: QueryPrinter>(renderer: &R, t: &Term) {
            println!();
            match t {
                Term::Record(map) | Term::RecRecord(map) if !map.is_empty() => {
                    let mut fields: Vec<_> = map.keys().collect();
                    fields.sort();
                    renderer.print_fields(fields.into_iter());
                }
                Term::Record(_) | Term::RecRecord(_) => renderer.print_metadata("value", "{}"),
                _ => (),
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
                    renderer.print_metadata("contract", &ctrs.join(","));
                    found = true;
                }

                match &meta {
                    MetaValue {
                        priority: MergePriority::Default,
                        value: Some(t),
                        ..
                    } if selected_attrs.default => {
                        renderer.print_metadata("default", &t.as_ref().shallow_repr());
                        found = true;
                    }
                    MetaValue {
                        priority: MergePriority::Normal,
                        value: Some(t),
                        ..
                    } if selected_attrs.value => {
                        renderer.print_metadata("value", &t.as_ref().shallow_repr());
                        found = true;
                    }
                    _ => (),
                }

                match meta.doc {
                    Some(ref s) if selected_attrs.doc => {
                        renderer.print_doc(s);
                        found = true;
                    }
                    _ => (),
                }

                if !found {
                    println!("Requested metadata were not found for this value.");
                    meta.value
                        .iter()
                        .for_each(|rt| print_fields(renderer, rt.as_ref()));
                }

                meta.value
                    .iter()
                    .for_each(|rt| print_fields(renderer, rt.as_ref()));
            }
            t @ Term::Record(_) | t @ Term::RecRecord(_) => {
                println!("No metadata found for this value.");
                print_fields(renderer, &t)
            }
            t => {
                println!("No metadata found for this value.\n");
                if selected_attrs.value {
                    renderer.print_metadata("value", &t.shallow_repr());
                }
            }
        }
    }
}
