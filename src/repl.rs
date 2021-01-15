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
    fn query(&mut self, exp: &str) -> Result<Term, Error>;
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

    fn query(&mut self, exp: &str) -> Result<Term, Error> {
        use crate::program;

        let file_id = self.cache.add_tmp("<repl-query>", String::from(exp));
        program::query(&mut self.cache, file_id, &self.eval_env, None)
    }

    fn cache_mut<'a>(&'a mut self) -> &'a mut Cache {
        &mut self.cache
    }
}

#[cfg(feature = "repl")]
pub mod rustyline_frontend {
    use super::*;
    use crate::error::REPLError;
    use crate::program;
    use ansi_term::{Colour, Style};
    use codespan::{FileId, Files};
    use codespan_reporting::diagnostic::Diagnostic;
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
        fn require_arg(cmd: &str, arg: &str, msg_opt: Option<&str>) -> Result<(), REPLError> {
            if arg.trim().is_empty() {
                Err(REPLError::MissingArg {
                    cmd: String::from(cmd),
                    msg_opt: msg_opt.map(String::from),
                })
            } else {
                Ok(())
            }
        }

        let mut repl = REPLImpl::new();
        match repl.load_stdlib() {
            Ok(()) => (),
            Err(err) => {
                program::report(repl.cache_mut(), err);
                return Err(InitError::Stdlib);
            }
        }

        let config = config();
        let mut editor: Editor<()> = Editor::with_config(config);
        let prompt = Style::new().fg(Colour::Green).paint("> ").to_string();

        loop {
            match editor.readline(&prompt) {
                Ok(line) if line.starts_with(":") => {
                    let cmd_end = line.find(" ").unwrap_or(line.len());
                    let command: String = line.chars().skip(1).take(cmd_end - 1).collect();
                    let arg: String = line.chars().skip(cmd_end + 1).collect();

                    let result = match command.as_str() {
                        "load" => {
                            require_arg(&command, &arg, Some("Please provid a file to load."))
                                .map_err(Error::from)
                                .and_then(|()| {
                                    repl.load(&arg).map(|term| match term.as_ref() {
                                        Term::Record(map) | Term::RecRecord(map) => println!(
                                            "Loaded {} symbol(s) in the environment.",
                                            map.len()
                                        ),
                                        _ => (),
                                    })
                                })
                        }
                        "typecheck" => require_arg(&command, &arg, None)
                            .map_err(Error::from)
                            .and_then(|()| {
                                repl.typecheck(&arg).map(|types| println!("Ok: {}", types))
                            }),
                        "query" => require_arg(&command, &arg, None)
                            .map_err(Error::from)
                            .and_then(|()| repl.query(&arg))
                            .map(|t| {
                                query_print::print_query_result(
                                    &t,
                                    query_print::Attributes::default(),
                                )
                            }),
                        "?" | "help" => {
                            print_help(&arg);
                            Ok(())
                        }
                        "exit" => {
                            println!("{}", Style::new().bold().paint("Exiting"));
                            return Ok(());
                        }
                        cmd => Err(Error::REPLError(REPLError::UnknownCommand(String::from(
                            cmd,
                        )))),
                    };

                    if let Err(err) = result {
                        program::report(repl.cache_mut(), err);
                    }
                }
                Ok(line) => {
                    match repl.eval(&line) {
                        Ok(t) => println!("{}\n", t.shallow_repr()),
                        Err(err) => program::report(repl.cache_mut(), err),
                    };
                }
                Err(ReadlineError::Interrupted) => {
                    println!("{}", Style::new().bold().paint("Interrupted. Exiting"));
                    break Ok(());
                }
                Err(ReadlineError::Eof) => (),
                Err(err) => {
                    program::report(
                        repl.cache_mut(),
                        Error::IOError(IOError(format!("{}", err))),
                    );
                }
            }
        }
    }

    fn print_help(arg: &str) {
        match arg.trim() {
            "help" => {
                println!(":help [command]");
                println!("Prints a list of available commands or the help of the given command");
            }
            "query" => {
                println!(":query <expression>");
                println!("Print the metadata attached to an attribute");
            }
            "load" => {
                println!(":load <file>");
                print!("Evaluate the content of <file> to a record and load its attributes in the environment.");
                println!(" Fail if the content of <file> doesn't evaluate to a record");
            }
            "typecheck" => {
                println!(":typecheck <expression>");
                println!("Typecheck the given expression and print its top-level type");
            }
            "exit" => {
                println!(":exit");
                println!("Exit the REPL session");
            }
            "" => println!("Available commands: ? help query load typecheck"),
            _ => (),
        };

        println!();
    }
}

pub mod query_print {
    use crate::identifier::Ident;
    use crate::label::Label;
    use crate::term::{MergePriority, MetaValue, Term};

    pub trait QueryPrinter {
        fn print_metadata(&self, attr: &str, value: &str);
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

    /// Helper to render the result of the `query` sub-command with markdown support.
    impl QueryPrinter for SimpleRenderer {
        fn print_metadata(&self, attr: &str, value: &str) {
            println!("* {}: {}", attr, value);
        }

        fn print_doc(&self, content: &str) {
            if content.find("\n").is_none() {
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
            if content.find("\n").is_none() {
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

    /// Which attributes are requested.
    #[derive(Clone, Copy, Eq, PartialEq)]
    pub struct Attributes {
        pub doc: bool,
        pub contract: bool,
        pub default: bool,
        pub value: bool,
    }

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

    pub fn print_query_result(term: &Term, selected_attrs: Attributes) {
        #[cfg(feature = "markdown")]
        let renderer = MarkdownRenderer::new();

        #[cfg(not(feature = "markdown"))]
        let renderer = SimpleRenderer {};

        print_query_result_(term, selected_attrs, &renderer)
    }

    pub fn print_query_result_<R: QueryPrinter>(
        term: &Term,
        selected_attrs: Attributes,
        renderer: &R,
    ) {
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
                match &meta.contract {
                    Some((_, Label { types, .. })) if selected_attrs.contract => {
                        renderer.print_metadata("contract", &format!("{}", types));
                        found = true;
                    }
                    _ => (),
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
