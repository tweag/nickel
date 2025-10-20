//! Native terminal implementation of a REPL frontend using rustyline.
use std::path::PathBuf;

use super::{command::Command, *};

use crate::{
    bytecode::value::{RecordBody, TermBody, ValueContentRef},
    error::report::ColorOpt,
    eval::cache::CacheImpl,
};

use anstyle::Style;
use rustyline::{Config, EditMode, Editor, error::ReadlineError};

/// The config of rustyline's editor.
pub fn config(color_opt: ColorOpt) -> Config {
    Config::builder()
        .history_ignore_space(true)
        .edit_mode(EditMode::Emacs)
        .color_mode(color_mode_from_opt(color_opt))
        .auto_add_history(true)
        .build()
}

fn color_mode_from_opt(c: ColorOpt) -> rustyline::config::ColorMode {
    use rustyline::config::ColorMode;
    match c {
        colorchoice::ColorChoice::Always => ColorMode::Forced,
        colorchoice::ColorChoice::AlwaysAnsi => ColorMode::Enabled,
        colorchoice::ColorChoice::Auto => ColorMode::Enabled,
        colorchoice::ColorChoice::Never => ColorMode::Disabled,
    }
}

/// Main loop of the REPL.
pub fn repl(histfile: Option<PathBuf>, color_opt: ColorOpt) -> Result<(), InitError> {
    let mut repl = ReplImpl::<CacheImpl>::new(std::io::stderr());

    match repl.load_stdlib() {
        Ok(()) => (),
        Err(err) => {
            repl.report(err, color_opt);
            return Err(InitError::Stdlib);
        }
    }

    let validator = InputParser::new(
        repl.cache_mut()
            .replace_string(SourcePath::ReplInput(0), String::new()),
    );

    let mut editor = Editor::with_config(config(color_opt))
        .map_err(|readline_err| InitError::ReadlineError(format!("{readline_err}")))?;
    if let Some(histfile) = histfile.as_ref() {
        let _ = editor.load_history(histfile);
    }
    editor.set_helper(Some(validator));

    let result = loop {
        let line = editor.readline("nickel> ");
        let mut stdout = std::io::stdout();

        match line {
            Ok(line) if line.trim().is_empty() => (),
            Ok(line) if line.starts_with(':') => {
                let cmd = line.chars().skip(1).collect::<String>().parse::<Command>();
                let result = match cmd {
                    Ok(Command::Load(path)) => repl.load(&path).map(|term| match term.content_ref() {
                        ValueContentRef::Record(container) => {
                            println!(
                                "Loaded {} symbol(s) in the environment.",
                                container.len()
                            )
                        }
                        ValueContentRef::Term(TermBody(Term::RecRecord(record, includes, dyn_fields, ..))) => {
                            if !dyn_fields.is_empty() {
                                println!(
                                    "Warning: loading dynamic fields is currently not supported. \
                                    {} symbols ignored",
                                    dyn_fields.len()
                                );
                            }

                            if !includes.is_empty() {
                                println!(
                                    "Warning: loading record with `include` expressions is currently not supported. \
                                    {} symbols ignored",
                                    includes.len()
                                );
                            }

                            println!(
                                "Loaded {} symbol(s) in the environment.",
                                record.fields.len()
                            )
                        }
                        _ => (),
                    }),
                    Ok(Command::Typecheck(exp)) => {
                        repl.typecheck(&exp).map(|typ| println!("Ok: {typ}"))
                    }
                    Ok(Command::Query(path)) => repl.query(path).map(|field| {
                        query_print::write_query_result(
                            &mut stdout,
                            &field,
                            query_print::Attributes::default(),
                        )
                        .unwrap();
                    }),
                    Ok(Command::Print(exp)) => {
                        match repl.eval_full(&exp) {
                            Ok(EvalResult::Evaluated(rt)) => println!("{rt}"),
                            Ok(EvalResult::Bound(_)) => (),
                            Err(err) => repl.report(err, color_opt),
                        };
                        Ok(())
                    }
                    Ok(Command::Help(arg)) => {
                        print_help(&mut std::io::stdout(), arg.as_deref()).unwrap();
                        Ok(())
                    }
                    Ok(Command::Exit) => {
                        println!("{}Exiting", Style::new().bold());
                        break Ok(());
                    }
                    Err(err) => Err(Error::from(err)),
                };

                if let Err(err) = result {
                    repl.report(err, color_opt);
                } else {
                    println!();
                }
            }
            Ok(line) => {
                match repl.eval_full(&line) {
                    Ok(EvalResult::Evaluated(rt)) => println!("{rt}\n"),
                    Ok(EvalResult::Bound(_)) => (),
                    Err(err) => repl.report(err, color_opt),
                };
            }
            Err(ReadlineError::Eof) => {
                println!("{}Ctrl+D. Exiting", Style::new().bold());
                break Ok(());
            }
            Err(ReadlineError::Interrupted) => (),
            Err(err) => {
                if let Some(histfile) = histfile.as_ref() {
                    let _ = editor.save_history(histfile);
                }
                repl.report(Error::IOError(IOError(format!("{err}"))), color_opt);
            }
        }
    };

    if let Some(histfile) = histfile.as_ref() {
        let _ = editor.save_history(histfile);
    }
    result
}
