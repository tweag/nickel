//! Native terminal implementation of a REPL frontend using rustyline.
use std::path::PathBuf;

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
pub fn repl(histfile: PathBuf) -> Result<(), InitError> {
    let mut repl = ReplImpl::new();

    match repl.load_stdlib() {
        Ok(()) => (),
        Err(err) => {
            program::report(repl.cache_mut(), err);
            return Err(InitError::Stdlib);
        }
    }

    let validator = InputParser::new(repl.cache_mut().add_tmp("<repl-input>", String::new()));

    let mut editor = Editor::with_config(config());
    let _ = editor.load_history(&histfile);
    editor.set_helper(Some(validator));
    let prompt = Style::new().fg(Colour::Green).paint("nickel> ").to_string();

    let result = loop {
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
                    Ok(Command::Load(path)) => repl.load(&path).map(|term| match term.as_ref() {
                        Term::Record(record) => {
                             println!("Loaded {} symbol(s) in the environment.", record.fields.len())
                        }
                        Term::RecRecord(record, dyn_fields, ..) => {
                            if !dyn_fields.is_empty() {
                                println!("Warning: loading dynamic fields is currently not supported. {} symbols ignored", dyn_fields.len());
                            }

                            println!("Loaded {} symbol(s) in the environment.", record.fields.len())
                        }
                        _ => (),
                    }),
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
                            Ok(EvalResult::Evaluated(rt)) => println!("{}\n", rt.as_ref().deep_repr()),
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
                        break Ok(());
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
                match repl.eval_full(&line) {
                    Ok(EvalResult::Evaluated(rt)) => println!("{}\n", rt.as_ref().deep_repr()),
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
                let _ = editor.save_history(&histfile);
                program::report(
                    repl.cache_mut(),
                    Error::IOError(IOError(format!("{}", err))),
                );
            }
        }
    };
    let _ = editor.save_history(&histfile);
    result
}
