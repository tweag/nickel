//! Native terminal implementation of a REPL frontend using rustyline.
use std::path::PathBuf;

use super::{command::Command, *};

use crate::{error::report::ColorOpt, eval::cache::CacheImpl};

use ansi_term::Style;
use rustyline::{error::ReadlineError, Config, EditMode, Editor};

/// The config of rustyline's editor.
pub fn config(color_opt: ColorOpt) -> Config {
    Config::builder()
        .history_ignore_space(true)
        .edit_mode(EditMode::Emacs)
        .color_mode(color_opt.into())
        .auto_add_history(true)
        .build()
}

impl From<ColorOpt> for rustyline::config::ColorMode {
    fn from(c: ColorOpt) -> Self {
        use rustyline::config::ColorMode;
        match c.0 {
            clap::ColorChoice::Always => ColorMode::Forced,
            clap::ColorChoice::Auto => ColorMode::Enabled,
            clap::ColorChoice::Never => ColorMode::Disabled,
        }
    }
}

/// Main loop of the REPL.
pub fn repl(histfile: PathBuf, color_opt: ColorOpt) -> Result<(), InitError> {
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
    let _ = editor.load_history(&histfile);
    editor.set_helper(Some(validator));

    let result = loop {
        let line = editor.readline("nickel> ");
        let mut stdout = std::io::stdout();

        match line {
            Ok(line) if line.trim().is_empty() => (),
            Ok(line) if line.starts_with(':') => {
                let cmd = line.chars().skip(1).collect::<String>().parse::<Command>();
                let result = match cmd {
                    Ok(Command::Load(path)) => repl.load(&path).map(|term| match term.as_ref() {
                        Term::Record(record) => {
                            println!(
                                "Loaded {} symbol(s) in the environment.",
                                record.fields.len()
                            )
                        }
                        Term::RecRecord(record, dyn_fields, ..) => {
                            if !dyn_fields.is_empty() {
                                println!(
                                    "Warning: loading dynamic fields is currently not supported. \
                                    {} symbols ignored",
                                    dyn_fields.len()
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
                        println!("{}", Style::new().bold().paint("Exiting"));
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
                println!("{}", Style::new().bold().paint("Ctrl+D. Exiting"));
                break Ok(());
            }
            Err(ReadlineError::Interrupted) => (),
            Err(err) => {
                let _ = editor.save_history(&histfile);
                repl.report(Error::IOError(IOError(format!("{err}"))), color_opt);
            }
        }
    };

    let _ = editor.save_history(&histfile);
    result
}
