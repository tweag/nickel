//! Simple, UI-agnostic interface to the REPL. Take string inputs and return string outputs.
//! The output may contain ANSI escape codes.
use super::{command::Command, *};
use crate::error::Error;
use crate::{serialize, serialize::ExportFormat};
use std::io::Cursor;

/// Add a failure mode to usual errors for features that are not supported by all REPLs (for
/// example, the `:load` command in the WASM frontend).
pub enum InputError {
    NickelError(Error),
    Other(String),
}

/// The successful result of the evaluation of an input.
pub enum InputResult {
    /// The input succeeded with associated error message.
    Success(String),
    /// The input was blank.
    Blank,
    /// The input is incomplete.
    Partial,
}

impl From<Error> for InputError {
    fn from(error: Error) -> InputError {
        InputError::NickelError(error)
    }
}

/// Return a new instance of an REPL with the standard library loaded.
pub fn init<EC: EvalCache>(trace: impl Write + 'static) -> Result<ReplImpl<EC>, Error> {
    let mut repl = ReplImpl::new(trace);
    repl.load_stdlib()?;
    Ok(repl)
}

/// Evaluate an input.
pub fn input<R: Repl>(repl: &mut R, line: &str) -> Result<InputResult, InputError> {
    if line.trim().is_empty() {
        Ok(InputResult::Blank)
    } else if line.starts_with(':') {
        let cmd = line.chars().skip(1).collect::<String>().parse::<Command>();
        match cmd {
            Ok(Command::Load(_)) => Err(InputError::Other(String::from(
                ":load is not enabled on this REPL.",
            ))),
            Ok(Command::Typecheck(exp)) => repl
                .typecheck(&exp)
                .map(|typ| InputResult::Success(format!("Ok: {typ}")))
                .map_err(InputError::from),
            Ok(Command::Query(path)) => repl
                .query(path)
                .map(|t| {
                    let mut buffer = Cursor::new(Vec::<u8>::new());
                    query_print::write_query_result(
                        &mut buffer,
                        &t,
                        query_print::Attributes::default(),
                    )
                    .unwrap();
                    InputResult::Success(String::from_utf8(buffer.into_inner()).unwrap())
                })
                .map_err(InputError::from),
            Ok(Command::Print(exp)) => repl
                .eval_full(&exp)
                .map(|res| match res {
                    EvalResult::Evaluated(rt) => InputResult::Success(format!("{rt}\n")),
                    EvalResult::Bound(_) => InputResult::Blank,
                })
                .map_err(InputError::from),
            Ok(Command::Help(arg)) => {
                let mut buffer = Cursor::new(Vec::<u8>::new());
                print_help(&mut buffer, arg.as_deref()).unwrap();
                Ok(InputResult::Success(
                    String::from_utf8(buffer.into_inner()).unwrap(),
                ))
            }
            Ok(Command::Exit) => Ok(InputResult::Success(String::from("Exiting"))),
            Err(err) => Err(InputError::from(Error::from(err))),
        }
    } else {
        repl.eval_full(line)
            .map(|eval_res| match eval_res {
                EvalResult::Evaluated(rt) => InputResult::Success(format!("{rt}\n")),
                EvalResult::Bound(_) => InputResult::Success(String::new()),
            })
            .map_err(InputError::from)
    }
}

/// Evaluate an input and serialize the result.
pub fn serialize<R: Repl>(
    repl: &mut R,
    format: ExportFormat,
    input: &str,
) -> Result<InputResult, InputError> {
    repl.eval_full(input)
        .and_then(|eval_res| match eval_res {
            EvalResult::Evaluated(t) => serialize::to_string(format, &t)
                .map(InputResult::Success)
                .map_err(Error::from),
            EvalResult::Bound(_) => Ok(InputResult::Success(String::new())),
        })
        .map_err(InputError::from)
}
