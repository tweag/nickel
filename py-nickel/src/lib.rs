use std::ffi::OsString;
use std::io::Cursor;

use nickel_lang_core::{
    error::{
        report::{report_as_str, ColorOpt},
        Error, NullReporter,
    },
    eval::cache::{Cache, CacheImpl},
    program::Program,
    serialize,
};

use pyo3::{create_exception, exceptions::PyException, prelude::*};

create_exception!(nickel, NickelException, PyException);

/// Turn an internal Nickel error into a PyErr with a fancy diagnostic message
fn error_to_exception<E: Into<Error>, EC: Cache>(error: E, program: &mut Program<EC>) -> PyErr {
    NickelException::new_err(report_as_str(
        &mut program.files(),
        error.into(),
        ColorOpt::default(),
    ))
}

/// Evaluate from a Python str of a Nickel expression to a Python str of the resulting JSON.
///
/// # Parameters
///
/// - `expr`: the Nickel expression to evaluate.
/// - `import_paths`: optional list of paths to search for imported files. In the Nickel
///   stand-alone binary, the import paths are controlled by the `NICKEL_IMPORT_PATH` environment
///   variable and the `--import-path` CLI argument. In the Python bindings, you need to provide
///   them explicitly instead.
#[pyfunction]
#[pyo3(signature = (expr, import_paths=None))]
pub fn run(expr: String, import_paths: Option<Vec<OsString>>) -> PyResult<String> {
    let mut program: Program<CacheImpl> = Program::new_from_source(
        Cursor::new(expr),
        "python",
        std::io::sink(),
        NullReporter {},
    )?;

    if let Some(import_paths) = import_paths {
        program.add_import_paths(import_paths.into_iter());
    }

    let term = program
        .eval_full()
        .map_err(|error| error_to_exception(error, &mut program))?;

    serialize::validate(serialize::ExportFormat::Json, &term)
        .map_err(|error| error_to_exception(error, &mut program))?;

    let json_string = serialize::to_string(serialize::ExportFormat::Json, &term)
        .map_err(|error| error_to_exception(error, &mut program))?;

    Ok(json_string)
}

#[pymodule]
pub fn nickel(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(run, m)?)?;
    Ok(())
}
