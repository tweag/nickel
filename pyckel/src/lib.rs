use std::io::Cursor;

use nickel_lang_core::{
    error::Error,
    eval::cache::{Cache, CacheImpl},
    program::Program,
    serialize,
};

use pyo3::{create_exception, exceptions::PyException, prelude::*};

create_exception!(pyckel, NickelException, PyException);

/// Turn an internal Nickel error into a PyErr with a fancy diagnostic message
fn error_to_exception<E: Into<Error>, EC: Cache>(error: E, program: &mut Program<EC>) -> PyErr {
    NickelException::new_err(program.report_as_str(error.into()))
}

/// Evaluate from a Python str of a Nickel expression to a Python str of the resulting JSON.
#[pyfunction]
pub fn run(s: String) -> PyResult<String> {
    let mut program: Program<CacheImpl> =
        Program::new_from_source(Cursor::new(s), "python", std::io::sink())?;

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
pub fn pyckel(_py: Python<'_>, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(run, m)?)?;
    Ok(())
}
