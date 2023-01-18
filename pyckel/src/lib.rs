use std::io::Cursor;

use nickel_lang::{eval::cache::CBNCache, program::Program, serialize};

use pyo3::{create_exception, exceptions::PyException, prelude::*};

create_exception!(pyckel, NickelException, PyException);

/// Evaluate from a Python str of a Nickel expression to a Python str of the resulting JSON.
#[pyfunction]
pub fn run(s: String) -> PyResult<String> {
    let mut program: Program<CBNCache> =
        Program::new_from_source(Cursor::new(s.to_string()), "python")?;

    let term = program
        .eval_full()
        .map_err(|err| NickelException::new_err(program.report_as_str(err)))?;

    serialize::validate(serialize::ExportFormat::Json, &term)
        .map_err(|err| NickelException::new_err(program.report_as_str(err)))?;

    let json_string = serialize::to_string(serialize::ExportFormat::Json, &term)
        .map_err(|err| NickelException::new_err(program.report_as_str(err)))?;

    Ok(json_string)
}

#[pymodule]
pub fn pyckel(_py: Python<'_>, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(run, m)?)?;
    Ok(())
}
