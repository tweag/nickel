use std::io::Cursor;

use crate::{
    error::{Error, SerializationError},
    eval::cache::CBNCache,
    program::Program,
    serialize,
};

use pyo3::{create_exception, exceptions::PyException, prelude::*};

create_exception!(nickel_lang, NickelException, PyException);

impl std::convert::From<Error> for PyErr {
    fn from(err: Error) -> PyErr {
        match err {
            // TODO better exceptions
            error => NickelException::new_err(format!("{error:?}")),
        }
    }
}

impl std::convert::From<SerializationError> for PyErr {
    fn from(err: SerializationError) -> PyErr {
        match err {
            // TODO better exceptions
            error => NickelException::new_err(format!("{error:?}")),
        }
    }
}

/// Evaluate from a Python str of a Nickel expression to a Python str of the resulting JSON.
#[pyfunction]
pub fn run(s: String) -> PyResult<String> {
    let mut program: Program<CBNCache> =
        Program::new_from_source(Cursor::new(s.to_string()), "python")?;
    let term = program.eval_full()?;
    serialize::validate(serialize::ExportFormat::Json, &term)?;
    let json_string = serialize::to_string(serialize::ExportFormat::Json, &term)?;

    Ok(json_string)
}

#[pymodule]
pub fn nickel_lang(_py: Python<'_>, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(run, m)?)?;
    Ok(())
}
