use std::io::Cursor;

use nickel_lang::{
    error::{Error, SerializationError},
    eval::cache::incremental::IncCache,
    program::Program,
    serialize,
};

use pyo3::{create_exception, exceptions::PyException, prelude::*};

create_exception!(pyckel, NickelException, PyException);

// see https://pyo3.rs/v0.17.3/function/error_handling.html#foreign-rust-error-types
struct NickelError(Error);
struct NickelSerializationError(SerializationError);

impl From<Error> for NickelError {
    fn from(value: Error) -> Self {
        Self(value)
    }
}

impl std::convert::From<NickelError> for PyErr {
    fn from(err: NickelError) -> PyErr {
        match err {
            // TODO better exceptions
            NickelError(error) => NickelException::new_err(format!("{error:?}")),
        }
    }
}

impl From<SerializationError> for NickelSerializationError {
    fn from(value: SerializationError) -> Self {
        Self(value)
    }
}

impl std::convert::From<NickelSerializationError> for PyErr {
    fn from(err: NickelSerializationError) -> PyErr {
        match err {
            // TODO better exceptions
            NickelSerializationError(error) => NickelException::new_err(format!("{error:?}")),
        }
    }
}

/// Evaluate from a Python str of a Nickel expression to a Python str of the resulting JSON.
#[pyfunction]
pub fn run(s: String) -> PyResult<String> {
    let mut program: Program<IncCache> =
        Program::new_from_source(Cursor::new(s.to_string()), "python")?;

    let term = program.eval_full().map_err(NickelError)?;

    serialize::validate(serialize::ExportFormat::Json, &term).map_err(NickelSerializationError)?;

    let json_string = serialize::to_string(serialize::ExportFormat::Json, &term)
        .map_err(NickelSerializationError)?;

    Ok(json_string)
}

#[pymodule]
pub fn pyckel(_py: Python<'_>, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(run, m)?)?;
    Ok(())
}
