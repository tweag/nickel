use std::io::Cursor;

use crate::{error::Error, eval::cache::CBNCache, program::Program};

use pyo3::{create_exception, exceptions::PyException, prelude::*};

create_exception!(nickel_lang, NickelException, PyException);

impl std::convert::From<Error> for PyErr {
    fn from(err: Error) -> PyErr {
        match err {
            // TODO better exceptions
            error => NickelException::new_err(format!("unexpected error: {error:?}")),
        }
    }
}

// Some test function
#[pyfunction]
pub fn run(s: String) -> PyResult<String> {
    let mut program: Program<CBNCache> =
        Program::new_from_source(Cursor::new(s.to_string()), "python")?;
    let term = program.eval()?;
    Ok(term.to_string())
}

#[pymodule]
pub fn nickel_lang(_py: Python<'_>, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(run, m)?)?;
    Ok(())
}
