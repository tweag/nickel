//! Global state for the nickel CLI.

use std::collections::HashSet;

use nickel_lang_core::{
    error::Error as CoreError, error::Reporter, eval::cache::lazy::CBNCache, files::Files,
    program::Program,
};

use crate::{
    cli::GlobalOptions,
    error::{Error, Warning},
    input::{Prepare, PrepareError},
};

/// Contains global state for the nickel CLI.
pub struct GlobalContext {
    /// The CLI arguments that are applicable to all commands.
    pub opts: GlobalOptions,
    /// Collects all errors that occur. Currently, there is at most one error but that
    /// might change.
    pub errors: Vec<Error>,
    /// Collects all emitted warnings.
    pub warnings: Vec<Warning>,
}

impl GlobalContext {
    pub fn new(opts: GlobalOptions) -> Self {
        GlobalContext {
            opts,
            errors: Vec::new(),
            warnings: Vec::new(),
        }
    }

    // This is just an alias for the trait method, but having an inherent impl
    // makes type inference better because rust defaults to the inherit impl and
    // doesn't get confused between `Warning` and `Error`.
    pub fn report_result<T, E: Into<Error>>(&mut self, result: Result<T, E>) {
        <Self as Reporter<Error>>::report_result(self, result)
    }

    /// Creates a program and applies a callback to it.
    ///
    /// The program is run with this global context as its warning collector, so
    /// that all of the program's warnings will be available on the global context
    /// after the program has finished.
    ///
    /// If the callback returns an error, that error will also be available on the
    /// global context.
    pub fn with_program<
        T,
        P: Prepare,
        F: FnOnce(&mut Program<CBNCache>) -> Result<T, CoreError>,
    >(
        &mut self,
        preparer: &P,
        f: F,
    ) -> Option<T> {
        let result = match preparer.prepare(self) {
            Ok(mut prog) => f(&mut prog).map_err(|error| {
                Error::Program {
                    error,
                    files: prog.files(),
                }
                .into()
            }),
            Err(e) => Err(e),
        };

        match result {
            Err(PrepareError::Error(error)) => {
                self.report(error);
                None
            }
            Err(PrepareError::EarlyReturn) => None,
            Ok(x) => Some(x),
        }
    }

    pub fn deduplicate_warnings(&mut self) {
        let mut seen: HashSet<Warning> = HashSet::new();
        self.warnings.retain(move |w| seen.insert(w.clone()));
    }
}

impl Reporter<Error> for GlobalContext {
    fn report(&mut self, e: Error) {
        self.errors.push(e);
    }
}

impl Reporter<Warning> for GlobalContext {
    fn report(&mut self, w: Warning) {
        self.warnings.push(w);
    }
}

impl Reporter<(nickel_lang_core::error::Warning, Files)> for GlobalContext {
    fn report(&mut self, (warning, files): (nickel_lang_core::error::Warning, Files)) {
        self.warnings.push(Warning::Program { warning, files });
    }
}
