//! Global state for the nickel CLI.

use std::{
    collections::HashSet,
    sync::mpsc::{Receiver, Sender, channel},
};

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
    pub errors: Receiver<Error>,
    /// Collects all emitted warnings.
    pub warnings: Receiver<Warning>,
    /// Contains the sending ends of the `errors` and `warnings` channels. This can be
    /// cloned and passed to anything that needs to send errors.
    pub reporter: CliReporter,
}

/// An implementation of [`nickel_lang_core::error::Reporter`] that makes its errors
/// available on the [`GlobalContext`].
///
/// We use `mpsc` channels, not because we want to send errors across threads but so that
/// we can decouple the lifetimes between the sending and receiving ends.
#[derive(Clone)]
pub struct CliReporter {
    errors: Sender<Error>,
    warnings: Sender<Warning>,
}

impl GlobalContext {
    pub fn new(opts: GlobalOptions) -> Self {
        let (errors_tx, errors) = channel();
        let (warnings_tx, warnings) = channel();
        GlobalContext {
            opts,
            errors,
            warnings,
            reporter: CliReporter {
                errors: errors_tx,
                warnings: warnings_tx,
            },
        }
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
                self.reporter.report(error);
                None
            }
            Err(PrepareError::EarlyReturn) => None,
            Ok(x) => Some(x),
        }
    }

    /// Drains all the warnings that have been received so far and deduplicates them.
    pub fn deduplicated_warnings(&mut self) -> Vec<Warning> {
        let mut seen: HashSet<Warning> = HashSet::new();
        let mut ret = Vec::new();
        for w in self.warnings.try_iter() {
            if seen.insert(w.clone()) {
                ret.push(w);
            }
        }
        ret
    }
}

impl CliReporter {
    // This is just an alias for the trait method, but having an inherent impl
    // makes type inference better because rust defaults to the inherit impl and
    // doesn't get confused between `Warning` and `Error`.
    pub fn report_result<T, E: Into<Error>>(&mut self, result: Result<T, E>) {
        <Self as Reporter<Error>>::report_result(self, result)
    }
}

impl Reporter<Error> for CliReporter {
    fn report(&mut self, e: Error) {
        // Ignore any errors, because if no one is listening for these diagnostics
        // then they aren't needed.
        let _ = self.errors.send(e);
    }
}

impl Reporter<Warning> for CliReporter {
    fn report(&mut self, w: Warning) {
        // Ignore any errors, because if no one is listening for these diagnostics
        // then they aren't needed.
        let _ = self.warnings.send(w);
    }
}

impl Reporter<(nickel_lang_core::error::Warning, Files)> for CliReporter {
    fn report(&mut self, (warning, files): (nickel_lang_core::error::Warning, Files)) {
        // Ignore any errors, because if no one is listening for these diagnostics
        // then they aren't needed.
        let _ = self.warnings.send(Warning::Program { warning, files });
    }
}
