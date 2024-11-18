use codespan_reporting::diagnostic::Diagnostic;

use crate::error::{primary, secondary, IntoDiagnostics};
use crate::files::{FileId, Files};
use crate::position::TermPos;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Warning {
    /// Applied a `fun label value => ...` (or match) as a contract directly,
    /// instead of using the constract constructors in the standard library.
    NakedFunctionContract {
        /// The position of the function that was used as a contract.
        func_pos: TermPos,
        /// The position of the thing that the contract was applied to.
        app_pos: TermPos,
    },
}

impl IntoDiagnostics for Warning {
    fn into_diagnostics(self, _files: &mut Files) -> Vec<Diagnostic<FileId>> {
        match self {
            Warning::NakedFunctionContract { func_pos, app_pos } => {
                let mut labels = vec![];
                if let Some(span) = app_pos.into_opt() {
                    labels.push(primary(&span).with_message("applied to this term"));
                }

                if let Some(func_span) = func_pos.into_opt() {
                    labels.push(secondary(&func_span).with_message("this function"));
                }

                vec![Diagnostic::warning()
                    .with_message("plain functions as contracts are deprecated")
                    .with_labels(labels)
                    .with_notes(vec!["wrap this function using one of the constructors in `std.contract` instead, like `std.contract.from_validator` or `std.contract.custom`".to_owned()])
                ]
            }
        }
    }
}
