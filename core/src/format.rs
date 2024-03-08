//! Utility module for formatting Nickel files using Topiary

use std::{
    fmt::Display,
    io::{Read, Write},
};

use topiary_core::{formatter, Language, Operation, TopiaryQuery};

#[derive(Debug)]
/// Errors that may be encountered during formatting
pub struct FormatError(topiary_core::FormatterError);

impl Display for FormatError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

/// Format a Nickel file being read from `input`, writing the result to `output`.
pub fn format(mut input: impl Read, mut output: impl Write) -> Result<(), FormatError> {
    let grammar = tree_sitter_nickel::language().into();
    let query = TopiaryQuery::new(&grammar, topiary_queries::nickel()).map_err(FormatError)?;
    let language = Language {
        name: "nickel".to_owned(),
        query,
        grammar,
        indent: None,
    };

    formatter(
        &mut input,
        &mut output,
        &language,
        Operation::Format {
            // We only enable the idempotency check in debug mode: it's useful to detect bugs in
            // the Nickel formatter, but we don't want to report an error or to make production
            // users pay the cost of the check, although this cost should be fairly low.
            skip_idempotence: !cfg!(debug_assertions),
            tolerate_parsing_errors: false,
        },
    )
    .map_err(FormatError)
}
