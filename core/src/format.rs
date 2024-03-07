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
            skip_idempotence: false,
            tolerate_parsing_errors: false,
        },
    )
    .map_err(FormatError)
}
