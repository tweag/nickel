//! Utility module for formatting Nickel files using Topiary

use std::{
    fmt::Display,
    io::{Read, Write},
};

use topiary::TopiaryQuery;

#[derive(Debug)]
/// Errors that may be encountered during formatting
pub struct FormatError(topiary::FormatterError);

impl Display for FormatError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

/// Format a Nickel file being read from `input`, writing the result to `output`.
pub fn format(mut input: impl Read, mut output: impl Write) -> Result<(), FormatError> {
    let topiary_config =
        topiary::Configuration::parse_default_configuration().map_err(FormatError)?;
    let language = topiary::SupportedLanguage::Nickel.to_language(&topiary_config);
    let grammar = tree_sitter_nickel::language().into();
    let query = TopiaryQuery::new(&grammar, topiary_queries::nickel()).map_err(FormatError)?;
    topiary::formatter(
        &mut input,
        &mut output,
        &query,
        language,
        &grammar,
        topiary::Operation::Format {
            skip_idempotence: true,
            tolerate_parsing_errors: false,
        },
    )
    .map_err(FormatError)
}
