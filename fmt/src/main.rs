//! This is a tiny version of "nickel format --check" that just uses
//! Topiary and doesn't depend on any other Nickel crates.
//!
//! The point here is that in the devShell and the CI, we want to
//! check formatting with the same formatter that's in the current
//! nickel binary. That's why we don't just use a Topiary from
//! nixpkgs or elsewhere. But on the other hand, building a nickel
//! binary just for the devShell is slow (and needs to be rebuilt
//! regularly, if you're actually working on Nickel in your devShell).
//!
//! This little binary does just enough for devShell and CI, while
//! changing infrequently and building fairly quickly.
use std::io::Cursor;

use topiary_core::{Language, Operation, TopiaryQuery, formatter};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<_> = std::env::args_os().collect();

    let grammar = tree_sitter_nickel::LANGUAGE.into();
    let query = TopiaryQuery::new(&grammar, topiary_queries::nickel())?;
    let language = Language {
        name: "nickel".to_owned(),
        query,
        grammar,
        indent: None,
    };
    for path in args.into_iter().skip(1) {
        eprintln!("checking path {}", path.display());
        let input = std::fs::read(&path)?;
        let mut output = Vec::new();

        formatter(
            &mut Cursor::new(&input),
            &mut output,
            &language,
            Operation::Format {
                // We're checkint whether the input equals the output.
                skip_idempotence: true,
                tolerate_parsing_errors: false,
            },
        )?;

        if input != output {
            panic!("file {} failed format checks", path.display());
        }
    }
    Ok(())
}
