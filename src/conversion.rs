//! This module contains a trait to implement on every thing you want to be convertible into nickel
//! AST.
//! It should be used mostly for AST to AST convertion (e.g.: nix to nickel). Effectively the
//! `translate` function require to pass a `codespan::FileId` so it's not wors to use it for
//! convertions which not imply a file/stream input. For these convertions, prefer `Into`/`From`
//! standart traits.

use crate::term::RichTerm;
use codespan::FileId;
use std::collections::HashSet;

/// State of the conversion. It contains the definitions in scope of the currently converted node (e.g.:
/// `with` environments, declared variables, current file id...), required for elaborate compilation (`with`).
#[derive(Clone)]
pub struct State {
    /// The current transformation file ID.
    pub file_id: FileId,
    /// Variables in scope.
    pub env: HashSet<String>,
    /// With scope. List of the records on which has been applied a with in the current scope.
    pub with: Vec<RichTerm>,
}

pub trait ToNickel: Sized {
    /// Used when converting a full file. Actually call `translate` with an initial `State`.
    fn to_nickel(self, file_id: FileId) -> RichTerm {
        let state = State {
            file_id,
            env: HashSet::new(),
            with: Vec::new(),
        };
        self.translate(&state)
    }

    /// Use to convert an expression or a term of the source language to nickel.
    /// For a complet exemple, you can see `crate::nix`, the convertion from Nix to Nickel.
    fn translate(self, state: &State) -> RichTerm;
}
