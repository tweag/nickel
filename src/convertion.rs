//! This module contains a trait to implement on every thing you want to be convertible into nickel
//! AST.
//! It should be used mostly for AST to AST convertion (e.g.: nix to nickel). Effectively the
//! `translate` function require to pass a `codespan::FileId` so it's not wors to use it for
//! convertions which not imply a file/stream input. For these convertions, prefer `Into`/`From`
//! standart traits.

use crate::term::RichTerm;
use codespan::FileId;
use std::collections::HashSet;

pub struct State {
    file_id: FileId,
    env: HashSet<String>,
}

pub trait ToNickel {
    fn to_nickel(&self, file_id: FileId) -> RichTerm {
        let state = State {
            file_id,
            env: HashSet::new(),
        };
        self.translate(state)
    }
    fn translate(self, state: State) -> RichTerm;
}
