//! This module contains a trait to implement on every thing you want to be convertible into nickel
//! AST.
//! It should be used mostly for AST to AST convertion (e.g.: nix to nickel). Effectively the
//! `translate` function require to pass a `codespan::FileId` so it's not wors to use it for
//! convertions which not imply a file/stream input. For these convertions, prefer `Into`/`From`
//! standart traits.

use crate::term::RichTerm;
use codespan::FileId;

pub trait ToNickel {
    fn translate(self, file_id: FileId) -> RichTerm;
}
