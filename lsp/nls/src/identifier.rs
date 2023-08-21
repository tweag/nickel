use nickel_lang_core::{identifier::Ident, position::TermPos};

/// An identifier with a location.
///
/// This differs from [`nickel_lang_core::identifier::LocIdent`] in that the
/// location is used in comparisons and hashes.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct LocIdent {
    pub symbol: Ident,
    pub pos: TermPos,
}

impl From<LocIdent> for nickel_lang_core::identifier::LocIdent {
    fn from(ours: LocIdent) -> Self {
        Self::from(ours.symbol).with_pos(ours.pos)
    }
}

impl From<nickel_lang_core::identifier::LocIdent> for LocIdent {
    fn from(theirs: nickel_lang_core::identifier::LocIdent) -> Self {
        Self {
            symbol: theirs.symbol(),
            pos: theirs.pos,
        }
    }
}
