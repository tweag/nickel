//! Suggest existing symbols that are similar to what the user typed in case of various "symbol not
//! found" errors, such as [super::EvalError::UnboundIdentifier] or
//! [super::EvalError::FieldMissing].
//!
//! The current implementation uses the normalized Damereau-Levenshtein edit distance to find the
//! closest match.

use strsim::normalized_damerau_levenshtein;

/// The maximum distance between two strings for them to be considered similar and suggested to the
/// other.
pub const MIN_SIMILARITY: f64 = 0.25;

/// Find the closest match to the user input in the given list of symbols, to be used as a
/// suggestion. If there is an exact match, it is returned immediately. Otherwise, the symbol with
/// the highest normalized Damerau-Levenshtein similarity is returned, as long as the similarity is
/// above the threshold [MIN_SIMILARITY].
pub fn find_best_match<'syms, 'input, S, I>(
    symbols: &'syms [S],
    user_input: &'input I,
) -> Option<&'syms str>
where
    S: AsRef<str>,
    I: AsRef<str>,
{
    let mut min = std::f64::MAX;
    let mut arg_min: Option<&'syms str> = None;

    if symbols.is_empty() {
        return None;
    }

    for sym in symbols {
        // If there is an exact match (modulo case), return it immediately.
        if sym.as_ref().to_lowercase() == user_input.as_ref().to_lowercase() {
            return Some(sym.as_ref());
        }

        let similarity = normalized_damerau_levenshtein(sym.as_ref(), user_input.as_ref());

        if similarity < min && similarity > MIN_SIMILARITY {
            arg_min = Some(sym.as_ref());
            min = similarity;
        }
    }

    arg_min
}
