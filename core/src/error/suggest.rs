//! Suggest existing symbols that are similar to what the user typed in case of various "symbol not
//! found" errors, such as [super::EvalError::UnboundIdentifier] or
//! [super::EvalError::FieldMissing].
//!
//! The current implementation uses the normalized Damereau-Levenshtein edit distance to find the
//! closest match.

use strsim::normalized_damerau_levenshtein;

/// The minimum similarity between the user's input and an existing symbol for the symbol to be
/// considered a serious candidate. The current threshold is rather low, because short words with
/// edit distance 1 such as `bar` and `bare` might have a similarity which isn't that high.
pub const MIN_SIMILARITY: f64 = 0.60;

/// Find the closest match to the user input in the given list of symbols, to be used as a
/// suggestion. If there is an exact match (modulo casing), it is returned immediately. Otherwise,
/// the symbol with the highest normalized Damerau-Levenshtein similarity is returned, as long as
/// the similarity is above the fixed threshold [MIN_SIMILARITY].
pub fn find_best_match<'syms, 'input, S, I>(
    symbols: &'syms [S],
    user_input: &'input I,
) -> Option<&'syms str>
where
    S: AsRef<str>,
    I: AsRef<str>,
{
    let mut max = std::f64::MIN;
    let mut arg_max: Option<&'syms str> = None;

    if symbols.is_empty() {
        return None;
    }

    for sym in symbols {
        // If there is an exact match (modulo case), return it immediately.
        if sym.as_ref().to_lowercase() == user_input.as_ref().to_lowercase() {
            return Some(sym.as_ref());
        }

        let similarity = normalized_damerau_levenshtein(sym.as_ref(), user_input.as_ref());

        if similarity > max && similarity >= MIN_SIMILARITY {
            arg_max = Some(sym.as_ref());
            max = similarity;
        }
    }

    arg_max
}

/// Take diagnostic notes and add a generic note suggesting existing symbols that are similar to
/// what the user typed thanks to [find_best_match], if any. If there is no match, `notes` is left
/// unchanged.
pub fn add_suggestion<S, I>(notes: &mut Vec<String>, symbols: &[S], user_input: &I)
where
    S: AsRef<str>,
    I: AsRef<str>,
{
    if let Some(best_match) = find_best_match(symbols, user_input) {
        notes.push(format!("Did you mean `{best_match}`?"));
    }
}
