use std::ops::{Deref, DerefMut};

use malachite::Rational;
use serde::{Deserialize, Serialize};
use unicode_segmentation::UnicodeSegmentation;

use super::{array::Array, Number, Term};

/// A Nickel string is really just a Rust `String`, overlayed with some
/// methods implementing custom logic (in particular, functions which
/// avoid ever breaking up Unicode extended grapheme clusters.)
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct NickelString(String);

// Conversions to & from String-like things.

impl<S> From<S> for NickelString
where
    String: From<S>,
{
    fn from(inner: S) -> Self {
        Self(inner.into())
    }
}

impl From<&NickelString> for String {
    fn from(s: &NickelString) -> Self {
        s.clone().into_inner()
    }
}

// The below impls broadly allow `NclString`s to be treated just like
// Rust `String`s.

impl Deref for NickelString {
    type Target = String;

    fn deref(&self) -> &String {
        &self.0
    }
}

impl DerefMut for NickelString {
    fn deref_mut(&mut self) -> &mut String {
        &mut self.0
    }
}

impl std::fmt::Display for NickelString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl AsRef<str> for NickelString {
    fn as_ref(&self) -> &str {
        self.0.as_str()
    }
}

impl NickelString {
    /// Creates a new empty `NclString`. Which is to say, it creates a new,
    /// empty `String`.
    pub fn new() -> NickelString {
        String::new().into()
    }

    /// The number of Unicode extended grapheme clusters the string contains.
    ///
    /// This method has `O(self.len())` time complexity.
    pub fn length(&self) -> usize {
        self.grapheme_cluster_count()
    }

    /// Returns the number of Unicode extended grapheme clusters the string
    /// contains.
    ///
    /// This method has `O(self.len())` time complexity.
    #[inline]
    fn grapheme_cluster_count(&self) -> usize {
        self.graphemes(true).count()
    }

    /// Returns an [`Array`] of Nickel strings, each one
    /// containing a single Unicode extended grapheme cluster.
    ///
    /// This method has `O(self.len())` time complexity.
    pub fn characters(&self) -> Array {
        self.grapheme_clusters()
    }

    /// Returns an [`Array`] of Nickel strings, each one
    /// containing a single Unicode extended grapheme cluster.
    ///
    /// This method has `O(self.len())` time complexity.
    #[inline]
    fn grapheme_clusters(&self) -> Array {
        self.graphemes(true)
            .map(|g| Term::Str(g.into()).into())
            .collect()
    }

    /// Splits the string on `separator`, returning an [`Array`] of Nickel
    /// strings.
    ///
    /// This method has `O(self.len() * separator.len())` time complexity when
    /// the separator is non-empty, or `O(self.len())` otherwise.
    pub fn split(&self, separator: &str) -> Array {
        if separator.is_empty() {
            // If there's no separator, we split the string between
            // each grapheme cluster.
            self.grapheme_clusters()
        } else {
            use grapheme_cluster_preservation::SearchEvent;

            let mut result = Vec::new();

            // We build our result vec by searching through the string for the separator.
            grapheme_cluster_preservation::search(self, separator).for_each(|e| match e {
                SearchEvent::Match {
                    // If we hit a match, then everything we saw between the previous
                    // match and now is a split...
                    since_last_match: split,
                } // ...then we do the same with whatever's left at the end.
                | SearchEvent::LastNonMatch { non_match: split } => {
                    result.push(Term::Str(split.into()).into())
                }
            });

            Array::from_iter(result.into_iter())
        }
    }

    /// Returns `true` if `needle` is contained in `self`, and `false` otherwise.
    ///
    /// Note that in contrast to Rust `String`'s `contains` method, this method
    /// will not consider a Unicode codepoint to be contained in a string if
    /// it exists as part of a larger extended grapheme cluster.
    ///
    /// The time complexity of this method is `O(self.len() * needle.len())`.
    pub fn contains(&self, needle: &str) -> bool {
        if needle.is_empty() {
            true
        } else {
            use grapheme_cluster_preservation::SearchEvent;

            grapheme_cluster_preservation::search(self, needle)
                .any(|e| matches!(e, SearchEvent::Match { .. }))
        }
    }

    /// Returns a new `NclString` replacing every occurence of `from` with `to`.
    ///
    /// This method has time complexity `O(self.len() * from.len())`.
    pub fn replace(&self, from: &str, to: &str) -> NickelString {
        let mut result = String::with_capacity(self.capacity());

        if from.is_empty() {
            // If `from` is empty then we:
            //   1. insert `to` at the beginning.
            result.push_str(to);
            //   2. insert `to` after each cluster.
            self.graphemes(true)
                .flat_map(|grapheme| [grapheme, to])
                .for_each(|s| result.push_str(s))
        } else {
            use grapheme_cluster_preservation::SearchEvent;

            grapheme_cluster_preservation::search(self, from).for_each(|e| match e {
                // If we hit a match...
                SearchEvent::Match { since_last_match } => {
                    // ...we write everything we've seen since the last match...
                    result.push_str(since_last_match);
                    // ...and then we replace the matched `from` with `to`.
                    result.push_str(to);
                }
                // We also need to write anything remaining after the last match.
                SearchEvent::LastNonMatch { non_match } => result.push_str(non_match),
            });
        }

        result.into()
    }

    /// Returns the substring of `self` between `start` and `end`.
    ///
    /// Returns an error if:
    ///    - either start or end is not a fraction
    ///    - start < 0
    ///    - start > end
    ///
    /// The time complexity of this method is `O(self.len())`.
    pub fn substring(&self, start: &Number, end: &Number) -> Result<NickelString, SubstringError> {
        let Ok(start_usize) = usize::try_from(start) else {
            return Err(SubstringError::NonIntStart(start.clone()));
        };

        let Ok(end_usize) = usize::try_from(end) else {
            return Err(SubstringError::NonIntEnd(end.clone()));
        };

        let mut from_start = self.graphemes(true).skip(start_usize).peekable();

        // If `from_start` is `None` then we skipped past the end of `self`.
        if from_start.peek().is_none() {
            Err(SubstringError::StartOutOfBounds {
                start: start.clone(),
                str_len: self.grapheme_cluster_count(),
            })
        } else if end_usize < start_usize {
            Err(SubstringError::EndOutOfBounds {
                start: start.clone(),
                end: end.clone(),
                str_len: self.grapheme_cluster_count(),
            })
        } else {
            let wanted_substr_len = end_usize - start_usize;
            let substr: String = from_start.take(wanted_substr_len).collect();
            let substr: NickelString = substr.into();
            // If `substr.grapheme_cluster_count()` is smaller than we wanted
            // then end was larger than the length of the string.
            if substr.grapheme_cluster_count() != wanted_substr_len {
                Err(SubstringError::EndOutOfBounds {
                    start: start.clone(),
                    end: end.clone(),
                    str_len: self.grapheme_cluster_count(),
                })
            } else {
                Ok(substr)
            }
        }
    }

    /// Consumes `self`, returning the Rust `String`.
    pub fn into_inner(self) -> String {
        self.0
    }
}

/// Errors returned by `NclString`'s `substring` method.
pub enum SubstringError {
    /// The start index was not an int
    NonIntStart(Rational),
    /// The end index was not an int
    NonIntEnd(Rational),
    /// The start index was not within the bounds of the string
    StartOutOfBounds { start: Rational, str_len: usize },
    EndOutOfBounds {
        start: Rational,
        end: Rational,
        str_len: usize,
    },
}

impl std::fmt::Display for SubstringError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use SubstringError::*;

        write!(f, "substring: ")?;

        match self {
            NonIntStart(start) => write!(
                f,
                "expected 2nd argument (start) to be a positive integer smaller than {}, got {start}",
                usize::MAX
            ),
            NonIntEnd(end) => write!(
                f,
                "expected 3rd argument (end) to be a positive integer smaller than {}, got {end}",
                usize::MAX
            ),
            StartOutOfBounds { start, str_len } => write!(
                f,
                "index out of bounds. \
                 Expected 2nd argument (start) to be between 0 and {str_len}, got {start}"
            ),
            EndOutOfBounds {
                start,
                end,
                str_len,
            } => write!(
                f,
                "index out of bounds. \
                Expected 3rd argument (end) to be between {start} and {str_len}, got {end}"
            ),
        }
    }
}

/// Types and functions designed to make it as easy as possible not to accidentally
/// break up Unicode extended grapheme clusters in string operations.
mod grapheme_cluster_preservation {
    use unicode_segmentation::GraphemeCursor;

    /// Returns a [`SearchIter`], which is an `Iterator` for iterating through
    /// `haystack` in order to find `needle`.
    ///
    /// [`SearchIter`] produces [`SearchEvent`]s which can be used to
    /// implement a variety of different behaviours.
    pub fn search<'a>(haystack: &'a str, needle: &'a str) -> SearchIter<'a> {
        SearchIter {
            haystack,
            needle,
            cursor: GraphemeCursor::new(0, haystack.len(), true),
            current_split_start: 0,
            last_boundary: 0,
        }
    }

    /// An event emitted by [`SearchIter`].
    pub enum SearchEvent<'a> {
        /// The `needle` was found within `haystack`.
        /// Since the caller already has a reference to `needle`, we don't
        /// include it in this event. However, we do include every part of
        /// `haystack` which was traversed up until the match happened.
        Match {
            /// The slice of the string from the end of the last match
            /// up to this one.
            since_last_match: &'a str,
        },
        /// Iteration through `haystack` has finished, but there was
        /// an unconsumed (non-matching) slice which the caller may wish
        /// to do something with.
        LastNonMatch {
            /// Any non-matching text from the end of the string.
            non_match: &'a str,
        },
    }

    /// Our base algorithm for implementing a variety of grapheme cluster
    /// preserving methods on [`NclString`]. As the name suggests, it is an
    /// `Iterator`. However, rather than iterating through grapheme clusters
    /// directly, its `Item` is a [`SearchEvent`].
    pub struct SearchIter<'a> {
        haystack: &'a str,
        needle: &'a str,
        cursor: GraphemeCursor,
        current_split_start: usize,
        last_boundary: usize,
    }

    impl<'a> Iterator for SearchIter<'a> {
        type Item = SearchEvent<'a>;

        fn next(&mut self) -> Option<Self::Item> {
            use indoc::indoc;

            let unwrap_explanation = indoc! {"
                None of the GraphemeIncomplete errors are possible here:
                    - PreContext and PrevChunk only happen if chunk_start is nonzero.
                    - NextChunk only happens if the chunk is smaller than the cursor's len parameter
                      but we pass `haystack` and `hackstack.len()`` respectively.
                    - InvalidOffset can't happen because we check that `haystack` contains `needle`
                      in the range (last_boundary, last_boundary + needle.len())
            "};

            loop {
                let nxt = self
                    .cursor
                    .next_boundary(self.haystack, 0)
                    .expect(unwrap_explanation);
                match nxt {
                    Some(next_boundary) => {
                        // To check whether a match has occured, we'll first check whether
                        // the slice of `haystack` starting at the `last_boundary` begins
                        // with `needle`. If it does, we *also* need to check that this
                        // instance of `needle` doesn't end in the middle of a cluster.
                        // This is to avoid the situtaion where we have a string like
                        // `"a🧑‍🤝‍🧑"` and we get a match by searching for `"a🧑"`.
                        let does_match_intersect_cluster = || {
                            let mut tmp_cursor = self.cursor.clone();
                            tmp_cursor.set_cursor(self.last_boundary + self.needle.len());
                            !tmp_cursor
                                .is_boundary(self.haystack, 0)
                                .expect(unwrap_explanation)
                        };

                        if self.haystack[self.last_boundary..].starts_with(self.needle)
                            && !does_match_intersect_cluster()
                        {
                            // Yay, we found a match! We start by grabbing the slice of
                            // the string we traversed while looking for the match.
                            let since_last_match =
                                &self.haystack[self.current_split_start..self.last_boundary];
                            // Now we move the cursor to after the match, to avoid needlessly
                            // traversing that part of the string again.
                            // Note that in particular this means that if we seach for "aa" in
                            // a string like "aaaa" we will get two matches: the first pair,
                            // and then the second.
                            let match_end = self.last_boundary + self.needle.len();
                            self.cursor.set_cursor(match_end);
                            self.current_split_start = match_end;
                            self.last_boundary = match_end;
                            // Finally we return the part of the string we traversed
                            // to get to this match back to the caller.
                            break Some(SearchEvent::Match { since_last_match });
                        } else {
                            // This is the only codepath which doesn't break
                            // the loop. This is because we didn't match `needle`
                            // so we're just going to move on to the next
                            // cluster boundary.
                            self.last_boundary = next_boundary;
                        }
                    }
                    None => {
                        // We finished iterating through the grapheme clusters, but
                        // there could still be a final chunk of the string that we
                        // haven't told the caller about yet.
                        if self.current_split_start < self.haystack.len() {
                            // We pass that slice back here & advance the `current_split_start`
                            // pointer to the end of the string to ensure we don't hit this
                            // condition again & will correctly return None on the next call.
                            let non_match = &self.haystack[self.current_split_start..];
                            self.current_split_start = self.haystack.len();
                            break Some(SearchEvent::LastNonMatch { non_match });
                        } else {
                            break None;
                        }
                    }
                }
            }
        }
    }
}
