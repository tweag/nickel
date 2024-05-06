use std::ops::Range;

use codespan::ByteIndex;
use nickel_lang_core::{
    position::TermPos,
    term::{RichTerm, Term, Traverse, TraverseControl},
};

use crate::{identifier::LocIdent, pattern::Bindings, term::RichTermPtr};

/// Turn a collection of "nested" ranges into a collection of disjoint ranges.
///
/// By "nested", I mean that if any two ranges intersect, then one of them contains the other.
/// This means that we can think of them as forming a tree, where parents contain the children.
/// Each of the input ranges is associated with a value. In determining the value for the output
/// ranges, the smallest ranges "win". For example, the input
///
/// ```text
/// [
///  [0, 10) -> a,
///  [2, 4) -> b,
///  [6, 8) -> c,
/// ]
/// ```
///
/// produces the output
///
/// ```text
/// [
///  [0, 2) -> a,
///  [2, 4) -> b,
///  [4, 6) -> a,
///  [6, 8) -> c,
///  [8, 10) -> a,
/// ]
/// ```
///
/// Or, more graphically,
///
/// ```text
/// aaaaaaaaaa   --->   aabbaaccaa
///   bb  cc
/// ```
///
/// The algorithm traverses the inputs as if it were a tree and accumulates the outputs in order:
/// we first traverse the "a" node and add the interval [0, 2) to the output. Then we go down to
/// the "b" child and add its interval to the output. Then we return to its parent ("a") and add
/// the part before the next child, and so on.
fn make_disjoint<T: Clone>(mut all_ranges: Vec<(Range<u32>, T)>) -> Vec<(Range<u32>, T)> {
    // This sort order corresponds to pre-order traversal of the tree.
    all_ranges.sort_by_key(|(range, _term)| (range.start, std::cmp::Reverse(range.end)));
    let mut all_ranges = all_ranges.into_iter().peekable();

    // `stack` is the path in the tree leading to the node that we are currently visiting.
    let mut stack = Vec::new();
    let mut next = all_ranges.next();

    let mut disjoint = Vec::new();
    // The last position we've added to `disjoint`. Gets bumped automatically every time we
    // push a new range.
    let mut pos = next
        .as_ref()
        .map(|(range, _term)| range.start)
        .unwrap_or_default();
    // We accumulate ranges using this closure, which guarantees that they are non-overlapping.
    // Note that `disjoint` and `pos` are only modified in here.
    let mut push_range = |pos: &mut u32, end: u32, term| {
        debug_assert!(*pos <= end);
        if *pos < end {
            disjoint.push((Range { start: *pos, end }, term));
            *pos = end;
        }
    };

    while let Some((cur, term)) = next {
        // If the next interval overlaps us, then we must contain it and it is our child.
        // Otherwise, we are a leaf and it is a sibling or a cousin or something.
        let next_start = all_ranges.peek().map(|(r, _term)| r.start);
        match next_start {
            Some(next_start) if cur.end > next_start => {
                // It is our child.
                push_range(&mut pos, next_start, term.clone());
                stack.push((cur, term));
                next = all_ranges.next();
            }
            _ => {
                // We are a leaf.
                push_range(&mut pos, cur.end, term.clone());
                next = stack.pop().or_else(|| all_ranges.next());
            }
        }
    }

    disjoint
}

/// A lookup data structure, for looking up the term at a given position.
///
/// Overlapping positions are resolved in favor of the smaller one; i.e., lookups return the
/// most specific term for a given position.
#[derive(Default, Clone, Debug)]
pub struct PositionLookup {
    // The intervals here are sorted and disjoint.
    term_ranges: Vec<(Range<u32>, RichTermPtr)>,
    ident_ranges: Vec<(Range<u32>, LocIdent)>,
}

impl PositionLookup {
    /// Create a position lookup table for looking up subterms of `rt` based on their positions.
    pub fn new(rt: &RichTerm) -> Self {
        let mut all_term_ranges = Vec::new();
        let mut idents = Vec::new();
        let mut fun = |term: &RichTerm, _state: &()| {
            if let TermPos::Original(pos) = &term.pos {
                all_term_ranges.push((
                    Range {
                        start: pos.start.0,
                        end: pos.end.0,
                    },
                    RichTermPtr(term.clone()),
                ));
            }

            match term.as_ref() {
                Term::Fun(id, _) | Term::Let(id, _, _, _) => idents.push(*id),
                Term::FunPattern(pat, _) | Term::LetPattern(pat, _, _) => {
                    let ids = pat.bindings().into_iter().map(|(_path, id, _)| id);
                    idents.extend(ids);
                }
                Term::Var(id) => idents.push(*id),
                Term::Record(data) | Term::RecRecord(data, _, _) => {
                    idents.extend(data.fields.keys().cloned());
                }
                Term::Match(data) => {
                    let ids = data
                        .branches
                        .iter()
                        .flat_map(|branch| branch.pattern.bindings().into_iter())
                        .map(|(_path, id, _)| id);
                    idents.extend(ids);
                }
                _ => {}
            }
            TraverseControl::<(), ()>::Continue
        };

        rt.traverse_ref(&mut fun, &());

        let mut ident_ranges: Vec<_> = idents
            .into_iter()
            .filter_map(|id| {
                id.pos
                    .into_opt()
                    .map(|span| (span.start.0..span.end.0, id.into()))
            })
            .collect();
        // Ident ranges had better be disjoint, so we can just sort by the start position.
        ident_ranges.sort_by_key(|(range, _id)| range.start);
        ident_ranges.dedup();

        PositionLookup {
            term_ranges: make_disjoint(all_term_ranges),
            ident_ranges,
        }
    }

    /// Returns the most specific subterm enclosing the given location, if there is one.
    ///
    /// Note that some positions (for example, positions belonging to top-level comments)
    /// may not be enclosed by any term.
    pub fn get(&self, index: ByteIndex) -> Option<&RichTerm> {
        search(&self.term_ranges, index).map(|rt| &rt.0)
    }

    /// Returns the ident at the given position, if there is one.
    pub fn get_ident(&self, index: ByteIndex) -> Option<LocIdent> {
        search(&self.ident_ranges, index).cloned()
    }
}

fn search<T>(vec: &[(Range<u32>, T)], index: ByteIndex) -> Option<&T> {
    vec.binary_search_by(|(range, _payload)| {
        if range.end <= index.0 {
            std::cmp::Ordering::Less
        } else if range.start > index.0 {
            std::cmp::Ordering::Greater
        } else {
            std::cmp::Ordering::Equal
        }
    })
    .ok()
    .map(|idx| &vec[idx].1)
}

#[cfg(test)]
pub(crate) mod tests {
    use assert_matches::assert_matches;
    use codespan::{ByteIndex, FileId, Files};
    use nickel_lang_core::{
        parser::{grammar, lexer, ErrorTolerantParser},
        term::{RichTerm, Term, UnaryOp},
    };

    use super::PositionLookup;

    pub fn parse(s: &str) -> (FileId, RichTerm) {
        let id = Files::new().add("<test>", String::from(s));

        let term = grammar::TermParser::new()
            .parse_strict(id, lexer::Lexer::new(s))
            .unwrap();
        (id, term)
    }

    #[test]
    fn find_pos() {
        let (_, rt) = parse("let x = { y = 1 } in x.y");
        let table = PositionLookup::new(&rt);

        // Index 14 points to the 1 in { y = 1 }
        let term_1 = table.get(ByteIndex(14)).unwrap();
        assert_matches!(term_1.term.as_ref(), Term::Num(..));

        // Index 23 points to the y in x.y
        let term_y = table.get(ByteIndex(23)).unwrap();
        assert_matches!(term_y.term.as_ref(), Term::Op1(UnaryOp::StaticAccess(_), _));

        // Index 21 points to the x in x.y
        let term_x = table.get(ByteIndex(21)).unwrap();
        assert_matches!(term_x.term.as_ref(), Term::Var(_));

        // This case has some mutual recursion between types and terms, which hit a bug in our
        // initial version.
        let (_, rt) = parse(
            "{ range_step\
                | std.contract.unstable.RangeFun (std.contract.unstable.RangeStep -> Dyn)\
                = fun a b c => []\
            }",
        );
        let table = PositionLookup::new(&rt);
        assert_matches!(
            table.get(ByteIndex(18)).unwrap().term.as_ref(),
            Term::Op1(UnaryOp::StaticAccess(_), _)
        );

        // This case has some mutual recursion between types and terms, which hit a bug in our
        // initial version.
        let (_, rt) = parse("let x | { _ : { foo : Number | default = 1 } } = {} in x.PATH.y");
        let table = PositionLookup::new(&rt);
        assert_matches!(
            table.get(ByteIndex(8)).unwrap().term.as_ref(),
            // Offset 8 actually points at the Dict, but that's a type and we only look up terms.
            // So it returns the enclosing let.
            Term::Let(..)
        );
        assert_matches!(
            table.get(ByteIndex(14)).unwrap().term.as_ref(),
            Term::RecRecord(..)
        );
    }
}
