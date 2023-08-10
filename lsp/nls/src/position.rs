use codespan::ByteIndex;
use nickel_lang_core::{
    position::TermPos,
    term::{RichTerm, SharedTerm, Traverse, TraverseControl},
};
use range_map::{Range, RangeMap};

// A term that uses pointer equality to implement Eq.
#[derive(Clone, Debug)]
struct RichTermPtr(RichTerm);

impl PartialEq for RichTermPtr {
    fn eq(&self, other: &Self) -> bool {
        self.0.pos == other.0.pos && SharedTerm::ptr_eq(&self.0.term, &other.0.term)
    }
}

impl Eq for RichTermPtr {}

/// A lookup data structure, for looking up the term at a given position.
///
/// Overlapping positions are resolved in favor of the smaller one; i.e., lookups return the
/// most specific term for a given position.
#[derive(Clone, Debug)]
pub struct PositionLookup {
    table: RangeMap<u32, RichTermPtr>,
}

impl PositionLookup {
    /// Create a position lookup table for looking up subterms of `rt` based on their positions.
    pub fn new(rt: &RichTerm) -> Self {
        let mut all_ranges = Vec::new();

        rt.traverse_ref(&mut |term: &RichTerm| {
            if let TermPos::Original(pos) = &term.pos {
                all_ranges.push((pos.start.0, pos.end.0, term.clone()));
            }
            TraverseControl::<()>::Continue
        });

        // We rely on the invariant that if two ranges overlap then one is contained in the
        // other. That is, the ranges form a tree. This sort order corresponds to pre-order
        // traversal of that tree.
        //
        // (It would be nice if we could build the lookup table directly from the traversal
        // in traverse_ref, but (1) the state tracking is fiddly and (2) the traversal order
        // in traverse_ref can be weird; for example, a `Term::Annotated` sometimes contains
        // children whose position is outside the position of the parent.)
        all_ranges.sort_by_key(|range| (range.0, std::cmp::Reverse(range.1)));
        let mut all_ranges = all_ranges.into_iter().peekable();

        // Now we iterate over the tree of ranges to make a disjoint set of ranges.
        // `stack` is the path in the tree leading to the node that we are currently visiting.
        let mut stack = Vec::new();
        let mut disjoint = Vec::new();
        let mut maybe_cur = all_ranges.next();
        let mut pos = maybe_cur.as_ref().map(|range| range.0).unwrap_or_default();

        // We accumulate ranges using this closure, which guarantees that they are non-overlapping.
        // (This is important: RangeMap panics if they are not.)
        let mut push_range = |end: u32, term: RichTerm| {
            if pos < end {
                // range_map::Range has inclusive ends for some reason.
                // This is overflow-safe because pos < end
                disjoint.push((Range::new(pos, end - 1), RichTermPtr(term)));
                pos = end;
            }
        };

        while let Some(cur) = maybe_cur {
            // If the next interval overlaps us, then we must contain it and it is our child.
            // Otherwise, we are a leaf and it is a sibling or a cousin or something.
            let next_start = all_ranges.peek().map(|r| r.0);
            if let (Some(next_start), Some(true)) = (next_start, next_start.map(|x| cur.1 > x)) {
                // It is our child.
                push_range(next_start, cur.2.clone());
                stack.push(cur);
                maybe_cur = all_ranges.next();
            } else {
                // It is a sibling/cousin
                push_range(cur.1, cur.2.clone());
                maybe_cur = stack.pop().or_else(|| all_ranges.next());
            }
        }

        PositionLookup {
            table: disjoint.into_iter().collect(),
        }
    }

    /// Returns the subterm at the given location, if there is one.
    pub fn get(&self, index: ByteIndex) -> Option<&RichTerm> {
        self.table.get(index.0).map(|rt| &rt.0)
    }
}

#[cfg(test)]
mod tests {
    use assert_matches::assert_matches;
    use codespan::{ByteIndex, Files};
    use nickel_lang_core::{
        parser::{grammar, lexer, ErrorTolerantParser},
        term::{RichTerm, Term, UnaryOp},
    };

    use super::PositionLookup;

    fn parse(s: &str) -> RichTerm {
        let id = Files::new().add("<test>", String::from(s));

        grammar::TermParser::new()
            .parse_strict(id, lexer::Lexer::new(s))
            .unwrap()
    }

    #[test]
    fn find_pos() {
        let rt = parse("let x = { y = 1 } in x.y");
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
        let rt = parse(
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
        let rt = parse("let x | { _ : { foo : Number | default = 1 } } = {} in x.PATH.y");
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
