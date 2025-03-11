use std::ops::Range;

use codespan::ByteIndex;
use nickel_lang_core::{
    bytecode::ast::{pattern::bindings::Bindings as _, Ast, Node},
    position::{RawSpan, TermPos},
    traverse::{TraverseAlloc, TraverseControl},
};

use crate::{identifier::LocIdent, term::AstPtr};

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
pub struct PositionLookup<'ast> {
    // The intervals here are sorted and disjoint.
    ast_ranges: Vec<(Range<u32>, AstPtr<'ast>)>,
    ident_ranges: Vec<(Range<u32>, LocIdent)>,
}

impl<'ast> PositionLookup<'ast> {
    /// Create a position lookup table for looking up subterms of `rt` based on their positions.
    pub fn new(ast: &'ast Ast<'ast>) -> Self {
        let mut all_term_ranges = Vec::new();
        let mut idents = Vec::new();
        let mut fun = |ast: &'ast Ast<'ast>, _state: &()| {
            if let TermPos::Original(pos) = &ast.pos {
                all_term_ranges.push((
                    Range {
                        start: pos.start.0,
                        end: pos.end.0,
                    },
                    AstPtr(ast),
                ));
            }

            match &ast.node {
                Node::Fun { args, .. } => idents.extend(
                    args.iter()
                        .flat_map(|arg| arg.bindings())
                        .map(|pat_bdg| pat_bdg.id),
                ),
                Node::Let { bindings, .. } => idents.extend(
                    bindings
                        .iter()
                        .flat_map(|bdg| bdg.pattern.bindings())
                        .map(|pat_bdg| pat_bdg.id),
                ),
                Node::Var(id) => idents.push(*id),
                Node::Record(data) => idents.extend(data.toplvl_stat_fields()),
                Node::Match(data) => {
                    let ids = data
                        .branches
                        .iter()
                        .flat_map(|branch| branch.pattern.bindings().into_iter())
                        .map(|bdg| bdg.id);
                    idents.extend(ids);
                }
                _ => {}
            }
            TraverseControl::<(), ()>::Continue
        };

        ast.traverse_ref(&mut fun, &());

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
            ast_ranges: make_disjoint(all_term_ranges),
            ident_ranges,
        }
    }

    /// Returns the most specific subterm enclosing the given location, if there is one.
    ///
    /// Note that some positions (for example, positions belonging to top-level comments)
    /// may not be enclosed by any term.
    pub fn at(&self, index: ByteIndex) -> Option<&'ast Ast<'ast>> {
        find(&self.ast_ranges, index).map(|ptr| ptr.0)
    }

    // /// Returns the subterm at the given span, if there is one. This method doesn't try to find an
    // /// enclosing term, but look for a term that exactly covers the span.
    // ///
    // /// This function supports terms that are split into multiple entries in the lookup table: what
    // /// we do is to find the enclosing term at the start of the span, and then iterate until the
    // /// end to find an AST with the exact wanted span.
    // pub fn at_span_exact(&self, span: RawSpan) -> Option<&'ast Ast<'ast>> {
    //     let mut idx = find_index(&self.ast_ranges, span.start)?;
    //
    //     while idx < self.ast_ranges.len() {
    //         let (range, ast) = &self.ast_ranges[idx];
    //         let curr_span = ast.0.pos.into_opt()?;
    //
    //         // Since spans are disjoint in the table, we can break early if we're not strictly
    //         // within the searched span.
    //         if range.start < span.start.0 || range.end > span.end.0 {
    //             break;
    //         }
    //
    //         if curr_span.start == span.start && curr_span.end == span.end {
    //             return Some(ast.0);
    //         }
    //
    //         idx += 1;
    //     }
    //
    //     None
    // }

    /// Returns the ident at the given position, if there is one.
    pub fn get_ident(&self, index: ByteIndex) -> Option<LocIdent> {
        find(&self.ident_ranges, index).copied()
    }

    /// Refine a sub-expression of an AST (in practice, a parse error) as a more precise AST. For
    /// example, when handling completion on incomplete input, we might start from a string
    /// `foo.bar.`, which is initially a parse error at `[n, n+8]`. The completion engine will find
    /// out that `foo.bar` parses correctly. Thus we want to split the original interval into `[n,
    /// n+7]` for the new AST and `[n+7, n+8]` for the original parse error.
    ///
    /// This function does precisely that: it inserts the new AST pointer while maintaining the
    /// invariant of the position lookup table.
    ///
    /// # Preconditions
    ///
    /// - we require that the given range `span_err` is one atomic interval in the lookup table and
    ///   isn't split: that is, there exists an element `(span_err.start, span_err.end]` in the
    ///   table.
    /// - we require that the provided AST has a defined position:
    ///   `new_ast.pos.into_opt().is_some()` holds
    pub(crate) fn refine_ast(&mut self, span_err: RawSpan, new_ast: &'ast Ast<'ast>) {
        // unwrap(): we expect the error to be a single interval present in the table as
        // precondition of this method.
        let idx = find_index(&self.ast_ranges, span_err.start).unwrap();
        // unwrap(): we expect the position of `new_ast` to be defined as a pre-condition of this
        // method.
        let span_new_ast = new_ast.pos.unwrap();
        let (range_err, old_ast) = self.ast_ranges[idx].clone();

        // This is a non-debug assertion because violating this condition will break the structural
        // invariants of `self.ast_ranges` and might give in silently wrong position lookups.
        assert!(
            range_err.end == span_err.end.0
                && range_err.start <= span_new_ast.start.0
                && range_err.end >= span_new_ast.end.0
        );

        // The following decision tree is a bit redundant, but in return it avoid some work when we
        // only need to split the AST in two instead of three.

        // If they start at the same position, we can just replace the old range with the new one.
        if span_new_ast.start.0 == range_err.start {
            self.ast_ranges[idx] = (span_new_ast.to_range(), AstPtr(new_ast));

            // If the new AST ends before, we need to at the missing interval (in practice this is
            // always true, as successfully refining parse error should give a strictly smaller
            // range than the original unparsable term.
            if span_new_ast.end.0 < range_err.end {
                self.ast_ranges
                    .insert(idx + 1, (span_new_ast.end.0..range_err.end, old_ast));
            }
        }
        // If they end at the same position, it's the symmetric variant of the previous case.
        else if span_new_ast.end.0 == range_err.end {
            self.ast_ranges[idx] = (span_new_ast.to_range(), AstPtr(new_ast));

            // If the new AST starts after , we need to at the missing interval (in practice this
            // is always true, as successfully refining parse error should give a strictly smaller
            // range than the original unparsable term.
            if span_new_ast.start.0 > range_err.start {
                self.ast_ranges
                    .insert(idx, (range_err.start..span_new_ast.start.0, old_ast));
            }
        }
        // In the general case, we need to split the original interval in three: `(range_err.start,
        // span_new_ast.start]` and `(span_new_ast.end, range_err.end]` for the original parse
        // error and `(span_new_ast.start, span_new_ast.end]` for the new parse tree.
        //
        // We need to reconstruct a new vector by stitching the different pieces together.
        else {
            let new_ranges = self.ast_ranges[..idx]
                .iter()
                .cloned()
                .chain([
                    (range_err.start..span_new_ast.start.0, old_ast),
                    (span_new_ast.start.0..span_new_ast.end.0, AstPtr(new_ast)),
                    (span_new_ast.end.0..range_err.end, old_ast),
                ])
                .chain(self.ast_ranges[idx + 1..].iter().cloned());

            self.ast_ranges = new_ranges.collect();
        }
    }
}

fn find_index<T>(vec: &[(Range<u32>, T)], index: ByteIndex) -> Option<usize> {
    vec.binary_search_by(|(range, _payload)| {
        let result = if range.end <= index.0 {
            std::cmp::Ordering::Less
        } else if range.start > index.0 {
            std::cmp::Ordering::Greater
        } else {
            std::cmp::Ordering::Equal
        };

        result
    })
    .ok()
}

fn find<T>(vec: &[(Range<u32>, T)], index: ByteIndex) -> Option<&T> {
    find_index(vec, index).map(|idx| &vec[idx].1)
}

#[cfg(test)]
pub(crate) mod tests {
    use assert_matches::assert_matches;
    use codespan::ByteIndex;
    use nickel_lang_core::{
        bytecode::ast::{primop::PrimOp, Ast, AstAlloc, Node},
        files::{FileId, Files},
        parser::{grammar, lexer, ErrorTolerantParser},
    };

    use super::PositionLookup;

    pub fn parse<'ast>(alloc: &'ast AstAlloc, s: &str) -> (FileId, Ast<'ast>) {
        let id = Files::new().add("<test>", String::from(s));

        let term = grammar::TermParser::new()
            .parse_strict(alloc, id, lexer::Lexer::new(s))
            .unwrap();
        (id, term)
    }

    #[test]
    fn find_pos() {
        let alloc = AstAlloc::new();

        let (_, ast) = parse(&alloc, "let x = { y = 1 } in x.y");
        let table = PositionLookup::new(&ast);

        // Index 14 points to the 1 in { y = 1 }
        let term_1 = table.at(ByteIndex(14)).unwrap();
        assert_matches!(term_1.node, Node::Number(..));

        // Index 23 points to the y in x.y
        let term_y = table.at(ByteIndex(23)).unwrap();
        assert_matches!(
            term_y.node,
            Node::PrimOpApp {
                op: PrimOp::RecordStatAccess(_),
                ..
            }
        );

        // Index 21 points to the x in x.y
        let term_x = table.at(ByteIndex(21)).unwrap();
        assert_matches!(term_x.node, Node::Var(_));

        // This case has some mutual recursion between types and terms, which hit a bug in our
        // initial version.
        let (_, ast) = parse(
            &alloc,
            "{ range_step\
                | std.contract.unstable.RangeFun (std.contract.unstable.RangeStep -> Dyn)\
                = fun a b c => []\
            }",
        );
        let table = PositionLookup::new(&ast);
        assert_matches!(
            table.at(ByteIndex(18)).unwrap().node,
            Node::PrimOpApp {
                op: PrimOp::RecordStatAccess(_),
                ..
            }
        );

        // This case has some mutual recursion between types and terms, which hit a bug in our
        // initial version.
        let (_, ast) = parse(
            &alloc,
            "let x | { _ : { foo : Number | default = 1 } } = {} in x.PATH.y",
        );
        let table = PositionLookup::new(&ast);
        assert_matches!(
            table.at(ByteIndex(8)).unwrap().node,
            // Offset 8 actually points at the Dict, but that's a type and we only look up terms.
            // So it returns the enclosing let.
            Node::Let { .. }
        );
        assert_matches!(table.at(ByteIndex(14)).unwrap().node, Node::Record(_));
    }
}
