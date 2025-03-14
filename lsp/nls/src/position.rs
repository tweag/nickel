use std::ops::Range;

use codespan::ByteIndex;
use nickel_lang_core::{
    bytecode::ast::{pattern::bindings::Bindings as _, Ast, Node},
    position::TermPos,
    traverse::{TraverseAlloc, TraverseControl},
};

use crate::{field_walker::FieldDefPiece, identifier::LocIdent, term::AstPtr};

/// An entry in the position lookup table. It can store different type of located objects.
#[derive(Debug, Clone, PartialEq, Eq)]
struct Entry<T> {
    range: Range<u32>,
    data: T,
}

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
fn make_disjoint<T: Clone>(mut all_ranges: Vec<Entry<T>>) -> Vec<Entry<T>> {
    // This sort order corresponds to pre-order traversal of the tree.
    all_ranges.sort_by_key(|entry| (entry.range.start, std::cmp::Reverse(entry.range.end)));
    let mut all_ranges = all_ranges.into_iter().peekable();

    // `stack` is the path in the tree leading to the node that we are currently visiting.
    let mut stack = Vec::new();
    let mut next = all_ranges.next();

    let mut disjoint = Vec::new();
    // The last position we've added to `disjoint`. Gets bumped automatically every time we
    // push a new range.
    let mut pos = next
        .as_ref()
        .map(|entry| entry.range.start)
        .unwrap_or_default();
    // We accumulate ranges using this closure, which guarantees that they are non-overlapping.
    // Note that `disjoint` and `pos` are only modified in here.
    let mut push_range = |pos: &mut u32, end: u32, data| {
        debug_assert!(*pos <= end);
        if *pos < end {
            disjoint.push(Entry {
                range: *pos..end,
                data,
            });
            *pos = end;
        }
    };

    while let Some(curr) = next {
        // If the next interval overlaps us, then we must contain it and it is our child.
        // Otherwise, we are a leaf and it is a sibling or a cousin or something.
        let next_start = all_ranges.peek().map(|entry| entry.range.start);
        match next_start {
            Some(next_start) if curr.range.end > next_start => {
                // It is our child.
                push_range(&mut pos, next_start, curr.data.clone());
                stack.push(curr);
                next = all_ranges.next();
            }
            _ => {
                // We are a leaf.
                push_range(&mut pos, curr.range.end, curr.data.clone());
                next = stack.pop().or_else(|| all_ranges.next());
            }
        }
    }

    disjoint
}

/// An entry for an AST in the position lookup table.
type AstEntry<'ast> = Entry<AstPtr<'ast>>;

/// Payload for an identifier in the position lookup table. We also need to know if the identifier
/// is part of field path in a field definition.
#[derive(Debug, Clone)]
pub struct IdentData<'ast> {
    pub ident: LocIdent,
    pub field_def: Option<FieldDefPiece<'ast>>,
}

// For deduplication, we don't care about the payload, and use the identifier and its position
// only. This works because we use NLS' `LocIdent` which does take position into account for
// everything (hashing, equality, etc.).
impl PartialEq for IdentData<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.ident == other.ident
    }
}

impl Eq for IdentData<'_> {}

type IdentEntry<'ast> = Entry<IdentData<'ast>>;

/// A lookup data structure, for looking up the term at a given position.
///
/// Overlapping positions are resolved in favor of the smaller one; i.e., lookups return the
/// most specific term for a given position.
#[derive(Default, Clone, Debug)]
pub struct PositionLookup<'ast> {
    // The intervals here are sorted and disjoint.
    ast_ranges: Vec<AstEntry<'ast>>,
    ident_ranges: Vec<IdentEntry<'ast>>,
}

impl<'ast> PositionLookup<'ast> {
    /// Create a position lookup table for looking up subterms of `rt` based on their positions.
    pub fn new(ast: &'ast Ast<'ast>) -> Self {
        fn push_id(idents: &mut Vec<IdentEntry>, id: nickel_lang_core::identifier::LocIdent) {
            if let Some(span) = id.pos.into_opt() {
                idents.push(Entry {
                    range: span.to_range(),
                    data: IdentData {
                        ident: id.into(),
                        field_def: None,
                    },
                });
            }
        }

        fn push_ids(
            idents: &mut Vec<IdentEntry>,
            ids: impl Iterator<Item = nickel_lang_core::identifier::LocIdent>,
        ) {
            idents.extend(ids.filter_map(|id| {
                Some(Entry {
                    range: id.pos.into_opt()?.to_range(),
                    data: IdentData {
                        ident: id.into(),
                        field_def: None,
                    },
                })
            }));
        }

        let mut ast_ranges = Vec::new();
        let mut ident_ranges = Vec::new();

        let mut gather_ranges = |ast: &'ast Ast<'ast>, _state: &()| {
            if let TermPos::Original(pos) = &ast.pos {
                ast_ranges.push(Entry {
                    range: pos.to_range(),
                    data: AstPtr(ast),
                });
            }

            match &ast.node {
                Node::Fun { args, .. } => push_ids(
                    &mut ident_ranges,
                    args.iter()
                        .flat_map(|arg| arg.bindings())
                        .map(|pat_bdg| pat_bdg.id),
                ),
                Node::Let { bindings, .. } => push_ids(
                    &mut ident_ranges,
                    bindings
                        .iter()
                        .flat_map(|bdg| bdg.pattern.bindings())
                        .map(|pat_bdg| pat_bdg.id),
                ),
                Node::Var(id) => push_id(&mut ident_ranges, *id),
                Node::Record(data) => {
                    // For each field, we traverse its path and for each element that is a static
                    // identifier with a defined position, we add it to the table together with a
                    // link to the corresponding field definition piece.
                    ident_ranges.extend(data.field_defs.iter().flat_map(|field_def| {
                        field_def
                            .path
                            .iter()
                            .enumerate()
                            .filter_map(|(index, path_elem)| {
                                let id = path_elem.try_as_ident()?;

                                Some(Entry {
                                    range: id.pos.into_opt()?.to_range(),
                                    data: IdentData {
                                        ident: id.into(),
                                        field_def: Some(FieldDefPiece { field_def, index }),
                                    },
                                })
                            })
                    }))
                }
                Node::Match(data) => {
                    let ids = data
                        .branches
                        .iter()
                        .flat_map(|branch| branch.pattern.bindings().into_iter())
                        .map(|bdg| bdg.id);
                    push_ids(&mut ident_ranges, ids);
                }
                _ => {}
            }
            TraverseControl::<(), ()>::Continue
        };

        ast.traverse_ref(&mut gather_ranges, &());

        // Ident ranges had better be disjoint, so we can just sort by the start position.
        ident_ranges.sort_by_key(|entry| entry.range.start);
        ident_ranges.dedup();

        PositionLookup {
            ast_ranges: make_disjoint(ast_ranges),
            ident_ranges,
        }
    }

    /// Returns the most specific subterm enclosing the given location, if there is one.
    ///
    /// Note that some positions (for example, positions belonging to top-level comments)
    /// may not be enclosed by any term.
    pub fn at(&self, index: ByteIndex) -> Option<&'ast Ast<'ast>> {
        log::debug!(
            "Looking up position {:?} in [{}]",
            index,
            self.ast_ranges
                .iter()
                .map(|entry| format!("({}, {}]", entry.range.start, entry.range.end))
                .collect::<Vec<_>>()
                .join(", ")
        );

        find(&self.ast_ranges, index).map(|ptr| ptr.0)
    }

    /// Returns the ident at the given position, if there is one.
    pub fn ident_at(&self, index: ByteIndex) -> Option<LocIdent> {
        find(&self.ident_ranges, index).map(|id_data| id_data.ident)
    }

    /// Same as [Self::ident_at], but returns an additional field definition piece which the
    /// identifier is part of, if there is one. The index of the definition piece is the position
    /// of the ident in the path.
    pub fn ident_data_at(&self, index: ByteIndex) -> Option<IdentData<'ast>> {
        find(&self.ident_ranges, index).map(|entry| entry.clone())
    }
}

fn find_index<T>(vec: &[Entry<T>], index: ByteIndex) -> Option<usize> {
    vec.binary_search_by(|Entry { range, data: _ }| {
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

fn find<T>(vec: &[Entry<T>], index: ByteIndex) -> Option<&T> {
    find_index(vec, index).map(|idx| &vec[idx].data)
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
