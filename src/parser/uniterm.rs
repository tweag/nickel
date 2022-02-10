//! Additional AST nodes for the common UniTerm syntax (see RFC002 for more details).
use super::*;
use error::ParseError;
use utils::{build_record, FieldPathElem};

use crate::{
    position::{RawSpan, TermPos},
    term::{MergePriority, MetaValue, RecordAttrs, RichTerm, Term},
    types::{AbsType, Types, UnboundTypeVariableError},
};

use std::convert::TryFrom;

/// A node of the uniterm AST. We only define new variants for constructs common to types and
/// terms. Otherwise, we piggyback on the existing ASTs to avoid duplicating code and definitions.
///
/// During parsing, some constructs are common to both terms and types, such as variables or record
/// literals ([`UniRecord`]s). They may be the source of ambiguities in the grammar, if we add two
/// different derivation for them. This happens inside rules that accepts both a term and and a
/// type, say the operands of an arrow `lhs -> rhs`. Take for example `lhs` a variable: `lhs := a`.
/// It's hard not to end up having two possible derivation for `a`, one going first through term
/// rules and then types rules (a custom contract `#a` in the old syntax), and vice-versa (a type
/// variable `a` in the old syntax.
///
/// To avoid the issue, we parse the source as a `UniTermNode`s, and give only one possible
/// derivation - we will take a variable as an example in the following - through the parsing
/// rules. As long as we can't decide yet how to see this variable, we keep it as a
/// `UniTermNode::Var`.
///
/// As soon as this variable is used in a compound expression, the top-level rule tells us how to
/// translate it. For example, if we see an arrow `a -> Num`, then we will convert it to a type
/// variable, and return `UniTermNode::Types(AbsType::Arrow(..))`. If, on the other hand, we enter
/// the rule for infix operator as in `a + 1`, `a` will be converted to a `Term::Var` and the
/// resulting uniterm will be `UniTermNode::Term(Term::Op2(..))`.
pub enum UniTermNode {
    /// A variable. Can refer both to a term variable or a type variable.
    Var(Ident),
    /// A recod, that can be either a record literal or a record type.
    Record(UniRecord),
    /// A `UniTerm` that has been determined to be a term.
    Term(RichTerm),
    /// A `UniTerm` that has been determined to be a type.
    Types(Types),
}

/// A uniterm with positional information.
pub struct UniTerm {
    node: UniTermNode,
    pos: TermPos,
}

impl From<UniTermNode> for UniTerm {
    fn from(node: UniTermNode) -> Self {
        UniTerm {
            node,
            pos: TermPos::None,
        }
    }
}

impl UniTerm {
    pub fn with_pos(mut self, pos: TermPos) -> Self {
        self.pos = pos;
        self
    }
}

impl TryFrom<UniTerm> for Types {
    type Error = ParseError;

    fn try_from(ut: UniTerm) -> Result<Self, ParseError> {
        match ut.node {
            UniTermNode::Var(id) => Ok(Types(AbsType::Var(id))),
            UniTermNode::Record(r) => Types::try_from(r),
            UniTermNode::Types(ty) => Ok(ty),
            UniTermNode::Term(rt) => Ok(Types(AbsType::Flat(rt))),
        }
    }
}

impl TryFrom<UniTerm> for RichTerm {
    type Error = ParseError;

    fn try_from(ut: UniTerm) -> Result<Self, ParseError> {
        let UniTerm { node, pos } = ut;
        let rt = match node {
            UniTermNode::Var(id) => RichTerm::new(Term::Var(id), pos),
            UniTermNode::Record(r) => RichTerm::try_from(r)?,
            UniTermNode::Types(ty) => {
                ty.contract().map_err(|UnboundTypeVariableError(id)| {
                    // We unwrap the position of the identifier, which must be set at this stage of parsing
                    let pos = id.pos;
                    ParseError::UnboundTypeVariables(vec![id], pos.unwrap())
                })?
            }
            UniTermNode::Term(rt) => rt,
        };

        Ok(rt.with_pos(pos))
    }
}

impl From<RichTerm> for UniTerm {
    fn from(rt: RichTerm) -> Self {
        let pos = rt.pos;

        UniTerm {
            node: UniTermNode::Term(rt),
            pos,
        }
    }
}

impl From<Term> for UniTerm {
    fn from(t: Term) -> Self {
        Self::from(RichTerm::from(t))
    }
}

impl From<Types> for UniTerm {
    fn from(ty: Types) -> Self {
        UniTerm {
            node: UniTermNode::Types(ty),
            pos: TermPos::None,
        }
    }
}

impl From<UniRecord> for UniTerm {
    fn from(ur: UniRecord) -> Self {
        let pos = ur.pos;

        UniTerm {
            node: UniTermNode::Record(ur),
            pos,
        }
    }
}

/// A record in the UniTerm syntax.
#[derive(Clone)]
pub struct UniRecord {
    pub fields: Vec<(FieldPathElem, RichTerm)>,
    pub tail: Option<(Types, TermPos)>,
    pub attrs: RecordAttrs,
    pub pos: TermPos,
}

/// Error indicating that a construct is not allowed when trying to interpret an `UniRecord` as a
/// record type in a strict way. See [`into_type_strict`]. Hold the position of the illegal
/// construct.
pub struct InvalidRecordTypeError(pub TermPos);

impl UniRecord {
    /// Try to convert a `UniRecord` to a type. The strict part means that if the `UniRecord` must
    /// be a plain record type, uniquely containing fields of the form `fields: Type`. Currently,
    /// it doesn't support the field path syntax: `{foo.bar.baz : Type}.into_type_strict()` returns
    /// an `Err`.
    pub fn into_type_strict(self) -> Result<Types, InvalidRecordTypeError> {
        let ty = self
            .fields
            .into_iter()
            // Because we build row types as a linked list by folding on the original iterator, the
            // order of identifiers is reversed. This not a big deal but it's less confusing to the
            // user to print them in the original order for error reporting.
            .rev()
            .try_fold(
                self.tail
                    .map(|(tail, _)| tail)
                    .unwrap_or(Types(AbsType::RowEmpty())),
                |acc, (path_elem, rt)| {
                    match path_elem {
                        FieldPathElem::Ident(id) => {
                            // At parsing stage, all `Rc`s must be 1-counted. We can thus call
                            // `into_owned()` without risking to actually clone anything.
                            match rt.term.into_owned() {
                                Term::MetaValue(MetaValue {
                                    doc: None,
                                    types: Some(ctrt),
                                    contracts,
                                    priority: MergePriority::Normal,
                                    value: None,
                                }) if contracts.is_empty() => Ok(Types(AbsType::RowExtend(
                                    id,
                                    Some(Box::new(ctrt.types)),
                                    Box::new(acc),
                                ))),
                                _ => {
                                    // Position of identifiers must always be set at this stage
                                    // (parsing)
                                    let span_id = id.pos.unwrap();
                                    let term_pos = rt.pos.into_opt().unwrap_or(span_id);
                                    Err(InvalidRecordTypeError(TermPos::Original(
                                        RawSpan::fuse(span_id, term_pos).unwrap(),
                                    )))
                                }
                            }
                        }
                        FieldPathElem::Expr(rt) => Err(InvalidRecordTypeError(rt.pos)),
                    }
                },
            )?;
        Ok(Types(AbsType::StaticRecord(Box::new(ty))))
    }

    pub fn with_pos(mut self, pos: TermPos) -> Self {
        self.pos = pos;
        self
    }
}

impl TryFrom<UniRecord> for RichTerm {
    type Error = ParseError;

    /// Convert a `UniRecord` to a term. If the `UniRecord` has a tail, it is first interpreted as
    /// a type and then converted to a contract. Otherwise it is interpreted as a record directly.
    /// Fail if the `UniRecord` has a tail but isn't syntactically a record type either.
    fn try_from(ur: UniRecord) -> Result<Self, ParseError> {
        let pos = ur.pos;

        let result = if let Some((_, tail_pos)) = ur.tail {
            ur.into_type_strict()
                // We unwrap all positions: at this stage of the parsing, they must all be set
                .map_err(|InvalidRecordTypeError(pos)| {
                    ParseError::InvalidUniRecord(pos.unwrap(), tail_pos.unwrap(), pos.unwrap())
                })
                .and_then(|ty| {
                    ty.contract().map_err(|UnboundTypeVariableError(id)| {
                        ParseError::UnboundTypeVariables(vec![id], pos.unwrap())
                    })
                })
        } else {
            Ok(RichTerm::from(build_record(
                ur.fields.into_iter(),
                ur.attrs,
            )))
        };

        result.map(|rt| rt.with_pos(pos))
    }
}

impl TryFrom<UniRecord> for Types {
    type Error = ParseError;

    /// Convert a `UniRecord` to a type. If the `UniRecord` has a tail, it is interpreted strictly
    /// as a type and fail if it isn't a plain record type. Otherwise, we first try to interpret it
    /// as a plain record type, and if that doesn't work, we interpret it as a term and wrap it as
    /// a flat type.
    fn try_from(ur: UniRecord) -> Result<Self, ParseError> {
        let pos = ur.pos;

        if let Some((_, tail_pos)) = ur.tail {
            ur.into_type_strict()
                .map_err(|InvalidRecordTypeError(illegal_pos)| {
                    ParseError::InvalidUniRecord(
                        illegal_pos.unwrap(),
                        tail_pos.unwrap(),
                        pos.unwrap(),
                    )
                })
        } else {
            ur.clone()
                .into_type_strict()
                .or_else(|_| RichTerm::try_from(ur).map(|rt| Types(AbsType::Flat(rt))))
        }
    }
}

// impl TryFrom<Types> for RichTerm {
//     type Error = ParseError;
//
//     fn try_from(ty: Types) -> Result<Self, ParseError> {
//         ty.contract().map_err(|UnboundTypeVariableError(id)| {
//             // We unwrap the position of the identifier, which must be set at this stage of parsing
//             let pos = id.pos;
//             ParseError::UnboundTypeVariables(vec![id], pos.unwrap())
//         })
//     }
// }
//
// // This `TryFrom` implementation can't actually fail. However, in order to have the macros
// // `AsTypes`/`AsTerm` of the grammar to work uniformly, we have to require the same
// // `TryFrom<A, Error = ParseError>` implementation for `UniRecord`, `Term`, and `Types`.
// impl TryFrom<RichTerm> for Types {
//     type Error = ParseError;
//
//     fn try_from(rt: RichTerm) -> Result<Self, ParseError> {
//         Ok(Types(AbsType::Flat(rt)))
//     }
// }
