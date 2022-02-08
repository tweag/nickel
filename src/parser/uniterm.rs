//! Additional AST nodes for the common UniTerm syntax (see RFC002 for more details).
use super::*;
use crate::types::UnboundTypeVariableError;
use error::ParseError;
use utils::{build_record, FieldPathElem};

use crate::{
    position::{RawSpan, TermPos},
    term::{MergePriority, MetaValue, RecordAttrs, RichTerm, Term},
    types::{AbsType, Types},
};

use std::convert::TryFrom;

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
