//! The AST of a nickel source file in the common UniTerm syntax.
use super::*;
use crate::match_sharedterm;
use crate::term::{MergePriority, MetaValue, RecordAttrs, RichTerm, Term};
use crate::types::{AbsType, Types};
use error::ParseError;
use std::convert::TryFrom;
use utils::{build_record, FieldPathElem};

#[derive(Clone)]
pub struct UniRecord {
    pub fields: Vec<(FieldPathElem, RichTerm)>,
    pub tail: Option<Types>,
    pub attrs: RecordAttrs,
}

impl UniRecord {
    pub fn into_type_strict(self) -> Result<Types, ParseError> {
        let ty = self.fields.into_iter()
            // As we build row types as a linked list via a fold on the original
            // iterator, the order of identifiers is reversed. This not a big deal
            // but it's less confusing to the user to print them in the original
            // order for error reporting.
            .rev()
            .try_fold(
                self.tail.unwrap_or(Types(AbsType::RowEmpty())),
                |acc, (path_elem, rt)| {
                    if let FieldPathElem::Ident(id) = path_elem {
                        match_sharedterm! {rt.term, with {
                            Term::MetaValue(MetaValue {
                                doc: None,
                                types: Some(ctrt),
                                contracts,
                                priority: MergePriority::Normal,
                                value: None,
                                }) if contracts.is_empty() =>
                                    Ok(Types(AbsType::RowExtend(id, Some(Box::new(ctrt.types)), Box::new(acc)))),
                            }
                            else {
                                Err(ParseError::InvalidUniRecord())
                            }
                        }
                    }
                    else {
                        Err(ParseError::InvalidUniRecord())
                    }
                }
            )?;
        Ok(Types(AbsType::StaticRecord(Box::new(ty))))
    }
}

impl TryFrom<UniRecord> for RichTerm {
    type Error = ParseError;

    fn try_from(ur: UniRecord) -> Result<Self, ParseError> {
        if ur.tail.is_some() {
            ur.into_type_strict()
                .map_err(|_err| ParseError::InvalidUniRecord())
                .map(|ty| ty.contract())
        } else {
            Ok(RichTerm::from(build_record(
                ur.fields.into_iter(),
                ur.attrs,
            )))
        }
    }
}

impl TryFrom<UniRecord> for Types {
    type Error = ParseError;

    fn try_from(ur: UniRecord) -> Result<Self, ParseError> {
        if ur.tail.is_some() {
            ur.into_type_strict()
        } else {
            ur.clone()
                .into_type_strict()
                .or_else(|_err| RichTerm::try_from(ur).map(|rt| Types(AbsType::Flat(rt))))
        }
    }
}
