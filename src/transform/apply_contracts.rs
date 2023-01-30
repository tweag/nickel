//! Generate contract applications from annotations.
//!
//! During the evaluation, the following invariant is enforced: any contract (be it the type
//! annotation or a contract annotation) contained in a `Annotated` or in a field's metadata must
//! have been applied to the inner value. This invariant is false just after parsing, as there's
//! merely no direct `Assume` in the output AST. This transformation makes it true after program
//! transformations by generating corresponding assume.
//!
//! It must be run before `share_normal_form` to avoid rechecking contracts each time the inner
//! value is unwrapped.
use crate::term::TypeAnnotation;
use crate::{
    match_sharedterm, mk_app,
    position::TermPos,
    term::{
        make as mk_term,
        record::{Field, RecordData},
        BinaryOp, PendingContract, RichTerm, Term,
    },
    types::UnboundTypeVariableError,
};

/// If the top-level node of the AST is a meta-value, apply the meta-value's contracts to the inner
/// value.  Otherwise, return the term unchanged.
/// Fail if an unbound type variable is encountered.
pub fn transform_one(rt: RichTerm) -> Result<RichTerm, UnboundTypeVariableError> {
    fn apply_from_annot(
        annot: &TypeAnnotation,
        rt: RichTerm,
        pos: TermPos,
    ) -> Result<RichTerm, UnboundTypeVariableError> {
        let ctrs = annot
            .iter()
            .map(|ctr| {
                Ok(PendingContract::new(
                    ctr.types.contract()?,
                    ctr.label.clone(),
                ))
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(apply_contracts(rt, ctrs.into_iter(), pos.into_inherited()))
    }

    fn apply_to_field(field: Field) -> Result<Field, UnboundTypeVariableError> {
        //TODO: change apply contracts
        let Field {
            metadata,
            value,
            pending_contracts,
        } = field;
        let value = value
            .map(|v| {
                let pos_v = v.pos;
                apply_from_annot(&metadata.annotation, v, pos_v)
            })
            .transpose()?;

        Ok(Field {
            value,
            metadata,
            pending_contracts,
        })
    }

    let pos = rt.pos;
    let result = match_sharedterm! {rt.term,
        with {
            Term::Annotated(annot, inner) => {
                let inner_transfd = apply_from_annot(&annot, inner, pos)?;
                RichTerm::new(Term::Annotated(annot, inner_transfd), pos)
            },
            Term::RecRecord(record_data, dyn_fields, deps) => {
                let RecordData {fields, attrs, sealed_tail} = record_data;

                let fields = fields.into_iter().map(|(id, field)| {
                    Ok((id, apply_to_field(field)?))
                }).collect::<Result<_, _>>()?;

                let dyn_fields = dyn_fields.into_iter().map(|(id_term, field)| {
                    Ok((id_term, apply_to_field(field)?))
                }).collect::<Result<_, _>>()?;

                RichTerm::new(Term::RecRecord(RecordData {fields, attrs, sealed_tail}, dyn_fields, deps), pos)
            },
            Term::Record(record_data) => {
                let RecordData {fields, attrs, sealed_tail} = record_data;

                let fields = fields.into_iter().map(|(id, field)| {
                    Ok((id, apply_to_field(field)?))
                }).collect::<Result<_, _>>()?;

                RichTerm::new(Term::Record(RecordData {fields, attrs, sealed_tail}), pos)
            }
        } else rt
    };
    Ok(result)
}

pub fn apply_contracts<I>(rt: RichTerm, contracts: I, pos: TermPos) -> RichTerm
where
    I: Iterator<Item = PendingContract>,
{
    contracts.fold(rt, |acc, ctr| {
        mk_app!(
            mk_term::op2(BinaryOp::Assume(), ctr.contract, Term::Lbl(ctr.label)).with_pos(pos),
            acc
        )
        .with_pos(pos)
    })
}
