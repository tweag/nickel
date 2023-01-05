//! Generate contract applications from annotations.
//!
//! During the evaluation, the following invariant is enforced: any contract (be it the type
//! annotation or a contract annotation) contained in a `MetaValue` must have been applied to the
//! inner value of this metavalue. This invariant is false just after parsing, as there's merely no
//! direct `Assume` in the output AST. This transformation makes it true after program
//! transformations by generating corresponding assume.
//!
//! It must be run before `share_normal_form` to avoid rechecking contracts each time the inner
//! value is unwrapped.
use crate::{
    match_sharedterm, mk_app,
    position::TermPos,
    term::{make as mk_term, BinaryOp, PendingContract, RichTerm, Term},
    types::UnboundTypeVariableError,
};

/// If the top-level node of the AST is a meta-value, apply the meta-value's contracts to the inner
/// value.  Otherwise, return the term unchanged.
/// Fail if an unbound type variable is encountered.
pub fn transform_one(rt: RichTerm) -> Result<RichTerm, UnboundTypeVariableError> {
    let pos = rt.pos;
    let result = match_sharedterm! {rt.term,
        with {
            Term::Annotated(annot, rt) => {
                let pos_inh = pos.into_inherited();

                let ctrs = annot.iter()
                    .map(|ctr| Ok(PendingContract::new(ctr.types.contract()?, ctr.label.clone())))
                    .collect::<Result<Vec<_>, _>>()?;

                let inner = apply_contracts(rt, ctrs.into_iter(), pos_inh);
                RichTerm::new(Term::Annotated(annot, rt), pos)
            },
            Term::MetaValue(meta) if meta.value.is_some() => {
                let mut meta = meta;
                let pos_inh = pos.into_inherited();

                let ctrs = meta
                    .types
                    .iter()
                    .chain(meta.contracts.iter())
                    .map(|ctr| Ok(PendingContract::new(ctr.types.contract()?, ctr.label.clone())))
                    .collect::<Result<Vec<_>, _>>()?;

                let inner = apply_contracts(meta.value.take().unwrap(), ctrs.into_iter(), pos_inh);

                meta.value.replace(inner);
                RichTerm::new(Term::MetaValue(meta), pos)
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
