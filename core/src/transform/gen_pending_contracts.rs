//! Generate contract applications from annotations.
//!
//! Since RFC005, contracts aren't "pre-applied" during the apply contract transformation, but
//! lazily applied during evaluation. This phase still exists, just to generate the pending
//! contracts (the only ones that the interpreter will care about at runtime) from the static
//! annotations.
//!
//! However, contracts generated from a type annotation (see
//! [#1228](https://github.com/tweag/nickel/issues/1228)) are an exception, because they shouldn't
//! propagate. Such contracts are generated and applied once and for all during this phase.
//!
//! The `gen_pending_contracts` phase implemented by this module must be run before
//! `share_normal_form` so that newly generated pending contracts are transformed as well.
use crate::{
    identifier::LocIdent,
    match_sharedterm,
    term::{
        record::{Field, RecordData},
        IndexMap, RichTerm, RuntimeContract, Term,
    },
    typ::UnboundTypeVariableError,
};

pub fn transform_one(rt: RichTerm) -> Result<RichTerm, UnboundTypeVariableError> {
    fn attach_to_field(field: Field) -> Result<Field, UnboundTypeVariableError> {
        // We simply add the contracts to the pending contract fields
        let pending_contracts = field.metadata.annotation.pending_contracts()?;
        // Type annotations are different: the contract is generated statically, because as opposed
        // to contract annotations, type anntotations don't propagate.
        let value = field
            .value
            .map(|v| -> Result<RichTerm, UnboundTypeVariableError> {
                if let Some(labeled_ty) = &field.metadata.annotation.typ {
                    let pos = v.pos;
                    let contract = RuntimeContract::from_static_type(labeled_ty.clone())?;
                    Ok(contract.apply(v, pos))
                } else {
                    Ok(v)
                }
            })
            .transpose()?;

        Ok(Field {
            value,
            pending_contracts,
            ..field
        })
    }

    fn attach_to_fields(
        fields: IndexMap<LocIdent, Field>,
    ) -> Result<IndexMap<LocIdent, Field>, UnboundTypeVariableError> {
        fields
            .into_iter()
            .map(|(id, field)| Ok((id, attach_to_field(field)?)))
            .collect()
    }

    let pos = rt.pos;
    let result = match_sharedterm!(match (rt.term) {
        Term::RecRecord(record_data, dyn_fields, deps) => {
            let RecordData {
                fields,
                attrs,
                sealed_tail,
            } = record_data;

            let fields = attach_to_fields(fields)?;
            let dyn_fields = dyn_fields
                .into_iter()
                .map(|(id_term, field)| Ok((id_term, attach_to_field(field)?)))
                .collect::<Result<_, _>>()?;

            RichTerm::new(
                Term::RecRecord(
                    RecordData {
                        fields,
                        attrs,
                        sealed_tail,
                    },
                    dyn_fields,
                    deps,
                ),
                pos,
            )
        }
        Term::Record(record_data) => {
            let RecordData {
                fields,
                attrs,
                sealed_tail,
            } = record_data;

            let fields = attach_to_fields(fields)?;

            RichTerm::new(
                Term::Record(RecordData {
                    fields,
                    attrs,
                    sealed_tail,
                }),
                pos,
            )
        }
        _ => rt,
    });
    Ok(result)
}
