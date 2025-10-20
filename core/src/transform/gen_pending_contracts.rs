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
    bytecode::value::{NickelValue, ValueContent, lens::TermContent},
    identifier::LocIdent,
    position::PosTable,
    term::{
        IndexMap, RuntimeContract, Term,
        record::{Field, RecordData},
    },
    typ::UnboundTypeVariableError,
};

/// Take a field, generate pending contracts from the annotation and return the field with the
/// pending contracts filled.
///
/// Contracts derived from typed annotations aren't added to the pending contracts, but mapped
/// directly onto the value, as they don't propagate (see RFC005 for more details).
///
/// # Preconditions
///
/// The pending contracts are expected to be initially empty. This function will override any
/// previous pending contract.
pub fn with_pending_contracts(
    pos_table: &mut PosTable,
    field: Field,
) -> Result<Field, UnboundTypeVariableError> {
    // We simply add the contracts to the pending contract fields
    let pending_contracts = field.metadata.annotation.pending_contracts(pos_table)?;
    // Type annotations are different: the contract is generated statically, because as opposed
    // to contract annotations, type anntotations don't propagate.
    let value = field
        .value
        .map(|v| -> Result<NickelValue, UnboundTypeVariableError> {
            if let Some(labeled_ty) = &field.metadata.annotation.typ {
                let pos_idx = v.pos_idx();
                let contract = RuntimeContract::from_static_type(pos_table, labeled_ty.clone())?;
                Ok(contract.apply(v, pos_idx))
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

pub fn transform_one(
    pos_table: &mut PosTable,
    value: NickelValue,
) -> Result<NickelValue, UnboundTypeVariableError> {
    fn attach_to_fields(
        pos_table: &mut PosTable,
        fields: IndexMap<LocIdent, Field>,
    ) -> Result<IndexMap<LocIdent, Field>, UnboundTypeVariableError> {
        fields
            .into_iter()
            .map(|(id, field)| Ok((id, with_pending_contracts(pos_table, field)?)))
            .collect()
    }

    let pos_idx = value.pos_idx();

    Ok(match value.content() {
        ValueContent::Record(lens) if lens.peek().is_empty_record() => lens.restore(),
        ValueContent::Record(lens) => {
            // unwrap(): we treated the inline empty record case above.
            let RecordData {
                fields,
                attrs,
                sealed_tail,
            } = lens.take().into_opt().unwrap().0;

            let fields = attach_to_fields(pos_table, fields)?;

            // unwrap(): since we took the position from `value` which is a record, its position
            // index must be an inline position index.
            NickelValue::record(
                RecordData {
                    fields,
                    attrs,
                    sealed_tail,
                },
                pos_idx,
            )
            .unwrap()
        }
        ValueContent::Term(lens) => match lens {
            TermContent::RecRecord(lens) => {
                let (record_data, includes, dyn_fields, deps, closurized) = lens.take();

                let RecordData {
                    fields,
                    attrs,
                    sealed_tail,
                } = record_data;

                let fields = attach_to_fields(pos_table, fields)?;
                let dyn_fields = dyn_fields
                    .into_iter()
                    .map(|(id_term, field)| {
                        Ok((id_term, with_pending_contracts(pos_table, field)?))
                    })
                    .collect::<Result<_, _>>()?;

                NickelValue::term(
                    Term::RecRecord(
                        RecordData {
                            fields,
                            attrs,
                            sealed_tail,
                        },
                        includes,
                        dyn_fields,
                        deps,
                        closurized,
                    ),
                    pos_idx,
                )
            }
            lens => lens.restore(),
        },
        lens => lens.restore(),
    })
}
