//! Generate contract applications from annotations.
//!
//! Since RFC005, contracts aren't "pre-applied" during the apply contract transformation, but lazy
//! applied during evaluation. This phase still exists, just to generate the pending contracts (the
//! only ones that the interpreter will care about at runtime) from the static annotations.
//!
//! It must be run before `share_normal_form` so that newly generated pending contracts are
//! transformed as well.
use crate::{
    identifier::Ident,
    match_sharedterm,
    term::{
        record::{Field, RecordData},
        IndexMap, RichTerm, Term,
    },
    types::UnboundTypeVariableError,
};

pub fn transform_one(rt: RichTerm) -> Result<RichTerm, UnboundTypeVariableError> {
    fn attach_to_field(field: Field) -> Result<Field, UnboundTypeVariableError> {
        let pending_contracts = field.metadata.annotation.as_pending_contracts()?;

        Ok(Field {
            pending_contracts,
            ..field
        })
    }

    fn attach_to_fields(
        fields: IndexMap<Ident, Field>,
    ) -> Result<IndexMap<Ident, Field>, UnboundTypeVariableError> {
        fields
            .into_iter()
            .map(|(id, field)| Ok((id, attach_to_field(field)?)))
            .collect()
    }

    let pos = rt.pos;
    let result = match_sharedterm! {rt.term,
        with {
            Term::RecRecord(record_data, dyn_fields, deps) => {
                let RecordData {fields, attrs, sealed_tail} = record_data;

                let fields = attach_to_fields(fields)?;
                let dyn_fields = dyn_fields.into_iter().map(|(id_term, field)| {
                     Ok((id_term, attach_to_field(field)?))
                }).collect::<Result<_, _>>()?;

                RichTerm::new(Term::RecRecord(RecordData {fields, attrs, sealed_tail}, dyn_fields, deps), pos)
            },
            Term::Record(record_data) => {
                let RecordData {fields, attrs, sealed_tail} = record_data;

                let fields = attach_to_fields(fields)?;

                RichTerm::new(Term::Record(RecordData {fields, attrs, sealed_tail}), pos)
            }
        } else rt
    };
    Ok(result)
}
