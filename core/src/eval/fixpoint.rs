//! Compute the fixpoint of a recursive record.
use super::{merge::RevertClosurize, *};
use crate::{
    position::{PosIdx, PosTable, TermPos},
};

/// Updates the environment of an expression by extending it with a recursive environment. In the
/// general case, the expression is expected to be a variable pointing to the element to be patched.
/// Otherwise, it's considered to have no dependencies and is left untouched.
///
/// This function achieve the same as [patch_field], but is somehow lower-level, as it operates on
/// a general [crate::bytecode::value::NickelValue] instead of a [crate::term::record::Field]. In
/// practice, the patched expression is either the value of a field or one of its pending contract.
fn patch_value<C: Cache>(cache: &mut C, value: &mut NickelValue, rec_env: &[(Ident, CacheIndex)]) {
    if let ValueContentRefMut::Thunk(idx) = value.content_make_mut() {
        // TODO: Shouldn't be mutable, [`CBNCache`] abstraction is leaking.
        cache.build_cached(idx, rec_env);
    } else {
        debug_assert!(value.is_constant())
    }
}

/// Builds a recursive environment from record bindings. For each field, `rec_env` either extracts
/// the corresponding cache element from the environment in the general case, or create a closure
/// on the fly if the field is a constant. The resulting environment is to be passed to the
/// [`patch_field`] function.
///
/// # Pending contracts
///
/// Since the implementation of RFC005, contract are lazily applied at the level of record fields.
/// Consider the following example:
///
/// ```nickel
/// let Schema = {bar | default = "hello"} in
///
/// {
///   foo | Schema = {},
///   baz = foo.bar
/// }
/// ```
///
/// This program is valid and should output `{foo.bar = "hello", baz = "hello"}`. To make this
/// happen, we have to ensure the pending contract `Schema` is applied to the recursive occurrence
/// `foo` used in the definition `baz = foo.bar`. That is, we must apply pending contracts to their
/// corresponding field when building the recursive environment.
pub fn rec_env<'a, I: Iterator<Item = (&'a LocIdent, &'a Field)>, C: Cache>(
    cache: &mut C,
    bindings: I,
    pos_record: PosIdx,
) -> Vec<(Ident, CacheIndex)> {
    bindings
        .map(|(id, field)| {
            if let Some(ref value) = field.value {
                let idx = if let Some(idx) = value.as_thunk() {
                    idx.clone()
                } else {
                    // If we are in this branch, `value` must be a constant after closurization
                    // (the evaluation of a recursive record starts by closurizing all fields and
                    // contracts). 
                    debug_assert!(value.is_constant());
                    // Constants don't need an environment: they contain no variable.
                    let closure: Closure = value.clone().into();

                    cache.add(closure, BindingType::Normal)
                };

                // We now need to wrap the binding in a value with contracts applied.
                let with_ctr_applied = RuntimeContract::apply_all(
                    NickelValue::thunk(idx, value.pos_idx()),
                    field.pending_contracts.iter().cloned(),
                    value.pos_idx(),
                );

                let final_closure: Closure = with_ctr_applied.into();

                (id.ident(), cache.add(final_closure, BindingType::Normal))
            } else {
                let error = EvalErrorData::MissingFieldDef {
                    id: *id,
                    metadata: field.metadata.clone(),
                    pos_record,
                    // The access location is not yet known (there may not be any access, if the
                    // current field is never used).
                    //
                    // This field is filled later by the evaluation function if this
                    // `MissingFieldDef` is ever extracted from the environment.
                    pos_access: PosIdx::NONE,
                };

                let closure: Closure = NickelValue::from(Term::RuntimeError(error)).into();

                (id.ident(), cache.add(closure, BindingType::Normal))
            }
        })
        .collect()
}

/// Update the environment of the content of a recursive record field by extending it with a
/// recursive environment. When seeing revertible elements as a memoizing device for functions,
/// this step correspond to function application (see documentation of
/// [crate::eval::cache::lazy::ThunkData]).
///
/// For each field, retrieve the set of dependencies from the corresponding element in the
/// environment, and only add those dependencies to the environment. This avoids retaining
/// reference-counted pointers to unused data. If no dependencies are available, conservatively add
/// all the recursive environment. See [`crate::transform::free_vars`].
pub fn patch_field<C: Cache>(cache: &mut C, field: &mut Field, rec_env: &[(Ident, CacheIndex)]) {
    if let Some(ref mut value) = field.value {
        patch_value(cache, value, rec_env);
    }

    // We must patch the contracts contained in the fields' pending contracts as well, since they
    // can depend recursively on other fields, as in:
    //
    // ```
    // let Variant = match {
    //   'num => Number,
    //   'str => String,
    //   'any => Dyn,
    // } in
    //
    // {
    //   tag | default = 'num,
    //   value | Variant tag,
    // }
    // ```
    //
    // Here, `Variant` depends on `tag` recursively.
    for ctr in field.pending_contracts.iter_mut() {
        patch_value(cache, &mut ctr.contract, rec_env);
    }
}

/// Revert an evaluated record (`Record`) back to a recursive record (`RecRecord`). The fixpoint
/// will be recomputed again at evaluation.
///
/// `revert` is used when an operation (excluding `merge`, which has its own reverting logic) might
/// have changed the value of fields (e.g. a lazy contract application on a record), requiring to
/// update recursive dependencies.
///
/// # Parameters
///
/// - `cache`: the evaluation cache
/// - `record_data`: the data of the record to revert
pub fn revert<C: Cache>(cache: &mut C, record_data: RecordData) -> Term {
    let fields = record_data
        .fields
        .into_iter()
        .map(|(id, field)| (id, field.revert_closurize(cache)))
        .collect();

    let record_data = RecordData {
        fields,
        ..record_data
    };

    // At run-time, we don't care about `RecordDeps`, because this information is already stored in
    // the cache (thunks in call-by-need mode). We set it to `None`.
    //
    // Include expressions are transformed to normal fields the very first time they are seen. They
    // aren't reconstructed, so the result of a merge never has any include expressions.
    //
    // The fields are already closurized.
    Term::RecRecord(record_data, Vec::new(), Vec::new(), None, true)
}
