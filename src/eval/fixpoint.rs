//! Compute the fixpoint of a recursive record.
use super::{merge::RevertClosurize, *};
use crate::{label::Label, position::TermPos};

// Update the environment of a term by extending it with a recursive environment. In the general
// case, the term is expected to be a variable pointing to the element to be patched. Otherwise, it's
// considered to have no dependencies and is left untouched.
//
// This function achieve the same as `patch_field`, but is somehow lower-level, as it operates on a
// general `RichTerm` instead of a `Field`. In practice, the patched term is either the value of a
// field or one of its pending contract.
fn patch_term<C: Cache>(
    cache: &mut C,
    term: &RichTerm,
    rec_env: &[(Ident, CacheIndex)],
    env: &Environment,
) -> Result<(), EvalError> {
    if let Term::Var(var_id) = &*term.term {
        // TODO: Shouldn't be mutable, [`CBNCache`] abstraction is leaking.
        let mut idx = env
            .get(var_id)
            .cloned()
            .ok_or(EvalError::UnboundIdentifier(*var_id, term.pos))?;

        cache.build_cached(&mut idx, rec_env);
    };

    Ok(())
}

/// Build a recursive environment from record bindings. For each field, `rec_env` either extracts
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
pub fn rec_env<'a, I: Iterator<Item = (&'a Ident, &'a Field)>, C: Cache>(
    cache: &mut C,
    bindings: I,
    env: &Environment,
    pos_record: TermPos,
) -> Result<Vec<(Ident, CacheIndex)>, EvalError> {
    bindings
        .map(|(id, field)| {
            if let Some(ref value) = field.value {
                let idx = match value.as_ref() {
                    Term::Var(ref var_id) => env
                        .get(var_id)
                        .cloned()
                        .ok_or(EvalError::UnboundIdentifier(*var_id, value.pos))?,
                    _ => {
                        // If we are in this branch, `rt` must be a constant after the share normal form
                        // transformation, hence it should not need an environment, which is why it is
                        // dropped.
                        let closure = Closure {
                            body: value.clone(),
                            env: Environment::new(),
                        };

                        cache.add(closure, IdentKind::Record, BindingType::Normal)
                    }
                };

                // We now need to wrap the binding in a value with contracts applied.
                // Pending contracts might use identifiers from the current record's environment,
                // so we start from in the environment of the original record.
                let mut final_env = env.clone();
                let id_value = Ident::fresh();
                final_env.insert(id_value, idx);

                let with_ctr_applied = RuntimeContract::apply_all(
                    RichTerm::new(Term::Var(id_value), value.pos),
                    field.pending_contracts.iter().cloned().flat_map(|ctr| {
                        // This operation is the heart of our preliminary fix for
                        // [#1161](https://github.com/tweag/nickel/issues/1161). Whenever we detect
                        // the presence of free type variables in a contract, by witnessing a nonempty
                        // type environment in the label, we need to not just apply the original
                        // contract but also its dual. The rationale here is that a record field that
                        // recursively depends on another of type `T`, say, should be considered a
                        // function with domain `T`. Consequently, the same contract that would be a
                        // applied to the argument of a function of type `T -> Dyn` should be applied
                        // to the recursive reference.
                        //
                        // Thus, the recursive reference must satisfy the contract for `T` as well as
                        // the "dual" contract `T.dualize()`; the latter is defined to be the domain
                        // contract for a function of type `T -> Dyn`. This sublety only matters if
                        // `T` contains free type variables because only then does `T.dualize()` differ
                        // from `T` at all.
                        //
                        // We expect to implement a way of solving this dilemma without essentially
                        // applying every contract twice. This will likely involve a rewriting of
                        // contracts corresponding to free variables which is yet to be proved sound.
                        if ctr.label.type_environment.is_empty() {
                            vec![ctr]
                        } else {
                            vec![
                                ctr.clone(),
                                RuntimeContract {
                                    contract: ctr.contract,
                                    label: Label {
                                        polarity: ctr.label.polarity.flip(),
                                        dualize: true,
                                        ..ctr.label
                                    },
                                },
                            ]
                        }
                    }),
                    value.pos,
                );

                let final_closure = Closure {
                    body: with_ctr_applied,
                    env: final_env,
                };

                Ok((
                    *id,
                    cache.add(final_closure, IdentKind::Record, BindingType::Normal),
                ))
            } else {
                let error = EvalError::MissingFieldDef {
                    id: *id,
                    metadata: field.metadata.clone(),
                    pos_record,
                    // The access is not yet known (there may not be any access, if this error is
                    // never raised).
                    //
                    // This field is filled by the evaluation function when a `MissingFieldDef` is
                    // extracted from the environment.
                    pos_access: TermPos::None,
                };

                let closure = Closure {
                    body: RichTerm::from(Term::RuntimeError(error)),
                    env: Environment::new(),
                };

                Ok((
                    *id,
                    cache.add(closure, IdentKind::Record, BindingType::Normal),
                ))
            }
        })
        .collect()
}

/// Update the environment of the content of a recursive record field by extending it with a
/// recursive environment. When seeing revertible elements as a memoizing device for functions,
/// this step correspond to function application (see documentation of
/// [crate::eval::cache::lazy::ThunkData]).
///
/// For each field, retrieve the set set of dependencies from the corresponding element in the
/// environment, and only add those dependencies to the environment. This avoids retaining
/// reference-counted pointers to unused data. If no dependencies are available, conservatively add
/// all the recursive environment. See [`crate::transform::free_vars`].
pub fn patch_field<C: Cache>(
    cache: &mut C,
    field: &Field,
    rec_env: &[(Ident, CacheIndex)],
    env: &Environment,
) -> Result<(), EvalError> {
    if let Some(ref value) = field.value {
        patch_term(cache, value, rec_env, env)?;
    }

    // We must patch the contracts contained in the fields' pending contracts as well, since they
    // can depend recursively on other fields, as in:
    //
    // ```
    // let Variant = match {
    //   `num => Number,
    //   `str => String,
    //   `any => Dyn,
    // } in
    //
    // {
    //   tag | default = `num,
    //   value | Variant tag,
    // }
    // ```
    //
    // Here, `Variant` depends on `tag` recursively.
    for pending_contract in field.pending_contracts.iter() {
        patch_term(cache, &pending_contract.contract, rec_env, env)?;
    }

    Ok(())
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
/// `env` and `local_env` are similar to the parameters of
/// `[crate::transform::Closurize::closurize]`.
///
/// - `cache`: the evaluation cache
/// - `record_data`: the data of the record to revert
/// - `env`: the final environment in which the fields of the result will be closurized
/// - `local_env`: the environment of the record represented by `record_data`
pub fn revert<C: Cache>(
    cache: &mut C,
    record_data: RecordData,
    env: &mut Environment,
    local_env: &Environment,
) -> Term {
    let fields = record_data
        .fields
        .into_iter()
        .map(|(id, field)| (id, field.revert_closurize(cache, env, local_env.clone())))
        .collect();

    let record_data = RecordData {
        fields,
        ..record_data
    };

    // At run-time, we don't care about `RecordDeps`, because this information is already stored in
    // the cache (formerly thunks in call-by-need). We set it to `None`.
    Term::RecRecord(record_data, Vec::new(), None)
}
