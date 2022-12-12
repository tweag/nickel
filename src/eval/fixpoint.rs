//! Compute the fixpoint of a recursive record.
use super::*;

/// Build a recursive environment from record bindings. For each field, `rec_env` either extracts
/// the corresponding thunk from the environment in the general case, or create a closure on the
/// fly if the field is a constant. The resulting environment is to be passed to the
/// [`patch_field`] function.
pub fn rec_env<'a, I: Iterator<Item = (&'a Ident, &'a Field)>, C: Cache>(
    cache: &mut C,
    bindings: I,
    env: &Environment,
) -> Result<Vec<(Ident, CacheIndex)>, EvalError> {
    bindings
        .map(|(id, field)| {
            if let Some(ref value) = field.value {
                match value.as_ref() {
                    Term::Var(ref var_id) => {
                        let idx = env
                            .get(var_id)
                            .cloned()
                            .ok_or(EvalError::UnboundIdentifier(*var_id, value.pos))?;
                        Ok((*id, idx))
                    }
                    _ => {
                        // If we are in this branch, `rt` must be a constant after the share normal form
                        // transformation, hence it should not need an environment, which is why it is
                        // dropped.
                        let closure = Closure {
                            body: value.clone(),
                            env: Environment::new(),
                        };
                        Ok((*id, cache.add(closure, IdentKind::Let, BindingType::Normal)))
                    }
                }
            } else {
                //TODO: how to handle missing recursive field, here?
                // We probably want to bundle a term that errors out with the relevant information
                // when trying to evaluate it.
                todo!()
            }
        })
        .collect()
}

/// Update the environment of the content of a recursive record field by extending it with a
/// recursive environment. When seeing revertible thunks as a memoizing device for functions, this
/// step correspond to function application (see documentation of [crate::eval::lazy::ThunkData]).
///
/// For each field, retrieve the set set of dependencies from the corresponding thunk in the
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
        if let Term::Var(var_id) = &*value.term {
            // TODO: Shouldn't be mutable, [`CBNCache`] abstraction is leaking.
            let mut idx = env
                .get(var_id)
                .cloned()
                .ok_or(EvalError::UnboundIdentifier(*var_id, value.pos))?;

            cache.build_cached(&mut idx, rec_env);
        }

        // Thanks to the share normal form transformation, the content is either a constant or a
        // variable. In the constant case, the environment is irrelevant and we don't have to do
        // anything in the `else` case.
    }

    Ok(())
}
