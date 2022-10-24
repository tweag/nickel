//! Compute the fixpoint of a recursive record.
use super::*;

/// Build a recursive environment from record bindings. For each field, `rec_env` either extracts
/// the corresponding thunk from the environment in the general case, or create a closure on the
/// fly if the field is a constant. The resulting environment is to be passed to the
/// [`patch_field`] function.
pub fn rec_env<'a, I: Iterator<Item = (&'a Ident, &'a RichTerm)>, C: Cache>(
    cache: &mut C,
    bindings: I,
    env: &Environment<C>,
) -> Result<Vec<(Ident, C::Index)>, EvalError> {
    bindings
        .map(|(id, rt)| match rt.as_ref() {
            Term::Var(ref var_id) => {
                let thunk = env
                    .get(var_id)
                    .cloned()
                    .ok_or(EvalError::UnboundIdentifier(*var_id, rt.pos))?;
                Ok((*id, thunk))
            }
            _ => {
                // If we are in this branch, `rt` must be a constant after the share normal form
                // transformation, hence it should not need an environment, which is why it is
                // dropped.
                let closure = Closure {
                    body: rt.clone(),
                    env: Environment::<C>::new(),
                };
                Ok((*id, cache.add(closure, IdentKind::Let, BindingType::Normal)))
            }
        })
        .collect()
}

/// Update the environment of the content of a recursive record field by extending it with a
/// recursive environment.
///
/// For each field, retrieve the set set of dependencies from the corresponding thunk in the
/// environment, and only add those dependencies to the environment. This avoids retaining
/// reference-counted pointers to unused data. If no dependencies are available, conservatively add
/// all the recursive environment. See [`crate::transform::free_vars`].
pub fn patch_field<C: Cache>(
    cache: &mut C,
    rt: &RichTerm,
    rec_env: &[(Ident, C::Index)],
    env: &Environment<C>,
) -> Result<(), EvalError> {
    if let Term::Var(var_id) = &*rt.term {
        let thunk = env
            .get(var_id)
            .cloned()
            .ok_or(EvalError::UnboundIdentifier(*var_id, rt.pos))?;

        let deps = thunk.deps();

        match deps {
            ThunkDeps::Known(deps) => cache.patch(thunk, |t| {
                t.env
                    .extend(rec_env.iter().filter(|(id, _)| deps.contains(id)).cloned())
            }),

            ThunkDeps::Unknown => cache.patch(thunk, |t| t.env.extend(rec_env.iter().cloned())),
            ThunkDeps::Empty => (),
        };
    }

    // Thanks to the share normal form transformation, the content is either a constant or a
    // variable. In the constant case, the environment is irrelevant and we don't have to do
    // anything in the `else` case.
    Ok(())
}
