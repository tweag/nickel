//! Compute the fixpoint of a recursive record.
use super::*;

/// Build a recursive environment from record bindings. For each field, `rec_env` either extracts
/// the corresponding thunk from the environment in the general case, or create a closure on the
/// fly if the field is a constant. The resulting environment is to be passed to the
/// [`patch_field`] function.
pub fn rec_env<'a, I: Iterator<Item = (&'a Ident, &'a RichTerm)>>(
    bindings: I,
    env: &Store,
) -> Result<Vec<(Ident, Thunk)>, EvalError> {
    bindings
        .map(|(id, rt)| match rt.as_ref() {
            Term::Symbol(ref sym) => {
                let thunk = env
                    .get(sym)
                    .ok_or_else(|| EvalError::UnboundIdentifier(sym.ident.clone(), rt.pos))?;
                Ok((id.clone(), thunk))
            }
            _ => {
                // If we are in this branch, `rt` must be a constant after the share normal form
                // transformation, hence it should not need an environment, which is why it is
                // dropped.
                let closure = Closure::atomic_closure(rt.clone());
                Ok((id.clone(), Thunk::new(closure, IdentKind::Let)))
            }
        })
        .collect()
}

/// Update the environment of the content of a recursive record field by extending it with a
/// recursive environment.
///
/// For each field, retrieve the set set of dependencies from the corresponding thunk in the
/// environment, and only add those dependencies to the environment. This avoid retaining
/// reference-counted pointers to unused data. If no dependencies are available, conservatively add
/// all the recursive environment. See [`crate::transform::free_vars`].
pub fn patch_field(
    rt: &RichTerm,
    rec_env: &[(Ident, Thunk)],
    env: &Store,
) -> Result<(), EvalError> {
    if let Term::Symbol(sym) = &*rt.term {
        let mut thunk = env
            .get(sym)
            .ok_or_else(|| EvalError::UnboundIdentifier(sym.ident.clone(), rt.pos))?;
        let deps = thunk.deps();

        thunk.borrow_mut().env.with_front_mut(|env| match deps {
            ThunkDeps::Known(deps) => {
                env.extend(rec_env.iter().filter(|(id, _)| deps.contains(id)).cloned())
            }
            ThunkDeps::Unknown => env.extend(rec_env.iter().cloned()),
            ThunkDeps::Empty => (),
        });
    }

    // Thanks to the share normal form transformation, the content is either a constant or a
    // variable. In the constant case, the environment is irrelevant and we don't have to do
    // anything in the `else` case.
    Ok(())
}
