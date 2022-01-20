//! Helper to compute the fixpoint of a recursive record.
use super::*;

/// Build a recursive environment from an iterator of bindings. For each binding, extract the thunk
/// from the passed environment, and return it in the result. The resulting environment can then be
/// passed to the [`patch_field`] function.
pub fn rec_env<'a, I: Iterator<Item = (&'a Ident, &'a RichTerm)>>(
    bindings: I,
    env: &Environment,
) -> Result<Vec<(Ident, Thunk)>, EvalError> {
    bindings
        .map(|(id, rt)| match rt.as_ref() {
            Term::Var(ref var_id) => {
                let thunk = env
                    .get(var_id)
                    .ok_or_else(|| EvalError::UnboundIdentifier(var_id.clone(), rt.pos))?;
                Ok((id.clone(), thunk))
            }
            _ => {
                // If we are in this branch, the term must be a constant after the
                // share normal form transformation, hence it should not need an
                // environment, which is why it is dropped.
                let closure = Closure {
                    body: rt.clone(),
                    env: Environment::new(),
                };
                Ok((id.clone(), Thunk::new(closure, IdentKind::Let)))
            }
        })
        .collect()
}

fn patch_thunk(mut thunk: Thunk, rec_env: &Vec<(Ident, Thunk)>) {
    thunk.borrow_mut().env.extend(rec_env.iter().cloned());
}

/// Take a [`RichTerm`] that is stored inside a field, and patch its environment by enriching it
/// with the recursive environment.
pub fn patch_field(
    rt: &RichTerm,
    rec_env: &Vec<(Ident, Thunk)>,
    env: &Environment,
) -> Result<(), EvalError> {
    if let Term::Var(var_id) = &*rt.term {
        let thunk = env
            .get(var_id)
            .ok_or_else(|| EvalError::UnboundIdentifier(var_id.clone(), rt.pos))?;
        patch_thunk(thunk, rec_env);
    }
    // Thanks to the share normal form transformation, the content is either a
    // constant or a variable. In the constant case, the environment is irrelevant,
    // we we don't have to do anyting in the else case.
    Ok(())
}
