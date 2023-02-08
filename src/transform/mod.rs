//! Various post transformations of nickel code.
use crate::{
    cache::ImportResolver,
    eval::{cache::Cache, Closure, Environment, IdentKind},
    identifier::Ident,
    term::{BindingType, LabeledType, RichTerm, Term, TraverseOrder},
    typecheck::Wildcards,
    types::{TypeF, Types, UnboundTypeVariableError},
};

pub mod apply_contracts;
pub mod desugar_destructuring;
pub mod free_vars;
pub mod import_resolution;
pub mod share_normal_form;
pub mod substitute_wildcards;

/// Apply all program transformations, excepted import resolution that is currently performed
/// earlier, as it needs to be done before typechecking.
///
/// Do not perform transformations on the imported files. If needed, either do it yourself using
/// pending imports returned by [`resolve_imports`][import_resolution::resolve_imports] or use the
/// [cache][crate::cache::Cache].
pub fn transform(
    mut rt: RichTerm,
    wildcards: Option<&Wildcards>,
) -> Result<RichTerm, UnboundTypeVariableError> {
    free_vars::transform(&mut rt);
    transform_no_free_vars(rt, wildcards)
}

/// Same as [`transform`], but doesn't apply the free vars transformation.
pub fn transform_no_free_vars(
    rt: RichTerm,
    wildcards: Option<&Wildcards>,
) -> Result<RichTerm, UnboundTypeVariableError> {
    let rt = rt.traverse(
        &|mut rt: RichTerm, _| -> Result<RichTerm, UnboundTypeVariableError> {
            // Start by substituting any wildcard with its inferred type
            if let Some(wildcards) = wildcards {
                rt = substitute_wildcards::transform_one(rt, wildcards);
            }
            // before anything, we have to desugar the syntax
            let rt = desugar_destructuring::transform_one(rt);
            // We need to do contract generation before wrapping stuff in variables
            let rt = apply_contracts::transform_one(rt)?;
            Ok(rt)
        },
        &mut (),
        TraverseOrder::TopDown,
    )?;

    Ok(rt
        .traverse(
            &|rt: RichTerm, _| -> Result<RichTerm, ()> {
                let rt = share_normal_form::transform_one(rt);
                Ok(rt)
            },
            &mut (),
            TraverseOrder::BottomUp,
        )
        .unwrap())
}

/// Structures which can be packed together with their environment as a closure.
///
/// The typical implementer is [`crate::term::RichTerm`], but structures containing
/// terms can also be closurizable, such as the contract in a [`crate::types::Types`].
/// In this case, the inner term is closurized.
pub trait Closurizable {
    /// Pack a closurizable together with its environment `with_env` as a closure in the main
    /// environment `env`.
    fn closurize<C: Cache>(
        self,
        cache: &mut C,
        env: &mut Environment,
        with_env: Environment,
    ) -> Self;
}

impl Closurizable for RichTerm {
    /// Pack a term together with an environment as a closure.
    ///
    /// Generate a fresh variable, bind it to the corresponding closure `(t,with_env)` in `env`,
    /// and return this variable as a fresh term.
    fn closurize<C: Cache>(
        self,
        cache: &mut C,
        env: &mut Environment,
        with_env: Environment,
    ) -> RichTerm {
        // If the term is already a variable, we don't have to create a useless intermediate
        // closure. We just transfer the original thunk to the new environment. This is not only an
        // optimization: this is relied upon by recursive record merging when computing the
        // fixpoint.
        //
        // More specifically, the evaluation of a recursive record patches the environment of each
        // field with the thunks recursively referring to the other fields of the record. `eval`
        // assumes that a recursive record field is either a constant or a generated variable whose
        // thunks *immediately* contain the original unevaluated expression (both properties are
        // true after the share normal form transformation and maintained when reverting thunks
        // before merging recursive records).
        //
        // To maintain this invariant, `closurize` must NOT introduce an indirection through a
        // variable, such as transforming:
        //
        // ```
        // {foo = %1, bar = 1} in env %1 <- 1 + bar
        // ```
        //
        // to:
        //
        // ```
        // {foo = %2, bar = 1} in env %2 <- (%1 in env %1 <- 1 + bar)
        // ```
        //
        // In this case, the evaluation of the recursive records will patch the outer environment
        // instead of the inner one, giving:
        //
        // ```
        // {foo = %2, bar = 1} in env %2 <- (%1 in env %1 <- 1 + bar), bar <- 1
        // ```
        //
        // Then, evaluating `foo` would unduly raise an unbound identifier error.
        //
        //
        // We currently only do this optimization for generated variables (introduced by the share
        // normal form). We could do it for non-generated identifiers as well, but we would be
        // renaming user-supplied variables by gibberish generated names. It may hamper error
        // reporting, so for the time being, we restrict ourselves to generated identifiers. Note
        // that performing or not performing this optimization for user-supplied variables doesn't
        // affect the invariant mentioned above, because the share normal form must ensure that the
        // fields of a record all contain generated variables (or constant), but never
        // user-supplied variables.
        let var = Ident::fresh();
        let pos = self.pos;

        let idx = match self.as_ref() {
            Term::Var(id) if id.is_generated() => with_env.get(id).cloned().unwrap_or_else(|| {
                panic!(
                "Internal error(closurize) : generated identifier {} not found in the environment",
                id
            )
            }),
            _ => {
                let closure: Closure = Closure {
                    body: self,
                    env: with_env,
                };
                cache.add(closure, IdentKind::Record, BindingType::Normal)
            }
        };

        env.insert(var, idx);
        RichTerm::new(Term::Var(var), pos.into_inherited())
    }
}

impl Closurizable for Types {
    /// Pack the contract of a type together with an environment as a closure.
    ///
    /// Extract the underlying contract, closurize it and wrap it back as a flat type (an opaque
    /// type defined by a custom contract).
    fn closurize<C: Cache>(
        self,
        cache: &mut C,
        env: &mut Environment,
        with_env: Environment,
    ) -> Types {
        Types(TypeF::Flat(
            self.contract().unwrap().closurize(cache, env, with_env),
        ))
    }
}

impl Closurizable for LabeledType {
    fn closurize<C: Cache>(
        self,
        cache: &mut C,
        env: &mut Environment,
        with_env: Environment,
    ) -> LabeledType {
        LabeledType {
            types: self.types.closurize(cache, env, with_env),
            label: self.label,
        }
    }
}
