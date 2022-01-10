use std::collections::HashSet;

use crate::cache::ImportResolver;
use crate::eval::{lazy::Thunk, Closure, Environment, IdentKind};
use crate::identifier::Ident;
use crate::term::{Contract, RichTerm, Term, TraverseMethod};
use crate::types::{AbsType, Types};
use simple_counter::*;

generate_counter!(FreshVarCounter, usize);

pub mod apply_contracts;
pub mod collect_free_vars;
pub mod desugar_destructuring;
pub mod import_resolution;
pub mod share_normal_form;
use collect_free_vars::collect_free_vars;

/// Apply all program transformations, excepted import resolution that is currently performed
/// earlier, as it needs to be done before typechecking.
///
/// Do not perform transformations on the imported files. If needed, either do it yourself using
/// pending imports returned by [`resolve_imports`](../fn.resolve_imports.html) or use the
/// [`Cache`](../../cache/struct.Cache.html)
pub fn transform(rt: RichTerm) -> RichTerm {
    let rt = rt
        .traverse(
            &mut |rt: RichTerm, _| -> Result<RichTerm, ()> {
                // before anything, we have to desugar the syntax
                let rt = desugar_destructuring::desugar_with_contract(rt);
                // We need to do contract generation before wrapping stuff in variables
                let rt = apply_contracts::transform_one(rt);
                Ok(rt)
            },
            &mut (),
            TraverseMethod::TopDown,
        )
        .unwrap();

    rt.traverse_monoid_state(
        &mut |rt: RichTerm, _, free_vars| -> Result<RichTerm, ()> {
            let mut rt = share_normal_form::transform_one(rt);
            collect_free_vars(&mut rt, free_vars);
            Ok(rt)
        },
        &mut (),
        &mut HashSet::new(),
        TraverseMethod::BottomUp,
    )
    .unwrap()
}

/// Generate a new fresh variable which do not clash with user-defined variables.
pub fn fresh_var() -> Ident {
    use crate::identifier::GEN_PREFIX;

    format!("{}{}", GEN_PREFIX, FreshVarCounter::next()).into()
}

/// Structures which can be packed together with their environment as a closure.
///
/// The typical implementer is [`RichTerm`](../term/enum.RichTerm.html), but structures containing
/// terms can also be closurizable, such as the contract in a [`Types`](../types/typ.Types.html).
/// In this case, the inner term is closurized.
pub trait Closurizable {
    /// Pack a closurizable together with its environment `with_env` as a closure in the main
    /// environment `env`.
    fn closurize(self, env: &mut Environment, with_env: Environment) -> Self;
}

impl Closurizable for RichTerm {
    /// Pack a term together with an environment as a closure.
    ///
    /// Generate a fresh variable, bind it to the corresponding closure `(t,with_env)` in `env`,
    /// and return this variable as a fresh term.
    fn closurize(self, env: &mut Environment, with_env: Environment) -> RichTerm {
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
        // normal form). We could do it for non-generated identifiers as well, but be we would be
        // renaming user-supplied variables by gibberish generated names. It may hamper error
        // reporting, so for the time being, we restrict ourselves to generated identifiers. Note
        // that performing or not performing this optimization for user-supplied variables doesn't
        // affect the invariant mentioned above, because the share normal form must ensure that the
        // fields of a record all contain generated variables (or constant), but never
        // user-supplied variables.
        let var = fresh_var();
        let pos = self.pos;

        let thunk = match self.as_ref() {
            Term::Var(id) if id.is_generated() => with_env.get(&id).expect(&format!(
                "Internal error(closurize) : generated identifier {} not found in the environment",
                id
            )),
            _ => {
                let closure = Closure {
                    body: self,
                    env: with_env,
                };
                Thunk::new(closure, IdentKind::Record)
            }
        };

        env.insert(var.clone(), thunk);
        RichTerm::new(Term::Var(var), pos.into_inherited())
    }
}

impl Closurizable for Types {
    /// Pack the contract of a type together with an environment as a closure.
    ///
    /// Extract the underlying contract, closurize it and wrap it back as a flat type (an opaque
    /// type defined by a custom contract).
    fn closurize(self, env: &mut Environment, with_env: Environment) -> Types {
        Types(AbsType::Flat(self.contract().closurize(env, with_env)))
    }
}

impl Closurizable for Contract {
    fn closurize(self, env: &mut Environment, with_env: Environment) -> Contract {
        Contract {
            types: self.types.closurize(env, with_env),
            label: self.label,
        }
    }
}
