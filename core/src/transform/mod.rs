//! Various post transformations of nickel code.
use crate::{
    cache::ImportResolver,
    eval::{cache::Cache, Closure, Environment},
    match_sharedterm,
    term::{
        array::{Array, ArrayAttrs},
        record::Field,
        record::RecordData,
        BindingType, RichTerm, RuntimeContract, Term, Traverse, TraverseOrder,
    },
    transform::share_normal_form::should_share,
    typ::UnboundTypeVariableError,
    typecheck::Wildcards,
};

pub mod desugar_destructuring;
pub mod free_vars;
pub mod gen_pending_contracts;
pub mod import_resolution;
pub mod share_normal_form;
pub mod substitute_wildcards;

/// Apply all program transformations, excepted import resolution that is currently performed
/// earlier, as it needs to be done before typechecking.
///
/// Do not perform transformations on the imported files. If needed, either do it yourself using
/// pending imports returned by [`resolve_imports`][import_resolution::strict::resolve_imports] or
/// use the [cache][crate::cache::Cache].
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
        &mut |mut rt: RichTerm| -> Result<RichTerm, UnboundTypeVariableError> {
            // Start by substituting any wildcard with its inferred type
            if let Some(wildcards) = wildcards {
                rt = substitute_wildcards::transform_one(rt, wildcards);
            }
            // We desugar destructuring before other transformations, as this step generates new
            // record contracts and terms that must be themselves transformed.
            let rt = desugar_destructuring::transform_one(rt);
            Ok(rt)
        },
        TraverseOrder::TopDown,
    )?;

    Ok(rt
        .traverse(
            &mut |rt: RichTerm| -> Result<RichTerm, UnboundTypeVariableError> {
                // We need to do contract generation before the share normal form transformation,
                // because `gen_pending_contracts` generates record contracts
                //
                // `gen_pending_contracts` is applied bottom-up, because it might generate
                // additional terms down the AST (pending contracts pushed down the fields of a
                // record). In a top-down workflow, we would then visit those new duplicated nodes
                // (already visited as part of transforming the metadata), and do that potentially
                // again one level down. This results in a potentially non-linear cost in the size
                // of the AST. This was witnessed on Terraform-Nickel, causing examples using huge
                // auto-generated contracts (several of MBs) to not terminate in reasonable time.
                let rt = gen_pending_contracts::transform_one(rt)?;
                let rt = share_normal_form::transform_one(rt);
                Ok(rt)
            },
            TraverseOrder::BottomUp,
        )
        .unwrap())
}

/// Structures which can be packed together with their environment as a closure.
///
/// The typical implementer is [`crate::term::RichTerm`], but structures containing
/// terms can also be closurizable, such as the contract case in a [`crate::typ::Type`], an array
/// of terms, etc.
///
/// In those cases, the inner terms are closurized.
pub trait Closurizable : Sized {
    /// Pack a closurizable together with its environment `env` as a closure.
    ///
    /// By default, this is just `self.closurize_as_btype(cache, env, BindingType::default())`.
    fn closurize<C: Cache>(self, cache: &mut C, env: Environment) -> Self {
        self.closurize_as_btype(cache, env, BindingType::default())
    }

    /// Pack a closurizable together with its environment as a closure with the dependencies set
    /// according to the given binding type.
    /// An exception is made for constant term, which are left untouched, as it doesn't make sense
    /// to allocate a closure for them.
    fn closurize_as_btype<C: Cache>(
        self,
        cache: &mut C,
        env: Environment,
        btype: BindingType,
    ) -> Self;
}

impl Closurizable for RichTerm {
    /// Pack a term together with an environment as a closure.
    ///
    /// Generate a fresh variable, bind it to the corresponding closure `(t,with_env)` in `env`,
    /// and return this variable as a fresh term.
    fn closurize_as_btype<C: Cache>(
        self,
        cache: &mut C,
        env: Environment,
        btype: BindingType,
    ) -> RichTerm {
        // There is no case where closurizing a constant term makes sense, because it's already
        // evaluated, it doesn't have any free variables and doesn't contain any unevaluated terms.
        // Even the merge of recursive records is able to handle non-closurized constant terms, so
        // we just return the original term.
        if self.term.is_constant() {
            return self;
        }

        // If the term is already a variable, we don't have to create a useless intermediate
        // closure. We just transfer the original index to the new environment. This is not only an
        // optimization: this is relied upon by recursive record merging when computing the
        // fixpoint.
        //
        // More specifically, the evaluation of a recursive record patches the environment of each
        // field with the indices recursively referring to the other fields of the record. `eval`
        // assumes that a recursive record field is either a constant or a generated variable whose
        // cache elements *immediately* contain the original unevaluated expression (both properties
        // are true after the share normal form transformation and maintained when reverting
        // elements before merging recursive records).
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
        // fields of a record all contain generated variables (or constant), but never user-supplied
        // variables.
        // let var = LocIdent::fresh();
        let pos = self.pos;

        let idx = match_sharedterm! { self, with {
                // We should always find a generated variable in the environment, but this method is
                // not fallible, so we panic if we don't. We just wrap it in a new closure which will
                // give an unbound identifier error if it's ever evaluated.
                Term::Var(id) if id.is_generated() => {
                    env.get(&id.ident()).cloned().unwrap_or_else(|| {
                        debug_assert!(false, "missing generated variable {id} in environment");
                        cache.add(Closure { body: RichTerm::new(Term::Var(id), pos), env}, btype)
                    })
                },
                // If we just need a normal closure, and we find a normal closure inside the thunk, we can just reuse it
                Term::Closure(idx) if idx.deps().is_empty() && matches!(btype, BindingType::Normal) => idx,
            }
            else {
                // It's suspicious to wrap a closure with existing dependencies in a new closure
                // with set dependencies, although I'm not sure it would actually break anything.
                // We panic in debug mode to catch this case.
                debug_assert!(!matches!((self.as_ref(), &btype), (Term::Closure(idx), BindingType::Revertible(_)) if !idx.deps().is_empty()), "wrapping a closure with non-empty deps in a new closure with different deps");

                cache.add(Closure { body: self, env}, btype)
            }
        };

        RichTerm::new(Term::Closure(idx), pos.into_inherited())
    }
}

impl Closurizable for RuntimeContract {
    fn closurize_as_btype<C: Cache>(self, cache: &mut C, env: Environment, btype: BindingType) -> RuntimeContract {
        self.map_contract(|ctr| ctr.closurize_as_btype(cache, env, btype.clone()))
    }
}

impl Closurizable for Vec<RuntimeContract> {
    fn closurize_as_btype<C: Cache>(self, cache: &mut C, env: Environment, btype: BindingType) -> Vec<RuntimeContract> {
        self.into_iter()
            .map(|pending_contract| pending_contract.closurize_as_btype(cache, env.clone(), btype.clone()))
            .collect()
    }
}

impl Closurizable for Field {
    fn closurize_as_btype<C: Cache>(self, cache: &mut C, env: Environment, btype: BindingType) -> Field {
        let pending_contracts = self.pending_contracts.closurize_as_btype(cache, env.clone(), btype.clone());
        let value = self.value.map(|value| value.closurize_as_btype(cache, env, btype));

        Field {
            metadata: self.metadata,
            value,
            pending_contracts,
        }
    }
}

impl Closurizable for Array {
    fn closurize_as_btype<C: Cache>(self, cache: &mut C, env: Environment, btype: BindingType) -> Self {
        self.into_iter()
            .map(|t| {
                if should_share(&t.term) {
                    t.closurize_as_btype(cache, env.clone(), btype.clone())
                } else {
                    t
                }
            })
            .collect()
    }
}

impl Closurizable for ArrayAttrs {
    fn closurize_as_btype<C: Cache>(self, cache: &mut C, env: Environment, btype: BindingType) -> Self {
        let pending_contracts = if !self.closurized {
            self.pending_contracts.closurize_as_btype(cache, env.clone(), btype)
        } else {
            self.pending_contracts
        };

        ArrayAttrs {
            // closurized controls if the elements of the array is closurized or not. It could look
            // like we can set it to `true` here, but this method only ensures that the pending
            // contracts are, not the actual array elements. In practice it is always done as part
            // of closurizing the whole array, but we let the caller set the flag explicitly to
            // avoid surprises.
            closurized: self.closurized,
            pending_contracts,
        }
    }
}

impl Closurizable for (Array, ArrayAttrs) {
    fn closurize_as_btype<C: Cache>(self, cache: &mut C, env: Environment, btype: BindingType) -> Self {
        if self.1.closurized && matches!(btype, BindingType::Normal) {
            self
        } else {
            (
                self.0.closurize_as_btype(cache, env.clone(), btype.clone()),
                self.1.closurize_as_btype(cache, env, btype).closurized(),
            )
        }
    }
}

impl Closurizable for RecordData {
    fn closurize_as_btype<C: Cache>(self, cache: &mut C, env: Environment, btype: BindingType) -> Self {
        if !self.attrs.closurized {
            // We don't closurize the sealed tail, if any, because the underlying term is a private
            // field anyway, and is supposed to be closurized already.
            // TODO: should we change the type of SealedTail.term to CacheIndex to reflect that?
            RecordData {
                fields: self
                    .fields
                    .into_iter()
                    .map(|(id, field)| (id, field.closurize_as_btype(cache, env.clone(), btype.clone())))
                    .collect(),
                attrs: self.attrs.closurized(),
                ..self
            }
        } else {
            self
        }
    }
}
