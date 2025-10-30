//! Closurization of terms.
//!
//! During evaluation, variable substitution is done lazily through the help of
//! [the environment][crate::eval::Environment]. In consequence, the complete data required to
//! evaluate an expression further isn't only the expression itself (the term), but the current
//! environment as well. [crate::eval::Closure] is precisely a tuple of a term together with its
//! environment.
//!
//! We often need to store a closure back into a term (without relying on the environment):
//! typically, when we combine several operands (think merging or array concatenation), each with
//! its own environment. This is what we call closurization: wrap a closure back as a term. By
//! extension, several structures containing term can be closurized as well, wich means to
//! closurize all the inner terms.

use crate::{
    eval::{
        Closure, Environment,
        cache::Cache,
        value::{Array, ArrayData, Container, NickelValue, ValueContentRef},
    },
    term::{
        BindingType, RuntimeContract, Term,
        record::{Field, FieldDeps, RecordData, RecordDeps},
    },
};

/// Structures which can be packed together with their environment as a closure.
///
/// The typical implementer is [NickelValue], but structures containing terms can also be
/// closurizable, such as the contract case in a [crate::typ::Type], an array of terms, etc.
///
/// In those cases, the inner terms are closurized.
pub trait Closurize: Sized {
    /// Pack a closurizable together with its environment `env` as a closure.
    ///
    /// By default, this is just `self.closurize_as_btype(cache, env, BindingType::default())`.
    fn closurize<C: Cache>(self, cache: &mut C, env: Environment) -> Self {
        self.closurize_as_btype(cache, env, BindingType::default())
    }

    /// Pack a closurizable together with its environment as a closure and with the dependencies
    /// set according to the given binding type.
    /// An exception is made for constant terms, which should be left untouched (it's useless to to
    /// allocate a closure for them)
    fn closurize_as_btype<C: Cache>(
        self,
        cache: &mut C,
        env: Environment,
        btype: BindingType,
    ) -> Self;
}

impl Closurize for NickelValue {
    fn closurize_as_btype<C: Cache>(
        self,
        cache: &mut C,
        env: Environment,
        btype: BindingType,
    ) -> NickelValue {
        // There is no case where closurizing a constant term makes sense, because it's already
        // evaluated, it doesn't have any free variables and doesn't contain any unevaluated terms.
        // Even the merge of recursive records is able to handle non-closurized constant terms, so
        // we just return the original term.
        if self.is_constant() {
            return self;
        }

        // If the term is already a closure, we don't have to allocate a useless intermediate
        // closure. We just transfer the original cache index to the new environment. This is not
        // only an optimization: this is relied upon by recursive record merging when computing the
        // fixpoint.
        //
        // More specifically, the evaluation of a recursive record patches the environment of each
        // field with the indices recursively referring to the other fields of the record. `eval`
        // assumes that a recursive record field is either a constant or a `Term::Closure` whose
        // cache elements *immediately* contain the original unevaluated expression (both
        // properties are true after the first evaluation of a value through
        // `Term::Closurize(value)` and maintained when reverting elements before merging recursive
        // records).
        //
        // To maintain this invariant, `closurize` must NOT introduce an indirection through a
        // additional closure, such as transforming:
        //
        // ```
        // {foo = <closure@1>, bar = 1} | 1 <- 1 + bar
        // ```
        //
        // to:
        //
        // ```
        // {foo = <closure@2>, bar = 1} | 2 <- (<closure@1> | 1 <- 1 + bar)
        // ```
        //
        // In this case, the evaluation of the recursive records will patch the outer environment
        // instead of the inner one, giving:
        //
        // ```
        // {foo = <closure:2>, bar = 1} | 2 <- (<closure:1> | 1 <- 1 + bar), bar <- 1
        // ```
        //
        // Then, evaluating `foo` would unduly raise an unbound identifier error.
        let pos_idx = self.pos_idx();

        let idx = match self.content_ref() {
            // If we just need a normal closure, and we find a normal closure inside the thunk, we
            // reuse it
            ValueContentRef::Thunk(thunk)
                if thunk.deps().is_empty() && matches!(btype, BindingType::Normal) =>
            {
                thunk.clone()
            }
            ValueContentRef::Term(Term::Var(id)) if id.is_generated() => {
                let id = *id;
                // Albeit we should always find a generated variable in the environment,
                // `env.get` is technically fallible, while this method is not. We thus wrap a
                // potential error in a new closure which will propagate the  unbound
                // identifier error upon evaluation.
                env.get(&id.ident()).cloned().unwrap_or_else(move || {
                    debug_assert!(false, "missing generated variable {id} in environment");
                    cache.add(Closure { value: self, env }, btype)
                })
            }
            content => {
                // It's suspicious to wrap a closure with existing dependencies in a new closure,
                // although I'm not sure it would actually break anything. We panic in debug mode
                // to catch this case.
                debug_assert!(
                    !matches!((content, &btype), (ValueContentRef::Thunk(thunk), BindingType::Revertible(_)) if !thunk.deps().is_empty()),
                    "wrapping a closure with non-empty deps in a new closure with different deps"
                );

                cache.add(Closure { value: self, env }, btype)
            }
        };

        NickelValue::thunk(idx, pos_idx)
    }
}

impl Closurize for RuntimeContract {
    fn closurize_as_btype<C: Cache>(
        self,
        cache: &mut C,
        env: Environment,
        btype: BindingType,
    ) -> RuntimeContract {
        self.map_contract(|ctr| ctr.closurize_as_btype(cache, env, btype.clone()))
    }
}

impl Closurize for Vec<RuntimeContract> {
    fn closurize_as_btype<C: Cache>(
        self,
        cache: &mut C,
        env: Environment,
        btype: BindingType,
    ) -> Vec<RuntimeContract> {
        self.into_iter()
            .map(|pending_contract| {
                pending_contract.closurize_as_btype(cache, env.clone(), btype.clone())
            })
            .collect()
    }
}

impl Closurize for Field {
    fn closurize_as_btype<C: Cache>(
        self,
        cache: &mut C,
        env: Environment,
        btype: BindingType,
    ) -> Field {
        let pending_contracts =
            self.pending_contracts
                .closurize_as_btype(cache, env.clone(), btype.clone());
        let value = self
            .value
            .map(|value| value.closurize_as_btype(cache, env, btype));

        Field {
            metadata: self.metadata,
            value,
            pending_contracts,
        }
    }
}

impl Closurize for Array {
    fn closurize_as_btype<C: Cache>(
        self,
        cache: &mut C,
        env: Environment,
        btype: BindingType,
    ) -> Self {
        self.into_iter()
            .map(|val| {
                if should_share(&val) {
                    val.closurize_as_btype(cache, env.clone(), btype.clone())
                } else {
                    val
                }
            })
            .collect()
    }
}

// Closurize an array together with its pending contracts
impl Closurize for ArrayData {
    fn closurize_as_btype<C: Cache>(
        self,
        cache: &mut C,
        env: Environment,
        btype: BindingType,
    ) -> Self {
        //TODO[RFC007]: we need to handle the case where the array is already closurized but is
        //revertible at the callsite, now.

        let pending_contracts =
            self.pending_contracts
                .closurize_as_btype(cache, env.clone(), btype.clone());

        ArrayData {
            array: self
                .array
                .closurize_as_btype(cache, env.clone(), btype.clone()),
            pending_contracts,
        }
    }
}

impl Closurize for RecordData {
    fn closurize_as_btype<C: Cache>(
        self,
        cache: &mut C,
        env: Environment,
        btype: BindingType,
    ) -> Self {
        // We don't closurize the sealed tail, if any, because the underlying term is a private
        // field and is supposed to be already closurized.
        // TODO: should we change the type of SealedTail.term to CacheIndex to reflect that?
        RecordData {
            fields: self
                .fields
                .into_iter()
                .map(|(id, field)| {
                    (
                        id,
                        field.closurize_as_btype(cache, env.clone(), btype.clone()),
                    )
                })
                .collect(),
            ..self
        }
    }
}

pub fn should_share(value: &NickelValue) -> bool {
    match value.content_ref() {
        ValueContentRef::Term(Term::Var(_)
            | Term::Fun(_, _)
            // match acts like a function, and is a WHNF
            | Term::Match(_)) => false,
        ValueContentRef::Term(_) => true,
        ValueContentRef::Null
        | ValueContentRef::Bool(_)
        | ValueContentRef::Array(Container::Empty)
        | ValueContentRef::Record(Container::Empty)
        | ValueContentRef::Number(_)
        | ValueContentRef::String(_)
        | ValueContentRef::Label(_)
        | ValueContentRef::SealingKey(_)
        | ValueContentRef::Thunk(_)
        | ValueContentRef::Type(_) => false,
        ValueContentRef::EnumVariant(enum_variant) => enum_variant.arg.is_some(),
        _ => true,
    }
}

/// Closurize all the inner terms of a recursive record, including pending contracts and
/// dynamically defined fields.
pub fn closurize_rec_record<C: Cache>(
    cache: &mut C,
    data: RecordData,
    dyn_fields: Vec<(NickelValue, Field)>,
    deps: Option<RecordDeps>,
    env: Environment,
) -> (RecordData, Vec<(NickelValue, Field)>) {
    let fields = data
        .fields
        .into_iter()
        .map(|(id, field)| {
            let field_deps = deps
                .as_ref()
                .and_then(|deps| deps.stat_fields.get(&id.ident()))
                .cloned();

            (
                id,
                field.closurize_as_btype(cache, env.clone(), mk_binding_type(field_deps.clone())),
            )
        })
        .collect();

    let dyn_fields = dyn_fields
        .into_iter()
        .enumerate()
        .map(|(index, (id_t, field))| {
            let field_deps = deps
                .as_ref()
                .and_then(|deps| deps.dyn_fields.get(index))
                .cloned();
            // Identifier expressions aren't currently allowed to recursively depend on another
            // field, so we closurize them as normal bindings.
            (
                id_t.closurize(cache, env.clone()),
                field.closurize_as_btype(cache, env.clone(), mk_binding_type(field_deps.clone())),
            )
        })
        .collect();

    let data = RecordData { fields, ..data };

    (data, dyn_fields)
}

fn mk_binding_type(field_deps: Option<FieldDeps>) -> BindingType {
    // If the fields has an empty set of dependencies, we can eschew the useless introduction of a
    // revertible element. Note that `field_deps` being `None` doesn't mean "empty dependencies"
    // but rather that the dependencies haven't been computed. In the latter case, we must be
    // conservative and assume the field can depend on any other field.
    let is_non_rec = field_deps
        .as_ref()
        .map(FieldDeps::is_empty)
        .unwrap_or(false);
    if is_non_rec {
        BindingType::Normal
    } else {
        BindingType::Revertible(field_deps.unwrap_or(FieldDeps::Unknown))
    }
}
