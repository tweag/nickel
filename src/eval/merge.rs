//! Evaluation of the merge operator.
//!
//! Merge is a primitive operation of Nickel, which recursively combines records. Together with
//! enriched values, it allows to write and mix contracts with standard records.
//!
//! # Operational semantics
//!
//! ## On records
//!
//! When records `r1` and `r2` are merged, the result is a new record with the following fields:
//! - All the fields of `r1` that are not in `r2`
//! - All the fields of `r2` that are not in `r1`
//! - Fields that are both in `r1` and `r2` are recursively merged: for a field `f`, the result
//! contains the binding `f = r1.f & r2.f`
//!
//! As fields are recursively merged, merge needs to operate on any value, not only on records.
//!
//! ## On simple values
//!
//! Simple values are terms which are not enriched values.
//!
//! - *Function*: merging a function with anything else fails
//! - *Values*: merging any other values succeeds if and only if these two values are equals, in which case it evaluates to
//! this common value.
//!
//! Note that merging of arrays is not yet implemented.
//!
//! ## On enriched values
//!
//! Enriched values (currently `Contract`, `Default`, `ContractDefault` or `Docstring`) get their
//! special powers from their interaction with the merge operator.
//!
//! ### Enriched/Enriched
//!
//! - *Contract/contract*: merging two contracts evaluates to a contract which is the composition
//! of the two
//! - *Default/default*: merging two default values evaluates to a default which value is the merge
//! of the two
//! - *Contract/default*: merging a `Default` with a `Contract` evaluates to a `ContractDefault`
//! - *ContractDefault/_*: Merging `ContractDefault` is done component-wise: with another
//! `ContractDefault`, it evaluates to a `ContractDefault` where the two contracts as well as the
//! two default values are respectively merged together. With either just a `Contract` or a
//! `Default`, it simply merges the corresponding component and let the other unchanged.
//!
//! ### Enriched/Simple
//!
//! - *Docstring*: merging a docstring (with inner term `inner`) with another term `t` recursively merges
//! `inner` and `t`, and evaluates to this result wrapped in the original docstring (`t` may be a simple value or an
//! enriched one here)
//! - *Default erasure*: merging a `Default` with a simple value drops the default value and
//! evaluates to the simple value
//! - *Contract check*: merging a `Contract` or a `ContractDefault` with a simple value `t`
//! evaluates to a contract check, that is an `Assume(..., t)`
use super::*;
use crate::error::{EvalError, IllegalPolymorphicTailAction};
use crate::label::Label;
use crate::position::TermPos;
use crate::term::{
    make as mk_term,
    record::{self, Field, FieldDeps, FieldMetadata, RecordAttrs, RecordData},
    BinaryOp, LabeledType, RichTerm, SharedTerm, Term, TypeAnnotation,
};
use crate::types::{TypeF, Types};
use std::collections::HashMap;

/// Merging mode. Merging is used both to combine standard data and to apply contracts defined as
/// records.
#[derive(Clone, PartialEq, Debug)]
pub enum MergeMode {
    /// Standard merging, for combining data.
    Standard,
    /// Merging to apply a record contract to a value, with the associated label.
    Contract(Label),
}

impl Default for MergeMode {
    fn default() -> Self {
        MergeMode::Standard
    }
}

/// Compute the merge of two evaluated operands. Support both standard merging and record contract
/// application.
///
/// # Mode
///
/// In [`MergeMode::Contract`] mode, `t1` must be the value and `t2` must be the contract. It is
/// important as `merge` is not commutative in this mode.
#[allow(clippy::too_many_arguments)] // TODO: Is it worth to pack the inputs in an ad-hoc struct?
pub fn merge<C: Cache>(
    cache: &mut C,
    t1: RichTerm,
    env1: Environment,
    t2: RichTerm,
    env2: Environment,
    pos_op: TermPos,
    mode: MergeMode,
    call_stack: &mut CallStack,
) -> Result<Closure, EvalError> {
    let RichTerm {
        term: t1,
        pos: pos1,
    } = t1;
    let RichTerm {
        term: t2,
        pos: pos2,
    } = t2;

    match (t1.into_owned(), t2.into_owned()) {
        // Merge is idempotent on basic terms
        (Term::Null, Term::Null) => Ok(Closure::atomic_closure(RichTerm::new(
            Term::Null,
            pos_op.into_inherited(),
        ))),
        (Term::Bool(b1), Term::Bool(b2)) => {
            if b1 == b2 {
                Ok(Closure::atomic_closure(RichTerm::new(
                    Term::Bool(b1),
                    pos_op.into_inherited(),
                )))
            } else {
                Err(EvalError::MergeIncompatibleArgs(
                    RichTerm {
                        term: SharedTerm::new(Term::Bool(b1)),
                        pos: pos1,
                    },
                    RichTerm {
                        term: SharedTerm::new(Term::Bool(b2)),
                        pos: pos2,
                    },
                    pos_op,
                ))
            }
        }
        (Term::Num(n1), Term::Num(n2)) => {
            if (n1 - n2).abs() < f64::EPSILON {
                Ok(Closure::atomic_closure(RichTerm::new(
                    Term::Num(n1),
                    pos_op.into_inherited(),
                )))
            } else {
                Err(EvalError::MergeIncompatibleArgs(
                    RichTerm {
                        term: SharedTerm::new(Term::Num(n1)),
                        pos: pos1,
                    },
                    RichTerm {
                        term: SharedTerm::new(Term::Num(n2)),
                        pos: pos2,
                    },
                    pos_op,
                ))
            }
        }
        (Term::Str(s1), Term::Str(s2)) => {
            if s1 == s2 {
                Ok(Closure::atomic_closure(RichTerm::new(
                    Term::Str(s1),
                    pos_op.into_inherited(),
                )))
            } else {
                Err(EvalError::MergeIncompatibleArgs(
                    RichTerm {
                        term: SharedTerm::new(Term::Str(s1)),
                        pos: pos1,
                    },
                    RichTerm {
                        term: SharedTerm::new(Term::Str(s2)),
                        pos: pos2,
                    },
                    pos_op,
                ))
            }
        }
        (Term::Lbl(l1), Term::Lbl(l2)) => {
            if l1 == l2 {
                Ok(Closure::atomic_closure(RichTerm::new(
                    Term::Lbl(l1),
                    pos_op.into_inherited(),
                )))
            } else {
                Err(EvalError::MergeIncompatibleArgs(
                    RichTerm {
                        term: SharedTerm::new(Term::Lbl(l1)),
                        pos: pos1,
                    },
                    RichTerm {
                        term: SharedTerm::new(Term::Lbl(l2)),
                        pos: pos2,
                    },
                    pos_op,
                ))
            }
        }
        (Term::Enum(i1), Term::Enum(i2)) => {
            if i1 == i2 {
                Ok(Closure::atomic_closure(RichTerm::new(
                    Term::Enum(i1),
                    pos_op.into_inherited(),
                )))
            } else {
                Err(EvalError::MergeIncompatibleArgs(
                    RichTerm {
                        term: SharedTerm::new(Term::Enum(i1)),
                        pos: pos1,
                    },
                    RichTerm {
                        term: SharedTerm::new(Term::Enum(i2)),
                        pos: pos2,
                    },
                    pos_op,
                ))
            }
        }
        (Term::Array(arr1, _attrs1), Term::Array(arr2, _attrs2))
            if arr1.is_empty() && arr2.is_empty() =>
        {
            Ok(Closure::atomic_closure(RichTerm::new(
                Term::Array(arr1, ArrayAttrs::new().closurized()),
                pos_op.into_inherited(),
            )))
        }
        // Merge put together the fields of records, and recursively merge
        // fields that are present in both terms
        (Term::Record(r1), Term::Record(r2)) => {
            // While it wouldn't be impossible to merge records with sealed tails,
            // working out how to do so in a "sane" way that preserves parametricity
            // is non-trivial. It's also not entirely clear that this is something
            // users will generally have reason to do, so in the meantime we've
            // decided to just prevent this entirely
            if let Some(record::SealedTail { label, .. }) = r1.sealed_tail.or(r2.sealed_tail) {
                return Err(EvalError::IllegalPolymorphicTailAccess {
                    action: IllegalPolymorphicTailAction::Merge,
                    evaluated_arg: label.get_evaluated_arg(cache),
                    label,
                    call_stack: std::mem::take(call_stack),
                });
            }

            let hashmap::SplitResult {
                left,
                center,
                right,
            } = hashmap::split(r1.fields, r2.fields);

            match mode {
                MergeMode::Contract(mut lbl) if !r2.attrs.open && !left.is_empty() => {
                    let fields: Vec<String> =
                        left.keys().map(|field| format!("`{}`", field)).collect();
                    let plural = if fields.len() == 1 { "" } else { "s" };
                    lbl.tag = format!("extra field{} {}", plural, fields.join(","));
                    return Err(EvalError::BlameError {
                        evaluated_arg: lbl.get_evaluated_arg(cache),
                        label: lbl,
                        call_stack: CallStack::new(),
                    });
                }
                _ => (),
            };

            let field_names: Vec<_> = left
                .keys()
                .chain(center.keys())
                .chain(right.keys())
                .cloned()
                .collect();
            let mut m = HashMap::with_capacity(left.len() + center.len() + right.len());
            let mut env = Environment::new();

            // Merging recursive records is the one operation that may override recursive fields. To
            // have the recursive fields depend on the updated values, we need to revert the
            // corresponding thunks to their original expression.
            //
            // We do that for the left and the right part.
            //
            // The fields in the intersection (center) need a slightly more general treatment to
            // correctly propagate the recursive values down each field: saturation. See
            // [crate::eval::lazy::Thunk::saturate].
            m.extend(
                left.into_iter()
                    .map(|(id, field)| (id, field.revert_closurize(cache, &mut env, env1.clone()))),
            );

            m.extend(
                right
                    .into_iter()
                    .map(|(id, field)| (id, field.revert_closurize(cache, &mut env, env2.clone()))),
            );

            for (id, (field1, field2)) in center.into_iter() {
                m.insert(
                    id,
                    merge_fields(
                        cache,
                        field1,
                        env1.clone(),
                        field2,
                        env2.clone(),
                        &mut env,
                        field_names.iter(),
                    )?,
                );
            }

            let final_pos = if mode == MergeMode::Standard {
                pos_op.into_inherited()
            } else {
                pos1.into_inherited()
            };

            Ok(Closure {
                body: RichTerm::new(
                    // We don't have to provide RecordDeps, which are required in a previous stage
                    // of program transformations. At this point, the interpreter doesn't care
                    // about them anymore, and dependencies are stored at the level of revertible
                    // thunks directly.
                    Term::RecRecord(
                        RecordData::new(m, RecordAttrs::merge(r1.attrs, r2.attrs), None),
                        Vec::new(),
                        None,
                    ),
                    final_pos,
                ),
                env,
            })
        }
        (t1_, t2_) => match (mode, &t2_) {
            // We want to merge a non-record term with a record contract
            (MergeMode::Contract(label), Term::Record(..)) => Err(EvalError::BlameError {
                evaluated_arg: label.get_evaluated_arg(cache),
                label,
                call_stack: call_stack.clone(),
            }),
            // The following cases are either errors or not yet implemented
            _ => Err(EvalError::MergeIncompatibleArgs(
                RichTerm {
                    term: SharedTerm::new(t1_),
                    pos: pos1,
                },
                RichTerm {
                    term: SharedTerm::new(t2_),
                    pos: pos2,
                },
                pos_op,
            )),
        },
    }
}

/// Apply a series of contract to the value of a field as well as the necessary saturation (see
/// [saturate]) and closurize operations.
///
/// Both the contract and the field are saturated, and the result is closurized. The result is
/// revertible if either one of the contract or the record is, with the union of their
/// dependencies.
///
/// # Parameters
///
/// - the value is given by `value1` in its environment `env1`
/// - the contracts are given as an iterator `it2` together with their (common) environment `env2`
fn cross_apply_contracts<'a, 'b, C: Cache, I: DoubleEndedIterator<Item = &'b Ident> + Clone>(
    cache: &mut C,
    value1: RichTerm,
    env1: Environment,
    mut it2: impl Iterator<Item = &'a LabeledType>,
    env2: &Environment,
    deps2: FieldDeps,
    fields: &I,
) -> Result<Closure, EvalError> {
    // The environment of `%assume% ctr1 (%assume% ctr2 (... field)...)`. `ctr` and `field` will be saturated, ie of the form
    // `%head% field1 .. fieldn`.
    let mut local_env = Environment::new();

    let pos = value1.pos.into_inherited();
    let combined_deps = field_deps(cache, &value1, &env1)?.union(deps2);

    // Produce the concrete sequence of application of the `assume` primop to the contract argument.
    let value_saturated = value1.saturate(cache, &mut local_env, &env1, fields.clone())?;
    let body = it2.try_fold(value_saturated, |acc, ctr| -> Result<RichTerm, EvalError> {
        let ty_saturated = ctr
            .types
            .clone()
            .saturate(cache, &mut local_env, &env2, fields.clone())
            .unwrap();
        Ok(mk_term::assume(ty_saturated, ctr.label.clone(), acc).map(|rt| rt.with_pos(pos))?)
    })?;

    // We closurize the final result in a new thunk with appropriate dependencies and
    // revertibility.
    let closure = Closure {
        body,
        env: local_env,
    };
    let mut final_env = Environment::new();
    let fresh_var = Ident::fresh();

    // new_rev takes care of not creating a revertible thunk if the dependencies are empty.
    final_env.insert(
        fresh_var,
        cache.add(
            closure,
            IdentKind::Record,
            BindingType::Revertible(combined_deps),
        ),
    );

    Ok(Closure {
        body: RichTerm::from(Term::Var(fresh_var)),
        env: final_env,
    })
}

/// Take two record fields in their respective environment and combine both their metadat and
/// values. Apply the required saturate, revert or closurize operation, including on the final
/// field returned.
fn merge_fields<'a, C: Cache, I: DoubleEndedIterator<Item = &'a Ident> + Clone>(
    cache: &mut C,
    field1: Field,
    env1: Environment,
    field2: Field,
    env2: Environment,
    env_final: &mut Environment,
    fields: I,
) -> Result<Field, EvalError> {
    // For now, we blindly closurize things and copy environments in this function. A
    // careful analysis would make it possible to spare a few closurize operations and more
    // generally environment cloning.
    let Field {
        metadata: metadata1,
        value: value1,
    } = field1;
    let Field {
        metadata: metadata2,
        value: value2,
    } = field2;

    // Cross-application (in the process of being removed, see RFC005)
    //
    // If:
    // 1. field1 has a value
    // 2. field2 has a contract
    // 3. The priorities (or the fact that field2's value is not defined) are such that
    //    field1's value will be used in the final value
    //
    // Then, we apply field2's contracts to field1. This creates a new value and a new
    // intermediate environment.
    let (value1, val_env1) = match (value1, metadata2.annotation.first()) {
        (Some(v1), Some(fst_ctr))
            if (metadata1.priority >= metadata2.priority || value2.is_none()) =>
        {
            let Closure { body, env } = cross_apply_contracts(
                cache,
                v1,
                env1.clone(),
                metadata2.annotation.iter(),
                &env2,
                ty_field_deps(cache, &fst_ctr.types, &env2).unwrap(),
                &fields,
            )?;
            (Some(body), env)
        }
        (v1, _) => (v1, env1.clone()),
    };

    // Cross-application (in the process of being removed, see RFC005)
    //
    // Dually, we cross apply meta1's contracts to meta2's value.
    let (value2, val_env2) = match (value2, metadata1.annotation.first()) {
        (Some(v2), Some(fst_ctr))
            if (metadata2.priority >= metadata1.priority || value1.is_none()) =>
        {
            let Closure { body, env } = cross_apply_contracts(
                cache,
                v2,
                env2.clone(),
                metadata1.annotation.iter(),
                &env1,
                ty_field_deps(cache, &fst_ctr.types, &env1)?,
                &fields,
            )?;
            (Some(body), env)
        }
        (v2, _) => (v2, env2.clone()),
    };

    // Selecting either meta1's value, meta2's value, or the merge of the two values,
    // depending on which is defined and respective priorities.
    let (value, priority) = match (value1, value2) {
        (Some(t1), Some(t2)) if metadata1.priority == metadata2.priority => (
            Some(
                fields_merge_closurize(cache, env_final, t1, &val_env1, t2, &val_env2, fields)
                    .unwrap(),
            ),
            metadata1.priority,
        ),
        (Some(t1), _) if metadata1.priority > metadata2.priority => (
            Some(t1.revert_closurize(cache, env_final, val_env1.clone())),
            metadata1.priority,
        ),
        (Some(t1), None) => (
            Some(t1.revert_closurize(cache, env_final, val_env1.clone())),
            metadata1.priority,
        ),
        (_, Some(t2)) if metadata2.priority > metadata1.priority => (
            Some(t2.revert_closurize(cache, env_final, val_env2.clone())),
            metadata2.priority,
        ),
        (None, Some(t2)) => (
            Some(t2.revert_closurize(cache, env_final, val_env2.clone())),
            metadata2.priority,
        ),
        (None, None) => (None, Default::default()),
        _ => unreachable!(),
    };

    let mut annot1 = metadata1
        .annotation
        .revert_closurize(cache, env_final, env1.clone());
    let mut annot2 = metadata2
        .annotation
        .revert_closurize(cache, env_final, env2.clone());

    // If both have type annotations, we arbitrarily choose the first one. At this point we are
    // evaluating, and types annotations and contracts make no difference operationnally. Even for
    // a query, it's strange to show multiple static types. So if both are set, we turn types2 to a
    // contract and keep type1 as the main type annotation.
    let types = match (annot1.types.take(), annot2.types.take()) {
        (Some(ctr1), Some(ctr2)) => {
            annot1.contracts.push(ctr2);
            Some(ctr1)
        }
        (ty1, ty2) => ty1.or(ty2),
    };

    let contracts: Vec<_> = annot1
        .contracts
        .into_iter()
        .chain(annot2.contracts.into_iter())
        .collect();
    let metadata = FieldMetadata {
        doc: merge_doc(metadata1.doc, metadata2.doc),
        annotation: TypeAnnotation { types, contracts },
        // If one of the record requires this field, then it musn't be optional. The
        // resulting field is optional iff both are.
        opt: metadata1.opt && metadata2.opt,
        priority,
    };

    Ok(Field { metadata, value })
}

/// Merge two optional documentations.
fn merge_doc(doc1: Option<String>, doc2: Option<String>) -> Option<String> {
    //FIXME: how to merge documentation? Just concatenate?
    doc1.or(doc2)
}

/// See [crate::eval::lazy::Thunk::saturate]. Saturation is a transformation on recursive thunks is
/// used when we must combine different values with different recursive dependencies (say, the two
/// values of fields being merged) into one expression.
///
/// Saturation is first and foremost a transformation of terms, but like
/// [crate::transform::Closurizable], it can be applied to other types that contain terms, hence
/// the trait.
trait Saturate: Sized {
    /// Take the content of a record field, and saturate the potential revertible thunk with the given
    /// fields. See [crate::eval::lazy::Thunk::saturate].
    ///
    /// If the expression is not a variable referring to a thunk (this can happen e.g. for numeric
    /// constants), we just return the term as it is, which falls into the zero dependencies special
    /// case.
    fn saturate<'a, I: DoubleEndedIterator<Item = &'a Ident> + Clone, C: Cache>(
        self,
        cache: &mut C,
        env: &mut Environment,
        local_env: &Environment,
        fields: I,
    ) -> Result<Self, EvalError>;
}

impl Saturate for RichTerm {
    fn saturate<'a, I: DoubleEndedIterator<Item = &'a Ident> + Clone, C: Cache>(
        self,
        cache: &mut C,
        env: &mut Environment,
        local_env: &Environment,
        fields: I,
    ) -> Result<RichTerm, EvalError> {
        if let Term::Var(var_id) = &*self.term {
            let idx = local_env
                .get(var_id)
                .cloned()
                .ok_or(EvalError::UnboundIdentifier(*var_id, self.pos))?;

            Ok(cache.saturate(idx, env, fields).with_pos(self.pos))
        } else {
            Ok(self)
        }
    }
}

impl Saturate for Types {
    fn saturate<'a, I: DoubleEndedIterator<Item = &'a Ident> + Clone, C: Cache>(
        self,
        cache: &mut C,
        env: &mut Environment,
        local_env: &Environment,
        fields: I,
    ) -> Result<Types, EvalError> {
        match self {
            // Currently, either:
            // 1. the contract is part of a recursive records and MUST have been closurized as a
            //    revertible thunk during program transformation:
            Types(TypeF::Flat(ctr)) if matches!(ctr.as_ref(), Term::Var(id) if id.is_generated()) => {
                Ok(Types(TypeF::Flat(
                    ctr.saturate(cache, env, local_env, fields)?,
                )))
            }
            // 2. or it is the contract of a non-recursive record (e.g. the contract generated by
            //    pattern matching or desugaring `foo.bar.baz = value`) and doesn't need to be
            //    saturated at all, as it can't be recursive. This contract can still capture
            //    variables from the environment, so we need to closurize it nontheless
            ty => Ok(ty.closurize(cache, env, local_env.clone())),
        }
    }
}

/// Return the dependencies of a field when represented as a `RichTerm`.
fn field_deps<C: Cache>(
    cache: &C,
    rt: &RichTerm,
    local_env: &Environment,
) -> Result<FieldDeps, EvalError> {
    if let Term::Var(var_id) = &*rt.term {
        local_env
            .get(var_id)
            .map(|idx| cache.deps(idx).unwrap_or_else(FieldDeps::empty))
            .ok_or(EvalError::UnboundIdentifier(*var_id, rt.pos))
    } else {
        Ok(FieldDeps::empty())
    }
}

/// Return the dependencies of a contract when represented as a `Types`.
fn ty_field_deps<C: Cache>(
    cache: &C,
    ty: &Types,
    local_env: &Environment,
) -> Result<FieldDeps, EvalError> {
    if let Types(TypeF::Flat(ctr)) = ty {
        field_deps(cache, ctr, local_env)
    } else {
        Ok(FieldDeps::empty())
    }
}

/// Return the dependencies of a contract when represented as a `Type`.

/// Take the current environment, two fields with their local environment, and return a term which
/// is the merge of the two fields, closurized in the provided final environment.
///
/// The thunk allocated for the result is revertible if and only if at least one of the original
/// thunks is (if one of the original values is overridable, then so is the merge of the two). In
/// this case, the field dependencies are the union of the dependencies of each field.
///
/// The fields are saturated (see [saturate]) to properly propagate recursive dependencies down to
/// `t1` and `t2` in the final, merged record.
fn fields_merge_closurize<'a, I: DoubleEndedIterator<Item = &'a Ident> + Clone, C: Cache>(
    cache: &mut C,
    env: &mut Environment,
    t1: RichTerm,
    env1: &Environment,
    t2: RichTerm,
    env2: &Environment,
    fields: I,
) -> Result<RichTerm, EvalError> {
    let mut local_env = Environment::new();

    let combined_deps = field_deps(cache, &t1, env1)?.union(field_deps(cache, &t2, env2).unwrap());
    let body = RichTerm::from(Term::Op2(
        BinaryOp::Merge(),
        t1.saturate(cache, &mut local_env, env1, fields.clone())?,
        t2.saturate(cache, &mut local_env, env2, fields)?,
    ));

    // We closurize the final result in a thunk with appropriate dependencies
    let closure = Closure {
        body,
        env: local_env,
    };
    let fresh_var = Ident::fresh();

    // new_rev takes care of not creating a revertible thunk if the dependencies are empty.
    env.insert(
        fresh_var,
        cache.add(
            closure,
            IdentKind::Record,
            BindingType::Revertible(combined_deps),
        ),
    );

    Ok(RichTerm::from(Term::Var(fresh_var)))
}

/// Same as [Closurizable], but also revert the thunk if the term is a variable.
trait RevertClosurize {
    /// Revert the thunk inside the term (if any), and closurize the result inside `env`.
    fn revert_closurize<C: Cache>(
        self,
        cache: &mut C,
        env: &mut Environment,
        with_env: Environment,
    ) -> Self;
}

impl RevertClosurize for RichTerm {
    fn revert_closurize<C: Cache>(
        self,
        cache: &mut C,
        env: &mut Environment,
        with_env: Environment,
    ) -> RichTerm {
        if let Term::Var(id) = self.as_ref() {
            //TODO: actually use the cache after rebase
            // This create a fresh variable which is bound to a reverted copy of the original thunk
            let reverted = cache.revert(with_env.get(id).unwrap());
            let fresh_id = Ident::fresh();
            env.insert(fresh_id, reverted);
            RichTerm::new(Term::Var(fresh_id), self.pos)
        } else {
            // Otherwise, if it is not a variable after the share normal form transformations, it
            // should be a constant and we don't need to revert anything
            self
        }
    }
}

impl RevertClosurize for Field {
    fn revert_closurize<C: Cache>(
        self,
        cache: &mut C,
        env: &mut Environment,
        with_env: Environment,
    ) -> Field {
        let metadata = self.metadata.revert_closurize(cache, env, with_env.clone());
        let value = self
            .value
            .map(|value| value.revert_closurize(cache, env, with_env));

        Field { metadata, value }
    }
}

impl RevertClosurize for Types {
    fn revert_closurize<C: Cache>(
        self,
        cache: &mut C,
        env: &mut Environment,
        with_env: Environment,
    ) -> Types {
        match self {
            // Currently, either:
            // 1. the contract is part of a recursive records and MUST have been closurized as a
            //    revertible thunk during program transformation:
            Types(TypeF::Flat(ctr)) if matches!(ctr.as_ref(), Term::Var(id) if id.is_generated()) => {
                Types(TypeF::Flat(ctr.revert_closurize(cache, env, with_env)))
            }
            // 2. or it is the contract of a non-recursive record (e.g. the contract generated by
            //    pattern matching or desugaring `foo.bar.baz = value`) and doesn't need to be
            //    saturated at all, as it can't be recursive. This contract can still capture
            //    variables from the environment, so we need to closurize it nontheless
            ty => ty.closurize(cache, env, with_env),
        }
    }
}

impl RevertClosurize for LabeledType {
    fn revert_closurize<C: Cache>(
        self,
        cache: &mut C,
        env: &mut Environment,
        with_env: Environment,
    ) -> LabeledType {
        LabeledType {
            types: self.types.revert_closurize(cache, env, with_env),
            label: self.label,
        }
    }
}

impl RevertClosurize for TypeAnnotation {
    fn revert_closurize<C: Cache>(
        self,
        cache: &mut C,
        env: &mut Environment,
        with_env: Environment,
    ) -> TypeAnnotation {
        let types = self
            .types
            .map(|ty| ty.revert_closurize(cache, env, with_env.clone()));
        let contracts = self
            .contracts
            .into_iter()
            .map(|labeled_ty| labeled_ty.revert_closurize(cache, env, with_env.clone()))
            .collect();

        TypeAnnotation { types, contracts }
    }
}

impl RevertClosurize for FieldMetadata {
    fn revert_closurize<C: Cache>(
        self,
        cache: &mut C,
        env: &mut Environment,
        with_env: Environment,
    ) -> FieldMetadata {
        FieldMetadata {
            annotation: self.annotation.revert_closurize(cache, env, with_env),
            ..self
        }
    }
}

pub mod hashmap {
    use std::collections::HashMap;

    pub struct SplitResult<K, V1, V2> {
        pub left: HashMap<K, V1>,
        pub center: HashMap<K, (V1, V2)>,
        pub right: HashMap<K, V2>,
    }

    /// Split two hashmaps m1 and m2 in three parts (left,center,right), where left holds bindings
    /// `(key,value)` where key is not in `m2.keys()`, right is the dual (keys of m2 that are not
    /// in m1), and center holds bindings for keys that are both in m1 and m2.
    pub fn split<K, V1, V2>(m1: HashMap<K, V1>, m2: HashMap<K, V2>) -> SplitResult<K, V1, V2>
    where
        K: std::hash::Hash + Eq,
    {
        let mut left = HashMap::new();
        let mut center = HashMap::new();
        let mut right = m2;

        for (key, value) in m1 {
            if let Some(v2) = right.remove(&key) {
                center.insert(key, (value, v2));
            } else {
                left.insert(key, value);
            }
        }

        SplitResult {
            left,
            center,
            right,
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn all_left() -> Result<(), String> {
            let mut m1 = HashMap::new();
            let m2 = HashMap::<isize, isize>::new();

            m1.insert(1, 1);
            let SplitResult {
                mut left,
                center,
                right,
            } = split(m1, m2);

            if left.remove(&1) == Some(1)
                && left.is_empty()
                && center.is_empty()
                && right.is_empty()
            {
                Ok(())
            } else {
                Err(String::from("Expected all elements to be in the left part"))
            }
        }

        #[test]
        fn all_right() -> Result<(), String> {
            let m1 = HashMap::<isize, isize>::new();
            let mut m2 = HashMap::new();

            m2.insert(1, 1);
            let SplitResult {
                left,
                center,
                mut right,
            } = split(m1, m2);

            if right.remove(&1) == Some(1)
                && right.is_empty()
                && left.is_empty()
                && center.is_empty()
            {
                Ok(())
            } else {
                Err(String::from(
                    "Expected all elements to be in the right part",
                ))
            }
        }

        #[test]
        fn all_center() -> Result<(), String> {
            let mut m1 = HashMap::new();
            let mut m2 = HashMap::new();

            m1.insert(1, 1);
            m2.insert(1, 2);
            let SplitResult {
                left,
                mut center,
                right,
            } = split(m1, m2);

            if center.remove(&1) == Some((1, 2))
                && center.is_empty()
                && left.is_empty()
                && right.is_empty()
            {
                Ok(())
            } else {
                Err(String::from(
                    "Expected all elements to be in the center part",
                ))
            }
        }

        #[test]
        fn mixed() -> Result<(), String> {
            let mut m1 = HashMap::new();
            let mut m2 = HashMap::new();

            m1.insert(1, 1);
            m1.insert(2, 1);
            m2.insert(1, -1);
            m2.insert(3, -1);
            let SplitResult {
                mut left,
                mut center,
                mut right,
            } = split(m1, m2);

            if left.remove(&2) == Some(1)
                && center.remove(&1) == Some((1, -1))
                && right.remove(&3) == Some(-1)
                && left.is_empty()
                && center.is_empty()
                && right.is_empty()
            {
                Ok(())
            } else {
                Err(String::from(
                    "Expected all elements to be in the center part",
                ))
            }
        }
    }
}
