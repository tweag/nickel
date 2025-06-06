//! Evaluation of the merge operator.
//!
//! Merge is a primitive operation of Nickel, which recursively combines records. Together with
//! field metadata, it allows to write and mix contracts with standard records.
//!
//! # Operational semantics
//!
//! ## On records
//!
//! When records `r1` and `r2` are merged, the result is a new record with the following fields:
//! - All the fields of `r1` that are not in `r2`
//! - All the fields of `r2` that are not in `r1`
//! - Fields that are both in `r1` and `r2` are recursively merged: for a field `f`, the result
//!   contains the binding `f = r1.f & r2.f`
//!
//! As fields are recursively merged, merge needs to operate on any value, not only on records:
//!
//! - *function*: merging a function with anything else fails
//! - *values*: merging any other values succeeds if and only if these two values are equals, in
//!   which case it evaluates to this common value.
//!
//! ## Metadata
//!
//! One can think of merge to be defined on metadata as well. When merging two fields, the
//! resulting metadata is the result of merging the two original field's metadata. The semantics
//! depend on each metadata.

use super::*;
use crate::{
    closurize::Closurize,
    combine::Combine,
    error::{EvalError, IllegalPolymorphicTailAction},
    label::{Label, MergeLabel},
    position::TermPos,
    term::{
        make as mk_term,
        record::{self, Field, FieldDeps, FieldMetadata, RecordAttrs, RecordData},
        BinaryOp, EnumVariantAttrs, IndexMap, RichTerm, Term, TypeAnnotation,
    },
};

/// Merging mode. Merging is used both to combine standard data and to apply contracts defined as
/// records.
///
/// In [MergeMode::Contract] mode, the merge operator acts like a custom contract. Instead of
/// returning the result directly, it either returns `'Ok result`, or `'Error {..}` if there were
/// some unexpected extra fields.
#[derive(Clone, PartialEq, Debug)]
pub enum MergeMode {
    /// Standard merging, for combining data.
    Standard(MergeLabel),
    /// Merging to apply a record contract to a value, with the associated label.
    Contract(Label),
}

impl From<MergeMode> for MergeLabel {
    /// Either takes the inner merge label if the mode is `Standard`, or converts a contract label
    /// to a merge label if the mode is `Contract`.
    fn from(mode: MergeMode) -> Self {
        match mode {
            MergeMode::Standard(merge_label) => merge_label,
            MergeMode::Contract(label) => label.into(),
        }
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

    // Determines if we need to wrap the result in `'Ok` upon successful merging, which is the case
    // when in contract merge mode. We're going to move out of `mode` at some point, so we need to
    // save this information now.
    let wrap_in_ok = matches!(mode, MergeMode::Contract(_));

    let result = match (t1.into_owned(), t2.into_owned()) {
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
                Err(EvalError::MergeIncompatibleArgs {
                    left_arg: RichTerm::new(Term::Bool(b1), pos1),
                    right_arg: RichTerm::new(Term::Bool(b2), pos2),
                    merge_label: mode.into(),
                })
            }
        }
        (Term::Num(n1), Term::Num(n2)) => {
            if n1 == n2 {
                Ok(Closure::atomic_closure(RichTerm::new(
                    Term::Num(n1),
                    pos_op.into_inherited(),
                )))
            } else {
                Err(EvalError::MergeIncompatibleArgs {
                    left_arg: RichTerm::new(Term::Num(n1), pos1),
                    right_arg: RichTerm::new(Term::Num(n2), pos2),
                    merge_label: mode.into(),
                })
            }
        }
        (Term::Str(s1), Term::Str(s2)) => {
            if s1 == s2 {
                Ok(Closure::atomic_closure(RichTerm::new(
                    Term::Str(s1),
                    pos_op.into_inherited(),
                )))
            } else {
                Err(EvalError::MergeIncompatibleArgs {
                    left_arg: RichTerm::new(Term::Str(s1), pos1),
                    right_arg: RichTerm::new(Term::Str(s2), pos2),
                    merge_label: mode.into(),
                })
            }
        }
        (Term::Lbl(l1), Term::Lbl(l2)) => {
            if l1 == l2 {
                Ok(Closure::atomic_closure(RichTerm::new(
                    Term::Lbl(l1),
                    pos_op.into_inherited(),
                )))
            } else {
                Err(EvalError::MergeIncompatibleArgs {
                    left_arg: RichTerm::new(Term::Lbl(l1), pos1),
                    right_arg: RichTerm::new(Term::Lbl(l2), pos2),
                    merge_label: mode.into(),
                })
            }
        }
        (Term::Enum(i1), Term::Enum(i2)) => {
            if i1 == i2 {
                Ok(Closure::atomic_closure(RichTerm::new(
                    Term::Enum(i1),
                    pos_op.into_inherited(),
                )))
            } else {
                Err(EvalError::MergeIncompatibleArgs {
                    left_arg: RichTerm::new(Term::Enum(i1), pos1),
                    right_arg: RichTerm::new(Term::Enum(i2), pos2),
                    merge_label: mode.into(),
                })
            }
        }
        (
            Term::EnumVariant {
                tag: tag1,
                arg: arg1,
                attrs: _,
            },
            Term::EnumVariant {
                tag: tag2,
                arg: arg2,
                attrs: _,
            },
        ) if tag1 == tag2 => {
            let arg = RichTerm::from(Term::Op2(
                BinaryOp::Merge(mode.into()),
                arg1.closurize(cache, env1),
                arg2.closurize(cache, env2),
            ));

            Ok(Closure::atomic_closure(RichTerm::new(
                Term::EnumVariant {
                    tag: tag1,
                    arg,
                    attrs: EnumVariantAttrs { closurized: true },
                },
                pos_op.into_inherited(),
            )))
        }
        // There are several different (and valid) ways of merging arrays. We don't want to choose
        // for the user, so future custom merge functions will provide a way to overload the native
        // merging function. For the time being, we still need to be idempotent: thus we rewrite
        // `array1 & array2` to `contract.Equal array1 array2`, so that we extend merge in the
        // minimum way such that it is idempotent.
        (t1 @ Term::Array(..), t2 @ Term::Array(..)) => {
            use crate::{mk_app, stdlib, typ::TypeF};
            use std::rc::Rc;

            let t1 = RichTerm::new(t1, pos1).closurize(cache, env1);
            let t2 = RichTerm::new(t2, pos2).closurize(cache, env2);

            // We reconstruct the contract we apply later on just to fill the label. This will be
            // printed out when reporting the error.
            let contract_for_display = mk_app!(
                mk_term::op1(
                    UnaryOp::RecordAccess("Equal".into()),
                    Term::Var("contract".into()),
                ),
                // We would need to substitute variables inside `t1` to make it useful to print,
                // but currently we don't want to do it preventively at each array merging, so we
                // just print `contract.Equal some_array`.
                //
                // If the error reporting proves to be insufficient, consider substituting the
                // variables inside `t1`, but be aware that it might (or might not) have a
                // noticeable impact on performance.
                mk_term::var("some_array")
            );

            let label = Label {
                typ: Rc::new(TypeF::Contract(contract_for_display).into()),
                span: MergeLabel::from(mode).span,
                ..Default::default()
            }
            .with_diagnostic_message("cannot merge unequal arrays")
            .append_diagnostic_note(
                "\
                This equality contract was auto-generated from a merge operation on two arrays. \
                Arrays can only be merged if they are equal.",
            );

            // We don't actually use `contract.Equal` directly, because contract could have been
            // locally redefined. We rather use the internal `$stdlib_contract_equal`, which is
            // exactly the same, but can't be shadowed.
            let eq_contract = mk_app!(stdlib::internals::stdlib_contract_equal(), t1);
            let result = mk_app!(
                mk_term::op2(BinaryOp::ContractApply, eq_contract, Term::Lbl(label)),
                t2
            )
            .with_pos(pos_op);

            Ok(Closure {
                body: result,
                env: Environment::new(),
            })
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

            let split::SplitResult {
                left,
                center,
                right,
            } = split::split(r1.fields, r2.fields);

            match mode {
                MergeMode::Contract(_) if !r2.attrs.open && !left.is_empty() => {
                    let fields: Vec<String> =
                        left.keys().map(|field| format!("`{field}`")).collect();
                    let plural = if fields.len() == 1 { "" } else { "s" };
                    let fields_list = fields.join(", ");

                    // The presence of extra fields is an immediate contract error. Thus, instead
                    // of raising a blame error as for a delayed contract error, which can't be
                    // caught in user-code, we return an `'Error {..}` value instead.
                    return Ok(Closure::atomic_closure(
                        mk_term::enum_variant("Error", Term::Record(RecordData::with_field_values([
                            ("message".into(), mk_term::string(format!("extra field{plural} {fields_list}"))),
                            ("notes".into(), Term::Array([
                                mk_term::string("Have you misspelled a field?"),
                                mk_term::string(
                                    "The record contract might also be too strict. By default, \
                                    record contracts exclude any field which is not listed.\n\
                                    Append `, ..` at the end of the record contract, as in \
                                    `{some_field | SomeContract, ..}`, to make it accept extra fields."
                                ),
                            ].into_iter().collect(), Default::default()).into())
                        ])))));
                }
                _ => (),
            };

            let final_pos = if let MergeMode::Standard(_) = mode {
                pos_op.into_inherited()
            } else {
                pos1.into_inherited()
            };

            let merge_label = MergeLabel::from(mode);

            let field_names: Vec<_> = left
                .keys()
                .chain(center.keys())
                .chain(right.keys())
                .copied()
                .collect();
            let mut m = IndexMap::with_capacity(left.len() + center.len() + right.len());

            // Merging recursive records is the one operation that may override recursive fields. To
            // have the recursive fields depend on the updated values, we need to revert the
            // corresponding elements in the cache to their original expression.
            //
            // We do that for the left and the right part.
            //
            // The fields in the intersection (center) need a slightly more general treatment to
            // correctly propagate the recursive values down each field: saturation. See
            // [crate::eval::cache::Cache::saturate()].
            m.extend(
                left.into_iter()
                    .map(|(id, field)| (id, field.revert_closurize(cache))),
            );

            m.extend(
                right
                    .into_iter()
                    .map(|(id, field)| (id, field.revert_closurize(cache))),
            );

            for (id, (field1, field2)) in center.into_iter() {
                m.insert(
                    id,
                    merge_fields(cache, merge_label, field1, field2, field_names.iter())?,
                );
            }

            let attrs = RecordAttrs::combine(r1.attrs, r2.attrs);
            // Both records passed to `merge` should be closurized, and their result after merging
            // must be as well
            debug_assert!(attrs.closurized);

            Ok(Closure {
                body: RichTerm::new(
                    // We don't have to provide RecordDeps, which are required in a previous stage
                    // of program transformations. At this point, the interpreter doesn't care
                    // about them anymore, and dependencies are stored at the level of revertible
                    // cache elements directly.
                    //
                    // Include expressions are transformed to normal fields the very first time
                    // they are seen, and there's no way back. We always set them to empty here.
                    Term::RecRecord(
                        RecordData::new(m, attrs, None),
                        Vec::new(),
                        Vec::new(),
                        None,
                    ),
                    final_pos,
                ),
                env: Environment::new(),
            })
        }
        (t1_, t2_) => Err(EvalError::MergeIncompatibleArgs {
            left_arg: RichTerm::new(t1_, pos1),
            right_arg: RichTerm::new(t2_, pos2),
            merge_label: mode.into(),
        }),
    };

    if wrap_in_ok {
        result.map(|closure| {
            let pos = closure.body.pos;

            Closure {
                body: mk_term::enum_variant("Ok", closure.body).with_pos(pos),
                ..closure
            }
        })
    } else {
        result
    }
}

/// Take two record fields in their respective environment and combine both their metadata and
/// values. Apply the required saturate, revert or closurize operation, including on the final
/// field returned.
#[allow(clippy::too_many_arguments)]
fn merge_fields<'a, C: Cache, I: DoubleEndedIterator<Item = &'a LocIdent> + Clone>(
    cache: &mut C,
    merge_label: MergeLabel,
    field1: Field,
    field2: Field,
    fields: I,
) -> Result<Field, EvalError> {
    let Field {
        metadata: metadata1,
        value: value1,
        pending_contracts: pending_contracts1,
    } = field1;
    let Field {
        metadata: metadata2,
        value: value2,
        pending_contracts: pending_contracts2,
    } = field2;

    // Selecting either meta1's value, meta2's value, or the merge of the two values,
    // depending on which is defined and respective priorities.
    let (value, priority) = match (value1, value2) {
        (Some(t1), Some(t2)) if metadata1.priority == metadata2.priority => (
            Some(fields_merge_closurize(cache, merge_label, t1, t2, fields).unwrap()),
            metadata1.priority,
        ),
        (Some(t1), _) if metadata1.priority > metadata2.priority => {
            (Some(t1.revert_closurize(cache)), metadata1.priority)
        }
        (Some(t1), None) => (Some(t1.revert_closurize(cache)), metadata1.priority),
        (_, Some(t2)) if metadata2.priority > metadata1.priority => {
            (Some(t2.revert_closurize(cache)), metadata2.priority)
        }
        (None, Some(t2)) => (Some(t2.revert_closurize(cache)), metadata2.priority),
        (None, None) => (None, Default::default()),
        _ => unreachable!(),
    };

    // Since contracts are closurized, they don't need another local environment
    let empty = Environment::new();

    let pending_contracts = RuntimeContract::combine_dedup(
        pending_contracts1.revert_closurize(cache),
        &empty,
        pending_contracts2.revert_closurize(cache),
        &empty,
    );

    Ok(Field {
        metadata: FieldMetadata {
            doc: merge_doc(metadata1.doc, metadata2.doc),
            annotation: TypeAnnotation::combine_dedup(metadata1.annotation, metadata2.annotation),
            // If one of the record requires this field, then it musn't be optional. The
            // resulting field is optional iff both are.
            opt: metadata1.opt && metadata2.opt,
            not_exported: metadata1.not_exported || metadata2.not_exported,
            priority,
        },
        value,
        pending_contracts,
    })
}

/// Merge two optional documentations.
///
/// This function is parametrized temporarily to accomodate both the mainline Nickel AST
/// ([crate::term::Term]) where documentation is represented as a `String`, and the new bytecode
/// AST where documentation is represented as an `Rc<str>`.
//FIXME: remove the type parameter `D` once we've moved evaluation to the new bytecode VM.
//Currently we need to handle both the old representation `D=String` and the new one `D=Rc<str>`.
pub(crate) fn merge_doc<D>(doc1: Option<D>, doc2: Option<D>) -> Option<D> {
    //FIXME: how to merge documentation? Just concatenate?
    doc1.or(doc2)
}

/// See [crate::eval::cache::Cache::saturate]. Saturation is a transformation on recursive cache
/// elements that is used when we must combine different values with different recursive
/// dependencies (say, the two values of fields being merged) into one expression.
///
/// Saturation is first and foremost a transformation of terms, but like
/// [crate::transform::Closurizable], it can be applied to other types that contain terms, hence
/// the trait.
trait Saturate: Sized {
    /// Take the content of a record field, and saturate the potential revertible element with the
    /// given fields. See [crate::eval::cache::Cache::saturate].
    ///
    /// If the expression is not a variable referring to an element in the cache (this can happen
    ///  e.g. for numeric constants), we just return the term as it is, which falls into the zero
    /// dependencies special case.
    fn saturate<'a, I: DoubleEndedIterator<Item = &'a LocIdent> + Clone, C: Cache>(
        self,
        cache: &mut C,
        fields: I,
    ) -> Result<Self, EvalError>;
}

impl Saturate for RichTerm {
    fn saturate<'a, I: DoubleEndedIterator<Item = &'a LocIdent> + Clone, C: Cache>(
        self,
        cache: &mut C,
        fields: I,
    ) -> Result<RichTerm, EvalError> {
        if let Term::Closure(idx) = &*self.term {
            Ok(cache
                .saturate(idx.clone(), fields.map(LocIdent::ident))
                .with_pos(self.pos))
        } else {
            debug_assert!(self.as_ref().is_constant());
            Ok(self)
        }
    }
}

/// Return the dependencies of a field when represented as a `RichTerm`.
fn field_deps<C: Cache>(cache: &C, rt: &RichTerm) -> Result<FieldDeps, EvalError> {
    if let Term::Closure(idx) = &*rt.term {
        Ok(cache.deps(idx).unwrap_or_else(FieldDeps::empty))
    } else {
        Ok(FieldDeps::empty())
    }
}

/// Take the current environment, two fields with their local environment, and return a term which
/// is the merge of the two fields, closurized in the provided final environment.
///
/// The element in the cache allocated for the result is revertible if and only if at least one of
/// the original elements is (if one of the original values is overridable, then so is the merge of
/// the two). In this case, the field dependencies are the union of the dependencies of each field.
///
/// The fields are saturated (see [saturate]) to properly propagate recursive dependencies down to
/// `t1` and `t2` in the final, merged record.
#[allow(clippy::too_many_arguments)]
fn fields_merge_closurize<'a, I: DoubleEndedIterator<Item = &'a LocIdent> + Clone, C: Cache>(
    cache: &mut C,
    merge_label: MergeLabel,
    t1: RichTerm,
    t2: RichTerm,
    fields: I,
) -> Result<RichTerm, EvalError> {
    let combined_deps = field_deps(cache, &t1)?.union(field_deps(cache, &t2)?);
    let body = RichTerm::from(Term::Op2(
        BinaryOp::Merge(merge_label),
        t1.saturate(cache, fields.clone())?,
        t2.saturate(cache, fields)?,
    ));

    // We closurize the final result with appropriate dependencies
    let closure = Closure {
        body,
        env: Environment::new(),
    };

    let idx = cache.add(closure, BindingType::Revertible(combined_deps));

    Ok(RichTerm::from(Term::Closure(idx)))
}

/// Same as [Closurizable], but also revert the element if the term is a closure.
pub(super) trait RevertClosurize {
    /// Revert the element at the index inside the term (if any)
    fn revert_closurize<C: Cache>(self, cache: &mut C) -> Self;
}

impl RevertClosurize for RichTerm {
    fn revert_closurize<C: Cache>(self, cache: &mut C) -> RichTerm {
        if let Term::Closure(idx) = self.as_ref() {
            RichTerm::new(Term::Closure(cache.revert(idx)), self.pos)
        } else {
            // Otherwise, if it is not a closure after the share normal form transformations, it
            // should be a constant and we don't need to revert anything
            debug_assert!(self.as_ref().is_constant());
            self
        }
    }
}

impl RevertClosurize for Field {
    fn revert_closurize<C: Cache>(self, cache: &mut C) -> Field {
        let value = self.value.map(|value| value.revert_closurize(cache));

        let pending_contracts = self.pending_contracts.revert_closurize(cache);

        Field {
            metadata: self.metadata,
            value,
            pending_contracts,
        }
    }
}

impl RevertClosurize for RuntimeContract {
    fn revert_closurize<C: Cache>(self, cache: &mut C) -> RuntimeContract {
        self.map_contract(|ctr| ctr.revert_closurize(cache))
    }
}

impl RevertClosurize for Vec<RuntimeContract> {
    fn revert_closurize<C: Cache>(self, cache: &mut C) -> Vec<RuntimeContract> {
        self.into_iter()
            .map(|pending_contract| pending_contract.revert_closurize(cache))
            .collect()
    }
}

pub mod split {
    use crate::term::IndexMap;

    pub struct SplitResult<K, V1, V2> {
        pub left: IndexMap<K, V1>,
        pub center: IndexMap<K, (V1, V2)>,
        pub right: IndexMap<K, V2>,
    }

    /// Split two maps m1 and m2 in three parts (left,center,right), where left holds bindings
    /// `(key,value)` where key is not in `m2.keys()`, right is the dual (keys of m2 that are not
    /// in m1), and center holds bindings for keys that are both in m1 and m2.
    pub fn split<K, V1, V2>(m1: IndexMap<K, V1>, m2: IndexMap<K, V2>) -> SplitResult<K, V1, V2>
    where
        K: std::hash::Hash + Eq,
    {
        let mut left = IndexMap::new();
        let mut center = IndexMap::new();
        let mut right = m2;

        for (key, value) in m1 {
            // We don't perserve the ordering of the right part. However, note that currently, what
            // matters is that the iteration order on hashmap is _deterministic_. It's a bonus that
            // it corresponds to insertion order for record literal, but we don't make any
            // guarantee on the result of merging or other operations. Exporting will sort the
            // result anyway.
            if let Some(v2) = right.swap_remove(&key) {
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
            let mut m1 = IndexMap::new();
            let m2 = IndexMap::<isize, isize>::new();

            m1.insert(1, 1);
            let SplitResult {
                mut left,
                center,
                right,
            } = split(m1, m2);

            if left.swap_remove(&1) == Some(1)
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
            let m1 = IndexMap::<isize, isize>::new();
            let mut m2 = IndexMap::new();

            m2.insert(1, 1);
            let SplitResult {
                left,
                center,
                mut right,
            } = split(m1, m2);

            if right.swap_remove(&1) == Some(1)
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
            let mut m1 = IndexMap::new();
            let mut m2 = IndexMap::new();

            m1.insert(1, 1);
            m2.insert(1, 2);
            let SplitResult {
                left,
                mut center,
                right,
            } = split(m1, m2);

            if center.swap_remove(&1) == Some((1, 2))
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
            let mut m1 = IndexMap::new();
            let mut m2 = IndexMap::new();

            m1.insert(1, 1);
            m1.insert(2, 1);
            m2.insert(1, -1);
            m2.insert(3, -1);
            let SplitResult {
                mut left,
                mut center,
                mut right,
            } = split(m1, m2);

            if left.swap_remove(&2) == Some(1)
                && center.swap_remove(&1) == Some((1, -1))
                && right.swap_remove(&3) == Some(-1)
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
