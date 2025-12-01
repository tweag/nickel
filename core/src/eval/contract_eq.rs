//! Computation of contract equality.
//!
//! This module is temporary, and has been extracted from the typechecker operating in the legacy
//! AST (`NickelValue`). It's used by the current tree-walking virtual machine, but will be scrapped
//! once the bytecode virtual machine (RFC007) is operational. At the time, we won't have the AST
//! around at runtime anymore, and will use an entirely different technique for contract
//! deduplication.
//!
//! Determine if two contracts are equal at runtime, which is leveraged by the contract
//! deduplication optimization.
//!
//! We first test for physical equality. If the comparison fails, we do a simple structural
//! recursion, unfolding simple forms and following variables with a limited number of times. For
//! anything more complex, we return false.
//!
//! The terms involved can be arbitrarily complex. Primops applications, `match`, and the like are
//! quite unlikely to appear inside an annotation (they surely appear inside contract definitions).
//!
//! We don't want to compare functions syntactically either. The spirit of this implementation is
//! to equate aliases or simple constructs that may appear inlined inside an annotation
//! (applications, records, primitive constants and arrays, mostly) in a structural way, and rely
//! on physical equality otherwise when two contracts "point" (through maybe variables) to the same
//! definition.
//!
//! ## Recursion
//!
//! We must refrain from following all variables links blindly, as there could be cycles in the
//! graph leading to an infinite loop:
//!
//! ```nickel
//! {
//!   Foo = Bar,
//!   Bar = Foo,
//! }
//! ```
//!
//! Because we just follows variables, and don't apply functions, we can detect cycles while
//! walking the graph. Still, as it is potentially performed many times during typechecking, type
//! equality ought to stay reasonably cheap. We choose to just set an arbitrary limit (the gas) on
//! the number of variable links that the type equality may follow. Doing so, we don't have to
//! worry about loops anymore.
use super::{Environment, cache::lazy::Thunk};

use crate::{
    eval::value::{Container, EnumVariantData, NickelValue, ValueContentRef},
    identifier::LocIdent,
    term::{IndexMap, Op1Data, StrChunk, Term, UnaryOp, record::Field},
    typ::{
        EnumRowF, EnumRows, EnumRowsIteratorItem, RecordRowF, RecordRows, RecordRowsIteratorItem,
        Type, TypeF,
    },
};

/// The maximal number of variable links we want to unfold before abandoning the check. It should
/// stay low, but has been fixed arbitrarily: feel fee to increase reasonably if it turns out
/// legitimate type equalities between simple contracts are unduly rejected in practice.
pub const MAX_GAS: u8 = 12;

/// State threaded through the type equality computation.
#[derive(Copy, Clone, Default)]
struct State {
    /// The current gas remaining for variable substitutions. Once it reaches zero and we encounter
    /// a variable, we abort the computation and return false.
    gas: u8,
}

impl State {
    fn new() -> Self {
        State { gas: MAX_GAS }
    }

    /// Try to consume one unit of gas for a variable substitution. Return true in case of success,
    /// or false if the gas was already at zero.
    fn use_gas(&mut self) -> bool {
        if self.gas == 0 {
            false
        } else {
            self.gas -= 1;
            true
        }
    }
}

/// Compute equality between two contracts.
///
/// # Parameters
///
/// - `env`: an environment mapping variables to their definition (the second placeholder in a
///   `let _ = _ in _`)
pub fn contract_eq(
    t1: &NickelValue,
    env1: &Environment,
    t2: &NickelValue,
    env2: &Environment,
) -> bool {
    contract_eq_bounded(&mut State::new(), t1, env1, t2, env2)
}

/// **Warning**: this function isn't computing a sound contract equality (it could equate contracts
/// that aren't actually the same). It is used to deduplicate type and contract annotations for
/// pretty-printing, where there is no notion of environment and the only thing that matters is
/// that they are printed the same or not.
///
/// Compute equality between two contracts in an empty environment. This means that two variables
/// with the same name are considered equal.
pub fn type_eq_noenv(t1: &Type, t2: &Type) -> bool {
    let empty = Environment::new();

    type_eq_bounded(&mut State::new(), t1, &empty, t2, &empty)
}

/// Decide type equality on contracts in their respective environment and given the remaining gas
/// in `state`.
fn contract_eq_bounded(
    state: &mut State,
    t1: &NickelValue,
    env1: &Environment,
    t2: &NickelValue,
    env2: &Environment,
) -> bool {
    // Test for physical equality as both an optimization and a way to cheaply equate complex
    // contracts that happen to point to the same definition (while the purposely limited
    // structural checks below may reject the equality)
    if t1.phys_eq(t2) && Environment::ptr_eq(env1, env2) {
        return true;
    }

    match (t1.content_ref(), t2.content_ref()) {
        (ValueContentRef::Null, ValueContentRef::Null) => true,
        (ValueContentRef::Bool(b1), ValueContentRef::Bool(b2)) => b1 == b2,
        (ValueContentRef::EnumVariant(enum1), ValueContentRef::EnumVariant(enum2)) => {
            match (enum1, enum2) {
                (
                    EnumVariantData {
                        tag: tag1,
                        arg: None,
                    },
                    EnumVariantData {
                        tag: tag2,
                        arg: None,
                    },
                ) => tag1 == tag2,
                (
                    EnumVariantData {
                        tag: tag1,
                        arg: Some(arg1),
                    },
                    EnumVariantData {
                        tag: tag2,
                        arg: Some(arg2),
                    },
                ) => tag1 == tag2 && contract_eq_bounded(state, arg1, env1, arg2, env2),
                _ => false,
            }
        }
        (ValueContentRef::SealingKey(s1), ValueContentRef::SealingKey(s2)) => s1 == s2,
        (ValueContentRef::Record(Container::Empty), ValueContentRef::Record(Container::Empty)) => {
            true
        }
        (
            ValueContentRef::Record(Container::Alloc(r1)),
            ValueContentRef::Record(Container::Alloc(r2)),
        ) => {
            map_eq(
                contract_eq_fields,
                state,
                &r1.fields,
                env1,
                &r2.fields,
                env2,
            ) && r1.attrs.open == r2.attrs.open
        }
        (ValueContentRef::Array(Container::Empty), ValueContentRef::Array(Container::Empty)) => {
            true
        }
        (
            ValueContentRef::Array(Container::Alloc(arr_data1)),
            ValueContentRef::Array(Container::Alloc(arr_data2)),
        ) => {
            arr_data1.array.len() == arr_data2.array.len()
                && arr_data1
                    .array
                    .iter()
                    .zip(arr_data2.array.iter())
                    .all(|(t1, t2)| contract_eq_bounded(state, t1, env1, t2, env2))
                // Ideally we would compare pending contracts, but it's a bit advanced and for now
                // we only equate arrays without additional contracts
                && arr_data1.pending_contracts.is_empty() && arr_data2.pending_contracts.is_empty()
        }
        // Contract is just a caching mechanism. `typ` should be the source of truth for equality
        // (and it's probably easier to prove that type are equal rather than their generated
        // contract version).
        (ValueContentRef::Type(type_data1), ValueContentRef::Type(type_data2)) => {
            type_eq_bounded(state, &type_data1.typ, env1, &type_data2.typ, env2)
        }
        (ValueContentRef::Term(term1), ValueContentRef::Term(term2)) => {
            match (&term1, &term2) {
                // We only compare string chunks when they represent a plain string (they don't contain any
                // interpolated expression), as static string may be currently parsed as such. We return
                // false for anything more complex.
                (Term::StrChunks(scs1), Term::StrChunks(scs2)) => {
                    scs1.len() == scs2.len()
                        && scs1.iter().zip(scs2.iter()).all(|(chunk1, chunk2)| {
                            match (chunk1, chunk2) {
                                (StrChunk::Literal(s1), StrChunk::Literal(s2)) => s1 == s2,
                                _ => false,
                            }
                        })
                }
                (Term::App(data1), Term::App(data2)) => {
                    contract_eq_bounded(state, &data1.head, env1, &data2.head, env2)
                        && contract_eq_bounded(state, &data1.arg, env1, &data2.arg, env2)
                }
                // All variables must be bound at this stage. This is checked by the typechecker when
                // walking annotations. However, we may assume that `env` is a local environment (that it
                // doesn't include the stdlib). In that case, free variables (unbound) may be deemed equal
                // if they have the same identifier: whatever global environment the term will be put in,
                // free variables are not redefined locally and will be bound to the same value in any case.
                (Term::Var(id1), Term::Var(id2)) => {
                    match (env1.get(&id1.ident()), env2.get(&id2.ident())) {
                        (Some(idx1), Some(idx2)) => {
                            // We consider unwrapping two thunks as only one operation using one
                            // unit of gas. This is arbitrary but unimportant.
                            if !state.use_gas() {
                                return false;
                            }

                            let closure1 = idx1.borrow();
                            let closure2 = idx2.borrow();

                            contract_eq_bounded(
                                state,
                                &closure1.value,
                                &closure1.env,
                                &closure2.value,
                                &closure2.env,
                            )
                        }
                        (None, None) => id1 == id2,
                        _ => false,
                    }
                }
                (Term::Var(id), _) => {
                    state.use_gas()
                        && env1
                            .get(&id.ident())
                            .map(|idx| {
                                let closure = idx.borrow();
                                contract_eq_bounded(state, &closure.value, &closure.env, t2, env2)
                            })
                            .unwrap_or(false)
                }
                (_, Term::Var(id)) => {
                    state.use_gas()
                        && env2
                            .get(&id.ident())
                            .map(|idx| {
                                let closure = idx.borrow();
                                contract_eq_bounded(state, t1, env1, &closure.value, &closure.env)
                            })
                            .unwrap_or(false)
                }
                (Term::RecRecord(data1), Term::RecRecord(data2)) =>
                // We only compare records whose field structure is statically known (i.e. without dynamic
                // fields) and without include expressions, which are a bit tricky to consider.
                {
                    data1.dyn_fields.is_empty()
                        && data2.dyn_fields.is_empty()
                        && data1.includes.is_empty()
                        && data2.includes.is_empty()
                        && map_eq(
                            contract_eq_fields,
                            state,
                            &data1.record.fields,
                            env1,
                            &data2.record.fields,
                            env2,
                        )
                        && data1.record.attrs.open == data2.record.attrs.open
                }
                // We must compare the inner values as well as the corresponding contracts or type
                // annotations.
                (Term::Annotated(data1), Term::Annotated(data2)) => {
                    let value_eq =
                        contract_eq_bounded(state, &data1.inner, env1, &data2.inner, env2);

                    // TODO:
                    // - does it really make sense to compare the annotations?
                    // - does it even happen to have contracts having themselves type annotations?
                    // - and in the latter case, should they be declared unequal because of that?
                    //   The answer to the last question is probably yes, because contracts are
                    //   fundamentally as powerful as function application, so they can change their
                    //   argument.

                    // We use the same logic as in the typechecker: the type associated to an annotated
                    // value is either the type annotation, or the first contract annotation.
                    let ty1 = data1.annot.first();
                    let ty2 = data2.annot.first();

                    let ty_eq = match (ty1, ty2) {
                        (None, None) => true,
                        (Some(ctr1), Some(ctr2)) => {
                            type_eq_bounded(state, &ctr1.typ, env1, &ctr2.typ, env2)
                        }
                        _ => false,
                    };

                    value_eq && ty_eq
                }
                (
                    Term::Op1(Op1Data {
                        op: UnaryOp::RecordAccess(id1),
                        arg: arg1,
                    }),
                    Term::Op1(Op1Data {
                        op: UnaryOp::RecordAccess(id2),
                        arg: arg2,
                    }),
                ) => id1 == id2 && contract_eq_bounded(state, arg1, env1, arg2, env2),
                (Term::Sealed(data1), Term::Sealed(data2)) => {
                    data1.key == data2.key
                        && contract_eq_bounded(state, &data1.inner, env1, &data2.inner, env2)
                }
                (Term::Closurize(v1), Term::Closurize(v2)) => {
                    contract_eq_bounded(state, v1, env1, v2, env2)
                }
                // We don't treat imports, parse errors, functions, let-bindings nor pairs of terms
                // that don't have the same shape
                _ => false,
            }
        }
        (ValueContentRef::Thunk(id1), ValueContentRef::Thunk(id2)) if Thunk::ptr_eq(id1, id2) => {
            true
        }
        (ValueContentRef::Thunk(thunk1), ValueContentRef::Thunk(thunk2)) => {
            // We consider unwrapping two thunks as only one operation using one unit of gas. This
            // is arbitrary but unimportant.
            if !state.use_gas() {
                return false;
            }

            // If we find a closure that isn't coming from a variable, it might be the content of a
            // recursive field. When deduplicating contracts at merge time, those fields have been
            // reverted to a state where they don't have a cached value built yet. This state is
            // not observable by the rest of the evaluator, but we do have to consider it here. We
            // make use of `borrow_orig` built for this purpose, to get the original closure in
            // this case instead of panicking as `borrow()` would.
            let closure1 = thunk1.borrow_orig();
            let closure2 = thunk2.borrow_orig();

            contract_eq_bounded(
                state,
                &closure1.value,
                &closure1.env,
                &closure2.value,
                &closure2.env,
            )
        }
        (ValueContentRef::Thunk(thunk), _) => {
            let closure = thunk.borrow_orig();

            state.use_gas() && contract_eq_bounded(state, &closure.value, &closure.env, t2, env2)
        }
        (_, ValueContentRef::Thunk(thunk)) => {
            let closure = thunk.borrow_orig();

            state.use_gas() && contract_eq_bounded(state, t1, env1, &closure.value, &closure.env)
        }
        (
            ValueContentRef::Term(Term::RecRecord(data)),
            ValueContentRef::Record(Container::Empty),
        )
        | (
            ValueContentRef::Record(Container::Empty),
            ValueContentRef::Term(Term::RecRecord(data)),
        ) => {
            data.dyn_fields.is_empty()
                && data.includes.is_empty()
                && data.record.fields.is_empty()
                // An open record is always properly allocated (not inlined), even if empty. So
                // `Container::Empty` implies closed.
                && !data.record.attrs.open
        }
        (
            ValueContentRef::Term(Term::RecRecord(rec_data)),
            ValueContentRef::Record(Container::Alloc(record)),
        )
        | (
            ValueContentRef::Record(Container::Alloc(record)),
            ValueContentRef::Term(Term::RecRecord(rec_data)),
        ) => {
            rec_data.dyn_fields.is_empty()
                && rec_data.includes.is_empty()
                && map_eq(
                    contract_eq_fields,
                    state,
                    &record.fields,
                    env1,
                    &rec_data.record.fields,
                    env2,
                )
                && rec_data.record.attrs.open == record.attrs.open
        }
        // Other cases are pairwise different
        _ => false,
    }
}

/// Compute the equality between two hashmaps holding either types or terms.
fn map_eq<V, F>(
    mut f: F,
    state: &mut State,
    map1: &IndexMap<LocIdent, V>,
    env1: &Environment,
    map2: &IndexMap<LocIdent, V>,
    env2: &Environment,
) -> bool
where
    F: FnMut(&mut State, &V, &Environment, &V, &Environment) -> bool,
{
    map1.len() == map2.len()
        && map1.iter().all(|(id, v1)| {
            map2.get(id)
                .map(|v2| f(state, v1, env1, v2, env2))
                .unwrap_or(false)
        })
}

/// Convert record rows to a hashmap.
///
/// Require the rows to be closed (i.e. the last element must be `RowEmpty`), otherwise `None` is
/// returned. `None` is returned as well if a type encountered is not row, or if it is a enum row.
fn rrows_as_map(erows: &RecordRows) -> Option<IndexMap<LocIdent, &Type>> {
    let map: Option<IndexMap<LocIdent, _>> = erows
        .iter()
        .map(|item| match item {
            RecordRowsIteratorItem::Row(RecordRowF { id, typ }) => Some((id, typ)),
            _ => None,
        })
        .collect();

    map
}

/// Convert enum rows to a hashmap.
///
/// Require the rows to be closed (i.e. the last element must be `RowEmpty`), otherwise `None` is
/// returned. `None` is returned as well if a type encountered is not row type, or if it is a
/// record row.
fn erows_as_map(erows: &EnumRows) -> Option<IndexMap<LocIdent, Option<&Type>>> {
    let set: Option<IndexMap<LocIdent, Option<_>>> = erows
        .iter()
        .map(|item| match item {
            EnumRowsIteratorItem::Row(EnumRowF { id, typ }) => Some((id, typ)),
            _ => None,
        })
        .collect();

    set
}

/// Check for contract equality between record fields. Fields are equal if they are both without a
/// definition, or are both defined and their values are equal.
///
/// The attached metadata must be equal as well: most record contracts are written as field with
/// metadata but without definition. For example, take `{ foo | {bar | Number}}` and `{foo | {bar |
/// String}}`. Those two record contracts are obviously not equal, but to know that, we have to
/// look at the contracts of each bar field.
fn contract_eq_fields(
    state: &mut State,
    field1: &Field,
    env1: &Environment,
    field2: &Field,
    env2: &Environment,
) -> bool {
    // Check that the pending contracts are equal.
    //
    // [^contract-eq-ignore-label]: We mostly ignore the label here, which doesn't impact the fact
    // that a contract blame or not. Different labels might lead to different error messages,
    // though. Note that there is one important exception: the field `type_environment` does impact
    // the evaluation of the contract. Fortunately, it's a simple datastructure that is easy to
    // compare, so we do check for equality here.
    //
    // Otherwise, comparing the rest of the labels seem rather clumsy (as labels store a wide
    // variety of static and runtime data) and not very meaningful.
    let pending_contracts_eq = field1
        .pending_contracts
        .iter()
        .zip(field2.pending_contracts.iter())
        .all(|(c1, c2)| {
            c1.label.type_environment == c2.label.type_environment
                && contract_eq_bounded(state, &c1.contract, env1, &c2.contract, env2)
        });

    // Check that the type and contract annotations are equal. [^contract-eq-ignore-label] applies
    // here as well.
    let annotations_eq = field1
        .metadata
        .iter_annots()
        .zip(field2.metadata.iter_annots())
        .all(|(t1, t2)| {
            t1.label.type_environment == t2.label.type_environment
                && type_eq_bounded(state, &t1.typ, env1, &t2.typ.clone(), env2)
        });

    // Check that "scalar" metadata (simple values) are equals
    let scalar_metadata_eq = field1.metadata.opt() == field2.metadata.opt()
        && field1.metadata.not_exported() == field2.metadata.not_exported()
        && field1.metadata.priority() == field2.metadata.priority();

    let value_eq = match (&field1.value, &field2.value) {
        (Some(value1), Some(value2)) => contract_eq_bounded(state, value1, env1, value2, env2),
        (None, None) => true,
        _ => false,
    };

    pending_contracts_eq && annotations_eq && scalar_metadata_eq && value_eq
}

/// Perform equality comparison on types. Structurally recurse into type constructors and test that
/// subtypes or subterms (contracts) are equals.
fn type_eq_bounded(
    state: &mut State,
    ty1: &Type,
    env1: &Environment,
    ty2: &Type,
    env2: &Environment,
) -> bool {
    match (&ty1.typ, &ty2.typ) {
        (TypeF::Wildcard(id1), TypeF::Wildcard(id2)) => id1 == id2,
        (TypeF::Dyn, TypeF::Dyn)
        | (TypeF::Number, TypeF::Number)
        | (TypeF::Bool, TypeF::Bool)
        | (TypeF::Symbol, TypeF::Symbol)
        | (TypeF::String, TypeF::String) => true,
        (
            TypeF::Dict {
                type_fields: uty1,
                flavour: attrs1,
            },
            TypeF::Dict {
                type_fields: uty2,
                flavour: attrs2,
            },
        ) if attrs1 == attrs2 => type_eq_bounded(state, uty1, env1, uty2, env2),
        (TypeF::Array(uty1), TypeF::Array(uty2)) => type_eq_bounded(state, uty1, env1, uty2, env2),
        (TypeF::Arrow(s1, t1), TypeF::Arrow(s2, t2)) => {
            type_eq_bounded(state, s1, env1, s2, env2) && type_eq_bounded(state, t1, env1, t2, env2)
        }
        (TypeF::Enum(uty1), TypeF::Enum(uty2)) => {
            fn type_eq_bounded_wrapper(
                state: &mut State,
                uty1: &Option<&Type>,
                env1: &Environment,
                uty2: &Option<&Type>,
                env2: &Environment,
            ) -> bool {
                match (uty1, uty2) {
                    (Some(uty1), Some(uty2)) => type_eq_bounded(state, uty1, env1, uty2, env2),
                    (None, None) => true,
                    _ => false,
                }
            }

            let map1 = erows_as_map(uty1);
            let map2 = erows_as_map(uty2);

            map1.zip(map2)
                .map(|(m1, m2)| map_eq(type_eq_bounded_wrapper, state, &m1, env1, &m2, env2))
                .unwrap_or(false)
        }
        (TypeF::Record(uty1), TypeF::Record(uty2)) => {
            fn type_eq_bounded_wrapper(
                state: &mut State,
                uty1: &&Type,
                env1: &Environment,
                uty2: &&Type,
                env2: &Environment,
            ) -> bool {
                type_eq_bounded(state, uty1, env1, uty2, env2)
            }

            let map1 = rrows_as_map(uty1);
            let map2 = rrows_as_map(uty2);

            map1.zip(map2)
                .map(|(m1, m2)| map_eq(type_eq_bounded_wrapper, state, &m1, env1, &m2, env2))
                .unwrap_or(false)
        }
        (TypeF::Contract(t1), TypeF::Contract(t2)) => {
            contract_eq_bounded(state, t1, env1, t2, env2)
        }
        // We don't handle foralls in this temporary module: they are unlikely to appear,
        // and they require type substitution machinery.
        _ => false,
    }
}
