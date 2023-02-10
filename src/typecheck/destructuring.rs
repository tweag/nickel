use std::collections::HashMap;

use crate::{
    destruct::{Destruct, Match},
    identifier::Ident,
    types::{RecordRowF, RecordRowsF, TypeF},
};

use super::{
    mk_uniftype, Environment, GenericUnifRecordRowsIteratorItem, GenericUnifType, State,
    UnifRecordRows, UnifType,
};

/// Build a `UnifType` from a `Destruct` pattern, creating fresh unification
/// variables wherever specific types are unknown.
pub fn build_pattern_type(state: &mut State, pat: &Destruct) -> UnifType {
    match pat {
        Destruct::Record { matches, open, .. } => {
            let tail = if *open {
                state.table.fresh_rrows_uvar()
            } else {
                UnifRecordRows::Concrete(RecordRowsF::Empty)
            };

            let rows = matches.iter().map(|m| match m {
                Match::Simple(id, _) => RecordRowF {
                    id: *id,
                    types: Box::new(state.table.fresh_type_uvar()),
                },
                Match::Assign(id, _, (_, Destruct::Empty)) => RecordRowF {
                    id: *id,
                    types: Box::new(state.table.fresh_type_uvar()),
                },
                Match::Assign(id, _, (_, r_pat @ Destruct::Record { .. })) => {
                    let ty = build_pattern_type(state, r_pat);
                    RecordRowF {
                        id: *id,
                        types: Box::new(ty),
                    }
                }
            });

            let rrows = rows.fold(tail, |tail, row| {
                UnifRecordRows::Concrete(RecordRowsF::Extend {
                    row,
                    tail: Box::new(tail),
                })
            });

            UnifType::Concrete(TypeF::Record(rrows))
        }
        Destruct::Empty => state.table.fresh_type_uvar(),
    }
}

/// Extend `env` with any new bindings brought into scope in `pat`. The
/// types of these bindings will be inferred from `pat_ty`.
///
/// For example, if `pat` represents the pattern `{ a, ..rest }` and
/// `pat_ty` is `{ a : Num, b : Str }` then the `env` will be extended
/// with `a : Num` and `rest : { b : Str }`.
pub fn inject_pattern_variables(
    state: &State,
    env: &mut Environment,
    pat: &Destruct,
    pat_ty: UnifType,
) {
    let pat_ty = pat_ty.into_root(state.table);

    match pat {
        Destruct::Record { matches, rest, .. } => {
            let mut type_map = RecordTypes::from(&pat_ty);

            matches.iter().for_each(|m| match m {
                Match::Simple(id, ..) => {
                    let ty = type_map.get_type(id);
                    env.insert(*id, ty);
                }
                Match::Assign(id, _, (bind_id, pat)) => {
                    let ty = type_map.get_type(id);

                    // If we don't have a `bind_id`, we can infer that `id` is
                    // an intermediate value that isn't accessible from the code.
                    // e.g. the `foo` in a binding like:
                    //
                    // ```
                    //   let { foo = { bar = baz } } = { foo.bar = 1 } in ...
                    // ```
                    //
                    // As such, we don't need to add it to the environment.
                    if let Some(id) = bind_id {
                        env.insert(*id, ty.clone());
                    }

                    // A non-empty `pat` here means we have a nested destructuring,
                    // so we recursively call this function.
                    if !pat.is_empty() {
                        inject_pattern_variables(state, env, pat, ty)
                    }
                }
            });

            if let Some(id) = rest {
                let rest_ty = type_map.rest();
                env.insert(*id, rest_ty);
            }
        }
        Destruct::Empty => (),
    }
}

/// A map of identifiers in a destructured record to their types.
///
/// This allows us to be resilient to ordering differences between the
/// pattern and its type. As well as keeping track of which identifiers
/// have already been "used" in the pattern, to ensure that we can
/// correctly construct the type of a `..rest` match, if it exists.
enum RecordTypes {
    Rows {
        known_types: HashMap<Ident, UnifType>,
        tail: UnifRecordRows,
    },
    Dyn,
}

impl From<&UnifType> for RecordTypes {
    fn from(u: &UnifType) -> Self {
        match u {
            GenericUnifType::Concrete(TypeF::Record(t)) => {
                let (tys, tail) =
                    t.iter()
                        .fold((HashMap::new(), None), |(mut m, _), ty| match ty {
                            GenericUnifRecordRowsIteratorItem::Row(rt) => {
                                m.insert(rt.id, rt.types.clone());
                                (m, None)
                            }
                            GenericUnifRecordRowsIteratorItem::TailDyn => {
                                (m, Some(UnifRecordRows::Concrete(RecordRowsF::TailDyn)))
                            }
                            GenericUnifRecordRowsIteratorItem::TailVar(v) => {
                                (m, Some(UnifRecordRows::Concrete(RecordRowsF::TailVar(*v))))
                            }
                            GenericUnifRecordRowsIteratorItem::TailUnifVar(n) => {
                                (m, Some(UnifRecordRows::UnifVar(n)))
                            }
                            GenericUnifRecordRowsIteratorItem::TailConstant(n) => {
                                (m, Some(UnifRecordRows::Constant(n)))
                            }
                        });
                RecordTypes::Rows {
                    known_types: tys,
                    tail: tail.unwrap_or(UnifRecordRows::Concrete(RecordRowsF::Empty)),
                }
            }
            _ => RecordTypes::Dyn,
        }
    }
}

impl RecordTypes {
    /// Returns the type of the identifier `id` in the record.
    ///
    /// In the case of `RecordTypes::Rows`, `id` is also removed from the
    /// map, so that it won't be considered as part of the "tail type"
    /// when `rest` is called.
    fn get_type(&mut self, id: &Ident) -> UnifType {
        match self {
            Self::Rows {
                known_types: h,
                tail: _,
            } => h
                .remove(id)
                .expect("Scopes of identifiers in destruct patterns should be checked already"),
            Self::Dyn => mk_uniftype::dynamic(),
        }
    }

    /// Returns the "tail type" of the record. I.e., the record's tail
    /// plus any "unused" matches from `known_types`.
    fn rest(self) -> UnifType {
        match self {
            Self::Rows { known_types, tail } => {
                let rows = known_types.iter().map(|(id, ty)| RecordRowF {
                    id: *id,
                    types: Box::new(ty.clone()),
                });
                let rrows = rows.fold(tail, |tail, row| {
                    UnifRecordRows::Concrete(RecordRowsF::Extend {
                        row,
                        tail: Box::new(tail),
                    })
                });
                UnifType::Concrete(TypeF::Record(rrows))
            }
            Self::Dyn => mk_uniftype::dyn_record(mk_uniftype::dynamic()),
        }
    }
}
