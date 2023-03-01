use std::collections::HashMap;

use crate::{
    destructuring::{FieldPattern, Match, RecordPattern},
    identifier::Ident,
    mk_uty_row,
    types::{RecordRowF, RecordRowsF, TypeF},
};

use super::{
    mk_uniftype, Environment, GenericUnifRecordRowsIteratorItem, State, UnifRecordRows, UnifType,
};

pub fn build_pattern_type_walk_mode(state: &mut State, pat: &RecordPattern) -> UnifRecordRows {
    build_pattern_type(state, pat, TypecheckMode::Walk)
}

pub fn build_pattern_type_check_mode(state: &mut State, pat: &RecordPattern) -> UnifRecordRows {
    build_pattern_type(state, pat, TypecheckMode::Check)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TypecheckMode {
    Walk,
    Check,
}

/// Build a `UnifType` from a `Destruct` pattern. The type of each "leaf"
/// identifier will be assigned based on the `leaf_type` argument. The
/// current possibilities are for each leaf to have type `Dyn` or to be
/// assigned a fresh unification variable.
fn build_pattern_type(
    state: &mut State,
    pat: &RecordPattern,
    leaf_type: TypecheckMode,
) -> UnifRecordRows {
    fn new_leaf_type(state: &mut State, leaf_type: TypecheckMode) -> UnifType {
        match leaf_type {
            TypecheckMode::Walk => mk_uniftype::dynamic(),
            TypecheckMode::Check => state.table.fresh_type_uvar(),
        }
    }

    let tail = if pat.open {
        match leaf_type {
            // We use a dynamic tail here since we're in walk mode,
            // but if/when we remove dynamic record tails this could
            // likely be made an empty tail with no impact.
            TypecheckMode::Walk => mk_uty_row!(; RecordRowsF::TailDyn),
            TypecheckMode::Check => state.table.fresh_rrows_uvar(),
        }
    } else {
        UnifRecordRows::Concrete(RecordRowsF::Empty)
    };

    let rows = pat.matches.iter().map(|m| match m {
        Match::Simple(id, _) => RecordRowF {
            id: *id,
            types: Box::new(new_leaf_type(state, leaf_type)),
        },
        Match::Assign(id, _, FieldPattern::Ident(_)) => RecordRowF {
            id: *id,
            types: Box::new(new_leaf_type(state, leaf_type)),
        },
        Match::Assign(
            id,
            _,
            FieldPattern::RecordPattern(r_pat)
            | FieldPattern::AliasedRecordPattern { pattern: r_pat, .. },
        ) => {
            let row_tys = build_pattern_type(state, r_pat, leaf_type);
            let ty = UnifType::Concrete(TypeF::Record(row_tys));
            RecordRowF {
                id: *id,
                types: Box::new(ty),
            }
        }
    });

    rows.fold(tail, |tail, row| {
        UnifRecordRows::Concrete(RecordRowsF::Extend {
            row,
            tail: Box::new(tail),
        })
    })
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
    pat: &RecordPattern,
    pat_ty: UnifRecordRows,
) {
    let pat_ty = pat_ty.into_root(state.table);
    let mut type_map = RecordTypes::from(&pat_ty);

    pat.matches.iter().for_each(|m| match m {
        Match::Simple(id, ..) => {
            let ty = type_map.get_type(id);
            env.insert(*id, ty);
        }
        Match::Assign(id, _, FieldPattern::Ident(bind_id)) => {
            let ty = type_map.get_type(id);
            env.insert(*bind_id, ty);
        }
        Match::Assign(id, _, FieldPattern::RecordPattern(pat)) => {
            let ty = type_map.get_type(id);

            // Since we don't have a `bind_id` in this branch,
            // we can infer that `id` is an intermediate value that
            // isn't accessible from the code. e.g. the `foo` in a
            // binding like:
            //
            // ```
            //   let { foo = { bar = baz } } = { foo.bar = 1 } in ...
            // ```
            //
            // As such, we don't need to add it to the environment.

            let UnifType::Concrete(TypeF::Record(rs)) = ty else {
                unreachable!("since this is a destructured record, \
                              its type was constructed by build_pattern_ty, \
                              which means it must be a concrete record type")
            };
            inject_pattern_variables(state, env, pat, rs)
        }
        Match::Assign(id, _, FieldPattern::AliasedRecordPattern { alias, pattern }) => {
            let ty = type_map.get_type(id);

            env.insert(*alias, ty.clone());

            let UnifType::Concrete(TypeF::Record(rs)) = ty else {
                unreachable!("since this is a destructured record, \
                              its type was constructed by build_pattern_ty, \
                              which means it must be a concrete record type")
            };
            inject_pattern_variables(state, env, pattern, rs)
        }
    });

    if let Some(id) = pat.rest {
        let rest_ty = type_map.rest();
        env.insert(id, rest_ty);
    }
}

/// A map of identifiers in a destructured record to their types.
///
/// This allows us to be resilient to ordering differences between the
/// pattern and its type. As well as keeping track of which identifiers
/// have already been "used" in the pattern, to ensure that we can
/// correctly construct the type of a `..rest` match, if it exists.
struct RecordTypes {
    known_types: HashMap<Ident, UnifType>,
    tail: UnifRecordRows,
}

impl From<&UnifRecordRows> for RecordTypes {
    fn from(u: &UnifRecordRows) -> Self {
        let (known_types, tail) =
            u.iter()
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
        RecordTypes {
            known_types,
            tail: tail.unwrap_or(UnifRecordRows::Concrete(RecordRowsF::Empty)),
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
        self.known_types
            .remove(id)
            .expect("Scopes of identifiers in destruct patterns should be checked already")
    }

    /// Returns the "tail type" of the record. I.e., the record's tail
    /// plus any "unused" matches from `known_types`.
    fn rest(self) -> UnifType {
        let Self { known_types, tail } = self;
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
}
