use crate::{
    destructuring::{FieldPattern, Match, RecordPattern},
    error::TypecheckError,
    identifier::LocIdent,
    mk_uty_row,
    term::{IndexMap, LabeledType},
    typ::{RecordRowF, RecordRowsF, TypeF},
    typecheck::{UnifRecordRow, Unify},
};

use super::{
    mk_uniftype, Context, Environment, GenericUnifRecordRowsIteratorItem, State, UnifRecordRows,
    UnifType, VarLevelsData,
};

pub fn build_pattern_type_walk_mode(
    state: &mut State,
    ctxt: &Context,
    pat: &RecordPattern,
) -> Result<UnifRecordRows, TypecheckError> {
    build_pattern_type(state, ctxt, pat, TypecheckMode::Walk)
}

pub fn build_pattern_type_check_mode(
    state: &mut State,
    ctxt: &Context,
    pat: &RecordPattern,
) -> Result<UnifRecordRows, TypecheckError> {
    build_pattern_type(state, ctxt, pat, TypecheckMode::Check)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TypecheckMode {
    Walk,
    Check,
}

/// Build a `UnifType` from a `Destruct` pattern. The type of each "leaf"
/// identifier will be assigned based on the `mode` argument. The
/// current possibilities are for each leaf to have type `Dyn`, to use an
/// explicit type annotation, or to be assigned a fresh unification variable.
fn build_pattern_type(
    state: &mut State,
    ctxt: &Context,
    pat: &RecordPattern,
    mode: TypecheckMode,
) -> Result<UnifRecordRows, TypecheckError> {
    fn new_leaf_type(
        state: &mut State,
        ctxt: &Context,
        mode: TypecheckMode,
        ty_annot: Option<LabeledType>,
    ) -> UnifType {
        match mode {
            TypecheckMode::Walk => mk_uniftype::dynamic(),
            TypecheckMode::Check => {
                if let Some(l_ty) = ty_annot {
                    UnifType::from_type(l_ty.typ, &ctxt.term_env)
                } else {
                    state.table.fresh_type_uvar(ctxt.var_level)
                }
            }
        }
    }

    let tail = if pat.open {
        match mode {
            // We use a dynamic tail here since we're in walk mode,
            // but if/when we remove dynamic record tails this could
            // likely be made an empty tail with no impact.
            TypecheckMode::Walk => mk_uty_row!(; RecordRowsF::TailDyn),
            TypecheckMode::Check => state.table.fresh_rrows_uvar(ctxt.var_level),
        }
    } else {
        UnifRecordRows::Concrete {
            rrows: RecordRowsF::Empty,
            var_levels_data: VarLevelsData::new_no_uvars(),
        }
    };

    let mut rows = pat.matches.iter().map(|m| match m {
        Match::Simple(id, field) => Ok(RecordRowF {
            id: *id,
            typ: Box::new(new_leaf_type(
                state,
                ctxt,
                mode,
                field.metadata.annotation.typ.clone(),
            )),
        }),
        Match::Assign(id, field, FieldPattern::Ident(_)) => Ok(RecordRowF {
            id: *id,
            typ: Box::new(new_leaf_type(
                state,
                ctxt,
                mode,
                field.metadata.annotation.typ.clone(),
            )),
        }),
        Match::Assign(
            id,
            field,
            FieldPattern::RecordPattern(r_pat)
            | FieldPattern::AliasedRecordPattern { pattern: r_pat, .. },
        ) => {
            let row_tys = build_pattern_type(state, ctxt, r_pat, mode)?;
            let ty = UnifType::concrete(TypeF::Record(row_tys));

            // If there are type annotations within nested record patterns
            // then we need to unify them with the pattern type we've built
            // to ensure (1) that they're mutually compatible and (2) that
            // we assign the annotated types to the right unification variables.
            if let Some(annot_ty) = &field.metadata.annotation.typ {
                let pos = annot_ty.typ.pos;
                let annot_uty = UnifType::from_type(annot_ty.typ.clone(), &ctxt.term_env);
                ty.clone()
                    .unify(annot_uty, state, ctxt)
                    .map_err(|e| e.into_typecheck_err(state, pos))?;
            }

            Ok(RecordRowF {
                id: *id,
                typ: Box::new(ty),
            })
        }
    });

    rows.try_fold(tail, |tail, row: Result<UnifRecordRow, TypecheckError>| {
        Ok(UnifRecordRows::concrete(RecordRowsF::Extend {
            row: row?,
            tail: Box::new(tail),
        }))
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
            env.insert(id.ident(), ty);
        }
        Match::Assign(id, _, FieldPattern::Ident(bind_id)) => {
            let ty = type_map.get_type(id);
            env.insert(bind_id.ident(), ty);
        }
        Match::Assign(id, _, FieldPattern::RecordPattern(pat)) => {
            let ty = type_map.get_type(id);

            // Since we don't have a `bind_id` in this branch,
            // we can infer that `id` is an intermediate value that
            // isn't accessible from the code. e.g. the `foo` in a
            // binding like:
            //
            // ```
            // let { foo = { bar = baz } } = { foo.bar = 1 } in ...
            // ```
            //
            // As such, we don't need to add it to the environment.
            let UnifType::Concrete {
                typ: TypeF::Record(rs),
                ..
            } = ty
            else {
                unreachable!(
                    "since this is a destructured record, \
                              its type was constructed by build_pattern_ty, \
                              which means it must be a concrete record type"
                )
            };
            inject_pattern_variables(state, env, pat, rs)
        }
        Match::Assign(id, _, FieldPattern::AliasedRecordPattern { alias, pattern }) => {
            let ty = type_map.get_type(id);

            env.insert(alias.ident(), ty.clone());

            let UnifType::Concrete {
                typ: TypeF::Record(rs),
                ..
            } = ty
            else {
                unreachable!(
                    "since this is a destructured record, \
                              its type was constructed by build_pattern_ty, \
                              which means it must be a concrete record type"
                )
            };
            inject_pattern_variables(state, env, pattern, rs)
        }
    });

    if let Some(id) = pat.rest {
        let rest_ty = type_map.rest();
        env.insert(id.ident(), rest_ty);
    }
}

/// A map of identifiers in a destructured record to their types.
///
/// This allows us to be resilient to ordering differences between the
/// pattern and its type. As well as keeping track of which identifiers
/// have already been "used" in the pattern, to ensure that we can
/// correctly construct the type of a `..rest` match, if it exists.
struct RecordTypes {
    known_types: IndexMap<LocIdent, UnifType>,
    tail: UnifRecordRows,
}

impl From<&UnifRecordRows> for RecordTypes {
    fn from(u: &UnifRecordRows) -> Self {
        let (known_types, tail) =
            u.iter()
                .fold((IndexMap::new(), None), |(mut m, _), ty| match ty {
                    GenericUnifRecordRowsIteratorItem::Row(rt) => {
                        m.insert(rt.id, rt.typ.clone());
                        (m, None)
                    }
                    GenericUnifRecordRowsIteratorItem::TailDyn => {
                        (m, Some(UnifRecordRows::concrete(RecordRowsF::TailDyn)))
                    }
                    GenericUnifRecordRowsIteratorItem::TailVar(v) => {
                        (m, Some(UnifRecordRows::concrete(RecordRowsF::TailVar(*v))))
                    }
                    GenericUnifRecordRowsIteratorItem::TailUnifVar { id, init_level } => {
                        (m, Some(UnifRecordRows::UnifVar { id, init_level }))
                    }
                    GenericUnifRecordRowsIteratorItem::TailConstant(n) => {
                        (m, Some(UnifRecordRows::Constant(n)))
                    }
                });
        RecordTypes {
            known_types,
            tail: tail.unwrap_or(UnifRecordRows::concrete(RecordRowsF::Empty)),
        }
    }
}

impl RecordTypes {
    /// Returns the type of the identifier `id` in the record.
    ///
    /// In the case of `RecordTypes::Rows`, `id` is also removed from the
    /// map, so that it won't be considered as part of the "tail type"
    /// when `rest` is called.
    fn get_type(&mut self, id: &LocIdent) -> UnifType {
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
            typ: Box::new(ty.clone()),
        });
        let rrows = rows.fold(tail, |tail, row| {
            UnifRecordRows::concrete(RecordRowsF::Extend {
                row,
                tail: Box::new(tail),
            })
        });
        UnifType::concrete(TypeF::Record(rrows))
    }
}
