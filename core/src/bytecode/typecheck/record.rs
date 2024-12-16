//! Typechecking records.
//!
//! Because record literal definitions are flexible in Nickel (piecewise definitions), they need
//! a bit of preprocessing before they can be typechecked. Preprocessing and typechecking of
//! records is handled in this module.
use super::*;
use crate::{
    bytecode::ast::record::{FieldDef, FieldPathElem, Record},
    combine::Combine,
};

use indexmap::{map::Entry, IndexMap};

use std::iter;

pub(super) trait Resolve<'ast> {
    type Resolved;

    fn resolve(&'ast self) -> Self::Resolved;
}

/// A resolved record literal, without field paths or piecewise definitions. Piecewise definitions
/// of fields have be grouped together, path have been broken into proper levels and top-level
/// fields are partitioned between static and dynamic.
#[derive(Default)]
pub(super) struct ResolvedRecord<'ast> {
    /// The static fields of the record.
    pub stat_fields: IndexMap<LocIdent, ResolvedField<'ast>>,
    /// The dynamic fields of the record.
    pub dyn_fields: Vec<(&'ast Ast<'ast>, ResolvedField<'ast>)>,
}

impl<'ast> ResolvedRecord<'ast> {
    pub fn empty() -> Self {
        ResolvedRecord {
            stat_fields: IndexMap::new(),
            dyn_fields: Vec::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.stat_fields.is_empty() && self.dyn_fields.is_empty()
    }

    pub fn check<V: TypecheckVisitor<'ast>>(
        &self,
        state: &mut State<'ast, '_>,
        ctxt: Context<'ast>,
        visitor: &mut V,
        ty: UnifType<'ast>,
    ) -> Result<(), TypecheckError> {
        // If we have no dynamic fields, we can check the record against a record type or a
        // dictionary type, depending on `ty`.
        if self.dyn_fields.is_empty() {
            self.check_stat(state, ctxt, visitor, ty)
        }
        // If some fields are defined dynamically, the only potential type that works is `{_ : a}`
        // for some `a`.
        else {
            self.check_dyn(state, ctxt, visitor, ty)
        }
    }

    fn check_dyn<V: TypecheckVisitor<'ast>>(
        &self,
        state: &mut State<'ast, '_>,
        mut ctxt: Context<'ast>,
        visitor: &mut V,
        ty: UnifType<'ast>,
    ) -> Result<(), TypecheckError> {
        let ty_dict = state.table.fresh_type_uvar(ctxt.var_level);

        ty.unify(mk_uniftype::dict(ty_dict.clone()), state, &ctxt)
            .map_err(|err| err.into_typecheck_err(state, todo!()))?;

        for id in self.stat_fields.keys() {
            ctxt.type_env.insert(id.ident(), ty_dict.clone());
            visitor.visit_ident(id, ty_dict.clone())
        }

        // We don't bind recursive fields in the term environment used to check for contract. See
        // [^term-env-rec-bindings] in `./mod.rs`.
        self.stat_fields
            .iter()
            .try_for_each(|(id, field)| -> Result<(), TypecheckError> {
                field.check(state, ctxt.clone(), visitor, *id, ty_dict.clone())
            })
    }

    fn check_stat<V: TypecheckVisitor<'ast>>(
        &self,
        state: &mut State<'ast, '_>,
        ctxt: Context<'ast>,
        visitor: &mut V,
        ty: UnifType<'ast>,
    ) -> Result<(), TypecheckError> {
        todo!()
    }
}

impl<'ast> Combine for ResolvedRecord<'ast> {
    fn combine(this: ResolvedRecord<'ast>, other: ResolvedRecord<'ast>) -> Self {
        use crate::eval::merge::split;

        let split::SplitResult {
            left,
            center,
            right,
        } = split::split(this.stat_fields, other.stat_fields);

        let mut stat_fields = IndexMap::with_capacity(left.len() + center.len() + right.len());

        stat_fields.extend(left);
        stat_fields.extend(right);

        for (id, (field1, field2)) in center.into_iter() {
            stat_fields.insert(id, Combine::combine(field1, field2));
        }

        let dyn_fields = this
            .dyn_fields
            .into_iter()
            .chain(other.dyn_fields.into_iter())
            .collect();

        ResolvedRecord {
            stat_fields,
            dyn_fields,
        }
    }
}

impl<'ast> Combine for ResolvedField<'ast> {
    fn combine(this: Self, other: Self) -> Self {
        match (this, other) {
            (ResolvedField::Record(r1), ResolvedField::Record(r2)) => {
                ResolvedField::Record(Combine::combine(r1, r2))
            }
            (ResolvedField::Value(v1), ResolvedField::Value(v2)) => ResolvedField::Values {
                resolved: ResolvedRecord::empty(),
                values: vec![v1, v2],
            },
            (
                ResolvedField::Values {
                    mut values,
                    resolved,
                },
                ResolvedField::Value(v),
            )
            | (
                ResolvedField::Value(v),
                ResolvedField::Values {
                    mut values,
                    resolved,
                },
            ) => {
                values.push(v);
                ResolvedField::Values { values, resolved }
            }
            (
                ResolvedField::Values {
                    resolved: r1,
                    values: mut vs1,
                },
                ResolvedField::Values {
                    resolved: r2,
                    values: vs2,
                },
            ) => {
                vs1.extend(vs2);
                ResolvedField::Values {
                    resolved: Combine::combine(r1, r2),
                    values: vs1,
                }
            }
            (ResolvedField::Record(r), ResolvedField::Value(v))
            | (ResolvedField::Value(v), ResolvedField::Record(r)) => ResolvedField::Values {
                resolved: r,
                values: vec![v],
            },
            (
                ResolvedField::Values {
                    resolved: r1,
                    values,
                },
                ResolvedField::Record(r2),
            )
            | (
                ResolvedField::Record(r1),
                ResolvedField::Values {
                    resolved: r2,
                    values,
                },
            ) => ResolvedField::Values {
                resolved: Combine::combine(r1, r2),
                values,
            },
            (rfield, ResolvedField::Vacant) | (ResolvedField::Vacant, rfield) => rfield,
        }
    }
}

/// The field of a resolved record.
///
/// A resolved field can be either:
///
/// - another resolved record, for the fields coming from elaboration, as
///   `mid` in `{ outer.mid.inner = true }`.
/// - A final value, for the last field of path, as `inner` in `{ outer.mid.inner = true }`.
/// - A combination of values, or of values and a resolved record, for a field defined piecewise
///   with multiple definitions, as `mid` in `fun param => { outer.mid.inner = true, outer.mid =
///   param}`. In this case, the resolved field `mid` will have a resolved part `{inner = true}`
///   and a value part `param`. Values can't be combined statically in all generality (imagine
///   adding another piecewise definition `outer.mid = other_variable` in the previous example), we
///   keep accumulating them. However, resolved parts can be merged statically, so we only need one
///   that we update as we collect the pieces of the definition.
#[derive(Default)]
pub(super) enum ResolvedField<'ast> {
    /// Default value. Meaningless but useful to take ownership of mutable references by swapping
    /// a mutable reference to a resolved field with this default value (`mem::swap`, hashmap entires, etc.)
    #[default]
    Vacant,
    /// A resolved record.
    Record(ResolvedRecord<'ast>),
    /// A final value (or no value at all). We only need to store an optional value and its
    /// metadata, but there is no such structure in the AST, so we store the whole field definition
    /// instead.
    Value(&'ast FieldDef<'ast>),
    /// Several values (coming from piecewise definitions).
    Values {
        resolved: ResolvedRecord<'ast>,
        values: Vec<&'ast FieldDef<'ast>>,
    },
}

impl<'ast> ResolvedField<'ast> {
    pub fn check<V: TypecheckVisitor<'ast>>(
        &self,
        state: &mut State<'ast, '_>,
        ctxt: Context<'ast>,
        visitor: &mut V,
        id: LocIdent,
        ty: UnifType<'ast>,
    ) -> Result<(), TypecheckError> {
        todo!()
    }
}

impl<'ast> Resolve<'ast> for Record<'ast> {
    type Resolved = ResolvedRecord<'ast>;

    fn resolve(&self) -> ResolvedRecord<'ast> {
        fn insert_static_field<'ast>(
            static_fields: &mut IndexMap<LocIdent, ResolvedField<'ast>>,
            id: LocIdent,
            field: ResolvedField<'ast>,
        ) {
            match static_fields.entry(id) {
                Entry::Occupied(mut occpd) => {
                    // temporarily putting an empty field in the entry to take the previous value.
                    let prev = occpd.insert(ResolvedField::Vacant);

                    // unwrap(): the field's identifier must have a position during parsing.
                    occpd.insert(Combine::combine(prev, field));
                }
                Entry::Vacant(vac) => {
                    vac.insert(field);
                }
            }
        }

        let mut stat_fields = IndexMap::new();
        let mut dyn_fields = Vec::new();

        for def in self.field_defs.iter() {
            // expect(): the field path must have at least one element, it's an invariant.
            let toplvl_field = def.path.first().expect("empty field path");
            let rfield = def.resolve();

            if let Some(id) = toplvl_field.try_as_ident() {
                insert_static_field(&mut stat_fields, id, rfield);
                continue;
            } else {
                // unreachable!(): `try_as_ident` returns `None` only if the path element is a
                // `Expr`
                let FieldPathElem::Expr(expr) = toplvl_field else {
                    unreachable!()
                };
                dyn_fields.push((expr, rfield));
            }
        }

        ResolvedRecord {
            stat_fields,
            dyn_fields,
        }
    }
}

// This turns a field definition into potentially nested resolved fields. Note that the top-level
// field is left out, as it's already been processed by the caller: resolving `foo.bar.baz.qux =
// 42` will return nested resolved records of the form `{bar = {baz = {qux = 42}}}`.
impl<'ast> Resolve<'ast> for FieldDef<'ast> {
    type Resolved = ResolvedField<'ast>;

    fn resolve(&'ast self) -> ResolvedField<'ast> {
        self.path[1..]
            .iter()
            .rev()
            .fold(ResolvedField::Value(self), |acc, path_elem| {
                if let Some(id) = path_elem.try_as_ident() {
                    ResolvedField::Record(ResolvedRecord {
                        stat_fields: iter::once((id, acc)).collect(),
                        dyn_fields: Vec::new(),
                    })
                } else {
                    // unreachable!(): `try_as_ident` returns `None` only if the path element is a
                    // `Expr`
                    let FieldPathElem::Expr(expr) = path_elem else {
                        unreachable!()
                    };

                    ResolvedField::Record(ResolvedRecord {
                        stat_fields: IndexMap::new(),
                        dyn_fields: vec![(expr, acc)],
                    })
                }
            })
    }
}
