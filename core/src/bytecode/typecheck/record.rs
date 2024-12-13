//! Typechecking records.
//!
//! Because record literal definitions are flexible in Nickel (piecewise definitions), they need
//! a bit of preprocessing before they can be typechecked. Preprocessing and typechecking of
//! records is handled in this module.
use super::{Ast, LocIdent};
use crate::{combine::Combine, bytecode::ast::record::{Record, FieldDef, FieldPathElem}};

use indexmap::{IndexMap, map::Entry};

use std::iter;

pub(super) trait Resolve<'ast> {
    type Resolved;

    fn resolve(&'ast self) -> Self::Resolved;
}

/// A resolved record literal, without field paths or piecewise definitions. Piecewise definitions
/// of fields have be grouped together, path have been broken into proper levels and top-level
/// fields are separated between static and dynamic.
#[derive(Default)]
pub(super) struct ResolvedRecord<'ast> {
    /// The static fields of the record. Most of the time the vector will contain only one element,
    /// the value of each field. However, when a field is defined piecewise and we can't compute
    /// the merged definitions statically (e.g. {foo.bar = x, foo.bar = y}` where `x` and `y` are
    /// variables - this will be elaborated to `{foo.bar = x & y}`), we need to keep all the
    /// definitions, hence the vector.
    pub stat_fields: IndexMap<LocIdent, ResolvedField<'ast>>,
    /// The dynamic fields of the record.
    pub dyn_fields: Vec<(&'ast Ast<'ast>, ResolvedField<'ast>)>,
}

impl ResolvedRecord<'_> {
    pub fn empty() -> Self {
        ResolvedRecord {
            stat_fields: IndexMap::new(),
            dyn_fields: Vec::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.stat_fields.is_empty() && self.dyn_fields.is_empty()
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
                    stat_fields.insert(id, Combine::combine(field1,field2));
                }

                let dyn_fields = this.dyn_fields.into_iter().chain(other.dyn_fields.into_iter()).collect();

                ResolvedRecord {
                    stat_fields,
                    dyn_fields,
                }
    }
}

impl<'ast> Combine for ResolvedField<'ast> {
    fn combine(this: Self, other: Self) -> Self {
        match (this, other) {
            (ResolvedField::Record(r1), ResolvedField::Record(r2)) => ResolvedField::Record(Combine::combine(r1, r2)),
            (ResolvedField::Value(values), ResolvedField::Value(v2)) => ResolvedField::Values { resolved: ResolvedRecord::empty(), values: vec![values, v2] },
            (ResolvedField::Values { mut values, resolved }, ResolvedField::Value(v)) | ( ResolvedField::Value(v), ResolvedField::Values { mut values, resolved }) => {
                values.push(v);
                ResolvedField::Values { values, resolved }
            }
            (ResolvedField::Values { resolved: r1, values: mut vs1 }, ResolvedField::Values { resolved: r2, values: vs2 }) => {
                vs1.extend(vs2);
                ResolvedField::Values { resolved: Combine::combine(r1, r2), values: vs1 }
            }
            (ResolvedField::Record(r), ResolvedField::Value(v)) | (ResolvedField::Value(v), ResolvedField::Record(r)) => {
                ResolvedField::Values { resolved: r, values: vec![v] }
            }
            (ResolvedField::Values { resolved: r1, values }, ResolvedField::Record(r2)) | (ResolvedField::Record(r1), ResolvedField::Values { resolved: r2, values }) => {
                ResolvedField::Values { resolved: Combine::combine(r1, r2), values }
            }
            (rfield, ResolvedField::Vacant) | (ResolvedField::Vacant, rfield) => rfield,
        }
    }
}

/// The field of a resolved record. Can either be another resolved record, or a final value (that
/// is an arbitrary [crate::bytecode::ast::Ast]).
pub(super) enum ResolvedField<'ast> {
    /// Default value. Meaningless but useful to take ownership of mutable references by swapping
    /// a mutable reference to a resolved field with this default value (`mem::swap`, hashmap entires, etc.)
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
        values: Vec<&'ast Ast<'ast>>
    },
}

// This implementation isn't really useful per se, but is below as a temporary value to swap data
// and take ownership of a field.
impl Default for ResolvedField<'_> {
    fn default() -> Self {
        ResolvedField::Vacant
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
            }
            else {
                // unreachable!(): `try_as_ident` returns `None` only if the path element is a
                // `Expr`
                let FieldPathElem::Expr(expr) = toplvl_field else { unreachable!() };
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
        self.path[1..].iter().rev().fold(ResolvedField::Value(self), |acc, path_elem| {
            if let Some(id) = path_elem.try_as_ident() {
                ResolvedField::Record(ResolvedRecord {
                    stat_fields: iter::once((id, acc)).collect(),
                    dyn_fields: Vec::new(),
                })
            }
            else {
                // unreachable!(): `try_as_ident` returns `None` only if the path element is a
                // `Expr`
                let FieldPathElem::Expr(expr) = path_elem else { unreachable!() };

                ResolvedField::Record(ResolvedRecord {
                    stat_fields: IndexMap::new(),
                    dyn_fields: vec![(expr, acc)],
                })
            }
        })
    }
}
