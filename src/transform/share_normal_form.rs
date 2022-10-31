//! Share normal form.
//!
//! Replace the subexpressions of WHNFs that are not functions by thunks, such that they can be
//! shared. It is similar to the behavior of other lazy languages with respect to data
//! constructors.  To do so, subexpressions are replaced by fresh variables, introduced by new let
//! bindings put at the beginning of the WHNF.
//!
//! For example, take the expression:
//! ```text
//! let x = {a = 1 + 1} in x.a + x.a
//! ```
//!
//! The term `{a = 1 + 1}` is a record, and hence a WHNF. In consequence, the thunk allocated to x
//! is never updated. Without additional machinery, `a` will be recomputed each time is it used,
//! two times here.
//!
//! The transformation replaces such subexpressions, namely the content of the fields
//! of records and the elements of arrays - `(1 + 1)` in our example -, with fresh variables
//! introduced by `let`  added at the head of the term:
//!
//! ```text
//! let x = (let var = 1 + 1 in {a = var}) in x.a + x.a
//! ```
//!
//! Now, the field `a` points to the thunk introduced by `var`: at the evaluation of the first
//! occurrence of `x.a`, this thunk is updated with `2`, and is not recomputed the second time.
//!
//! Newly introduced variables begin with a special character to avoid clashing with user-defined
//! variables.
use crate::{
    identifier::Ident,
    match_sharedterm,
    position::TermPos,
    term::{record::RecordData, BindingType, LetAttrs, RichTerm, Term},
};

use std::{collections::HashSet, rc::Rc};

/// Transform the top-level term of an AST to a share normal form, if it can.
///
/// This function is not recursive: it just tries to apply one step of the transformation to
/// the top-level node of the AST. For example, it transforms `[1 + 1, [1 + 2]]` to `let %0 = 1
/// + 1 in [%0, [1 + 2]]`: the nested subterm `[1 + 2]` is left as it was. If the term is
/// neither a record, an array nor an enriched value, it is returned the same.  In other words,
/// the transformation is implemented as rewrite rules, and must be used in conjunction a
/// traversal to obtain a full transformation.
pub fn transform_one(rt: RichTerm) -> RichTerm {
    let pos = rt.pos;
    match_sharedterm! {rt.term,
        with {
            Term::Record(RecordData { fields, attrs }) => {
                let mut bindings = Vec::with_capacity(fields.len());

                let fields = fields
                    .into_iter()
                    .map(|(id, t)| {
                        if should_share(&t.term) {
                            let fresh_var = Ident::fresh();
                            let pos_t = t.pos;
                            bindings.push((fresh_var.clone(), t, BindingType::Normal));
                            (id, RichTerm::new(Term::Var(fresh_var), pos_t))
                        } else {
                            (id, t)
                        }
                    })
                    .collect();

                with_bindings(Term::Record(RecordData { fields, attrs }), bindings, pos)
            },
            Term::RecRecord(record, dyn_fields, deps) => {
                // When a recursive record is evaluated, all fields need to be turned to closures
                // anyway (see the corresponding case in `eval::eval()`), which is what the share
                // normal form transformation does. This is why the test is more lax here than for
                // other constructors: it is not only about sharing, but also about the future
                // evaluation of recursive records. Only constant are not required to be
                // closurized.
                //
                // In theory, the variable case is one exception: if the field is already a bare
                // variable, it seems useless to add one more indirection through a generated
                // variable. However, it is currently fundamental for recursive record merging that
                // the sare normal form transformation ensure the following post-condition: the
                // fields of recursive records contain either a constant or a *generated* variable,
                // but never a user-supplied variable directly (the former starts with a special
                // marker). See comments inside [`crate::RichTerm::closurize`] for more details.
                let mut bindings = Vec::with_capacity(record.fields.len());

                fn mk_binding_type(field_deps: Option<HashSet<Ident>>) -> BindingType {
                    // If the fields has an empty set of dependencies, we can eschew the
                    // useless introduction of a revertible thunk. Note that if
                    // `field_deps` being `None` doesn't mean "empty dependencies" but
                    // rather that the dependencies haven't been computed. In the latter
                    // case, we must be conservative and assume the field is potentially
                    // recursive.
                    let is_non_rec = (&field_deps).as_ref().map(|deps| deps.is_empty()).unwrap_or(false);
                    if is_non_rec {
                        BindingType::Normal
                    } else {
                        BindingType::Revertible(field_deps.map(Rc::new))
                    }
                }

                let static_fields = record.fields
                    .into_iter()
                    .map(|(id, t)| {
                        // CHANGE THIS CONDITION CAREFULLY. Doing so can break the post-condition
                        // explained above.
                        if !t.as_ref().is_constant() {
                            let fresh_var = Ident::fresh();
                            let pos_t = t.pos;
                            let field_deps = deps.as_ref().and_then(|deps| deps.stat_fields.get(&id)).cloned();
                            let is_non_rec = (&field_deps).as_ref().map(|deps| deps.is_empty()).unwrap_or(false);
                            let btype = mk_binding_type(field_deps);
                            bindings.push((fresh_var.clone(), t, btype));

                            (id, RichTerm::new(Term::Var(fresh_var), pos_t))
                        } else {
                            (id, t)
                        }
                    })
                    .collect();

                let dyn_fields = dyn_fields
                    .into_iter()
                    .enumerate()
                    .map(|(index, (id_t, t))| {
                        if !t.as_ref().is_constant() {
                            let fresh_var = Ident::fresh();
                            let pos_t = t.pos;
                            let field_deps = deps.as_ref().and_then(|deps| deps.dyn_fields.get(index)).cloned();
                            let btype = mk_binding_type(field_deps);
                            bindings.push((fresh_var.clone(), t, btype));
                            (id_t, RichTerm::new(Term::Var(fresh_var), pos_t))
                        } else {
                            (id_t, t)
                        }
                    })
                    .collect();

                with_bindings(Term::RecRecord(RecordData { fields: static_fields, attrs: record.attrs }, dyn_fields, deps), bindings, pos)
            },
            Term::Array(ts, attrs) => {
                let mut bindings = Vec::with_capacity(ts.len());

                let ts = ts
                    .into_iter()
                    .map(|t| {
                        if should_share(&t.term) {
                            let fresh_var = Ident::fresh();
                            let pos_t = t.pos;
                            bindings.push((fresh_var.clone(), t, BindingType::Normal));
                            RichTerm::new(Term::Var(fresh_var), pos_t)
                        } else {
                            t
                        }
                    })
                    .collect();

                with_bindings(Term::Array(ts, attrs), bindings, pos)
            },
            Term::MetaValue(meta) if meta.value.as_ref().map(|t| should_share(&t.term)).unwrap_or(false) => {
                    let mut meta = meta;
                    let fresh_var = Ident::fresh();
                    let t = meta.value.take().unwrap();
                    meta.value
                        .replace(RichTerm::new(Term::Var(fresh_var.clone()), t.pos));
                    let inner = RichTerm::new(Term::MetaValue(meta), pos);
                    let attrs = LetAttrs {
                        binding_type: BindingType::Normal,
                        rec : false,
                    };
                    RichTerm::new(Term::Let(fresh_var, t, inner, attrs), pos)
            }
        } else rt
    }
}

/// Determine if a subterm of a WHNF should be wrapped in a thunk in order to be shared.
///
/// Sharing is typically useless if the subterm is already a WHNF which can be copied without
/// duplicating any work. On the other hand, a WHNF which can contain other shareable
/// subexpressions, such as a record, should be shared.
pub fn should_share(t: &Term) -> bool {
    match t {
        Term::Null
        | Term::Bool(_)
        | Term::Num(_)
        | Term::Str(_)
        | Term::Lbl(_)
        | Term::SealingKey(_)
        | Term::Var(_)
        | Term::Enum(_)
        | Term::Fun(_, _) => false,
        _ => true,
    }
}

/// Bind a list of pairs `(identifier, term, binding_type)` in a term.
///
/// Given the term `body` and bindings of identifiers to terms represented as a list of pairs
/// `(id_1, term_1), .., (id_n, term_n)`, return the new term `let id_n = term_n in ... let
/// id_1 = term_1 in body`.
fn with_bindings(
    body: Term,
    bindings: Vec<(Ident, RichTerm, BindingType)>,
    pos: TermPos,
) -> RichTerm {
    bindings.into_iter().fold(
        RichTerm::new(body, pos.into_inherited()),
        |acc, (id, t, binding_type)| {
            RichTerm::new(
                Term::Let(
                    id,
                    t,
                    acc,
                    LetAttrs {
                        binding_type,
                        rec: false,
                    },
                ),
                pos,
            )
        },
    )
}
