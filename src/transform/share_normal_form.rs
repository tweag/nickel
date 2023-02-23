//! Share normal form.
//!
//! Replace the subexpressions of WHNFs that are not functions by indices, such that they can be
//! shared. It is similar to the behavior of other lazy languages with respect to data
//! constructors.  To do so, subexpressions are replaced by fresh variables, introduced by new let
//! bindings put at the beginning of the WHNF.
//!
//! For example, take the expression:
//! ```text
//! let x = {a = 1 + 1} in x.a + x.a
//! ```
//!
//! The term `{a = 1 + 1}` is a record, and hence a WHNF. In consequence, the index pointing to x
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
//! Now, the field `a` points to the element introduced by `var`: at the evaluation of the first
//! occurrence of `x.a`, this element is updated with `2`, and is not recomputed the second time.
//!
//! Newly introduced variables begin with a special character to avoid clashing with user-defined
//! variables.
use crate::{
    identifier::Ident,
    match_sharedterm,
    position::TermPos,
    term::{
        record::{Field, FieldDeps, RecordData},
        BindingType, LetAttrs, RichTerm, Term,
    },
};

struct Binding {
    fresh_var: Ident,
    term: RichTerm,
    binding_type: BindingType,
}

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
            // CAUTION: currently, if the record literals written by a user are always recursive
            // records, other phases of program transformation (such as desugaring of field paths
            // like `{foo.bar.baz = val}` or desugaring of destructuring) do introduce non
            // recursive records.
            //
            // I've scratched my head more that once on a transformation seemingly not happening
            // only for record elaborated by the destructuring desguaring phase, to find that the
            // `Term::Record` case below has been forgotten, and only the case `Term::RecRecord`
            // was updated.
            Term::Record(record_data) => {
                let mut bindings = Vec::with_capacity(record_data.fields.len());
                let empty_deps = FieldDeps::empty();

                let fields = record_data.fields.into_iter().map(|(id, field)| {
                    (id, transform_rec_field(field, Some(empty_deps.clone()), &mut bindings))
                }).collect();

                with_bindings(Term::Record(RecordData { fields, ..record_data }), bindings, pos)
            },
            Term::RecRecord(record_data, dyn_fields, deps) => {
                let mut bindings = Vec::with_capacity(record_data.fields.len());

                let fields = record_data.fields.into_iter().map(|(id, field)| {
                    let field_deps = deps.as_ref().and_then(|deps| deps.stat_fields.get(&id)).cloned();
                    (id, transform_rec_field(field, field_deps, &mut bindings))
                }).collect();

                let dyn_fields = dyn_fields
                    .into_iter()
                    .enumerate()
                    .map(|(index, (id_t, field))| {
                        let field_deps = deps.as_ref().and_then(|deps| deps.dyn_fields.get(index)).cloned();
                        (id_t, transform_rec_field(field, field_deps, &mut bindings))
                    })
                    .collect();

                with_bindings(Term::RecRecord(RecordData { fields, ..record_data}, dyn_fields, deps), bindings, pos)
            },
            Term::Array(ts, attrs) => {
                let mut bindings = Vec::with_capacity(ts.len());

                let ts = ts
                    .into_iter()
                    .map(|t| {
                        if should_share(&t.term) {
                            let fresh_var = Ident::fresh();
                            let pos_t = t.pos;
                            bindings.push(Binding {fresh_var, term: t, binding_type: BindingType::Normal});
                            RichTerm::new(Term::Var(fresh_var), pos_t)
                        } else {
                            t
                        }
                    })
                    .collect();

                with_bindings(Term::Array(ts, attrs), bindings, pos)
            },
            Term::Annotated(annot, t) if should_share(&t.term) => {
                let fresh_var = Ident::fresh();
                let shared = RichTerm::new(Term::Var(fresh_var), t.pos);
                let inner = RichTerm::new(Term::Annotated(annot, shared), pos);
                RichTerm::new(Term::Let(fresh_var, t, inner, LetAttrs::default()), pos)
            },
        } else rt
    }
}

/// Transform the field of a recursive record. Take care of transforming the pending contracts
/// attached to the field, as those might be recursively depending on other fields.
///
/// When a recursive record is evaluated, all fields need to be turned to closures anyway (see the
/// corresponding case in `eval::eval()`), which is what the share normal form transformation does.
/// This is why the test is more lax here than for other constructors: it is not only about
/// sharing, but also about the future evaluation of recursive records. Only constant are not
/// required to be closurized.
///
/// In theory, the variable case is one exception: if the field is already a bare variable, it
/// seems useless to add one more indirection through a generated variable. However, it is
/// currently fundamental for recursive record merging that the share normal form transformation
/// ensure the following post-condition: the fields of recursive records contain either a constant
/// or a *generated* variable, but never a user-supplied variable directly (the former starts with
/// a special marker). See comments inside [`crate::RichTerm::closurize`] for more details.
fn transform_rec_field(
    field: Field,
    field_deps: Option<FieldDeps>,
    bindings: &mut Vec<Binding>,
) -> Field {
    let mut share_normal_form = |rt: RichTerm| {
        // CHANGE THIS CONDITION CAREFULLY. Doing so can break the post-condition
        // explained in this method's documentation above.
        if !rt.as_ref().is_constant() {
            let fresh_var = Ident::fresh();
            let pos_contract = rt.pos;
            let binding_type = mk_binding_type(field_deps.clone());
            bindings.push(Binding {
                fresh_var,
                term: rt,
                binding_type,
            });
            RichTerm::new(Term::Var(fresh_var), pos_contract)
        } else {
            rt
        }
    };

    let pending_contracts = field
        .pending_contracts
        .into_iter()
        .map(|pending_contract| pending_contract.map_contract(&mut share_normal_form))
        .collect();

    let value = field.value.map(&mut share_normal_form);

    Field {
        value,
        metadata: field.metadata,
        pending_contracts,
    }
}

fn mk_binding_type(field_deps: Option<FieldDeps>) -> BindingType {
    // If the fields has an empty set of dependencies, we can eschew the
    // useless introduction of a revertible element. Note that
    // `field_deps` being `None` doesn't mean "empty dependencies" but
    // rather that the dependencies haven't been computed. In the latter
    // case, we must be conservative and assume the field is potentially
    // recursive.
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

/// Determine if a subterm of a WHNF should be cached in order to be shared.
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
        | Term::Fun(_, _)
        // match acts like a function, and is a WHNF
        | Term::Match {..} => false,
        _ => true,
    }
}

/// Bind a list of pairs `(identifier, term, binding_type)` in a term.
///
/// Given the term `body` and bindings of identifiers to terms represented as a list of pairs
/// `(id_1, term_1), .., (id_n, term_n)`, return the new term `let id_n = term_n in ... let
/// id_1 = term_1 in body`.
fn with_bindings(body: Term, bindings: Vec<Binding>, pos: TermPos) -> RichTerm {
    bindings.into_iter().fold(
        RichTerm::new(body, pos.into_inherited()),
        |acc,
         Binding {
             fresh_var,
             term,
             binding_type,
         }| {
            RichTerm::new(
                Term::Let(
                    fresh_var,
                    term,
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
