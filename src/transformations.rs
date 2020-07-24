//! Program transformations.

/// Share normal form.
///
/// Replace the subexpressions of WHNFs that are not functions by thunks, such that they can be
/// shared. It is similar to the behavior of other lazy languages with respect to data
/// constructors.  To do so, subexpressions are replaced by fresh variables, introduced by new let
/// bindings put at the beginning of the WHNF.
///
/// For example, take the expression
/// ```
/// let x = {a = (1 + 1);} in x.a + x.a
/// ```
///
/// The term `{a = 1 + 1;}` is a record, and hence a WHNF. In consequence, the thunk allocated to x
/// is never updated. Without additional machinery, `a` will be recomputed each time is it used,
/// two times here.
///
/// [`transform`](fn.transform.html) replaces such subexpressions, namely the content of the fields
/// of records and the elements of lists - `(1 + 1)` in our example -, with fresh variables
/// introduced by `let`  added at the head of the term:
///
/// ```
/// let x = (let var = 1 + 1 in {a = var;}) in x.a + x.a
/// ```
///
/// Now, the field `a` points to the thunk introduced by `var`: at the evaluation of the first
/// occurrence of `x.a`, this thunk is updated with `2`, and is not recomputed the second time.
///
/// Newly introduced variables begin with a special character to avoid clashing with user-defined
/// variables.
pub mod share_normal_form {
    use crate::identifier::Ident;
    use crate::term::{RichTerm, Term, UnaryOp};
    use simple_counter::*;
    use std::collections::HashMap;

    generate_counter!(FreshVariableCounter, usize);

    /// Transform a term to a share normal form.
    pub fn transform(rt: &RichTerm) -> RichTerm {
        let RichTerm { term, pos } = rt;
        let pos = pos.clone();
        match &**term {
            v @ &Term::Bool(_)
            | v @ &Term::Num(_)
            | v @ &Term::Str(_)
            | v @ &Term::Lbl(_)
            | v @ &Term::Sym(_)
            | v @ &Term::Var(_)
            | v @ &Term::Enum(_) => RichTerm {
                term: Box::new(v.clone()),
                pos,
            },
            &Term::Fun(ref id, ref t) => RichTerm {
                term: Box::new(Term::Fun(id.clone(), transform(t))),
                pos,
            },
            &Term::Let(ref id, ref t1, ref t2) => RichTerm {
                term: Box::new(Term::Let(id.clone(), transform(t1), transform(t2))),
                pos,
            },
            &Term::App(ref t1, ref t2) => RichTerm {
                term: Box::new(Term::App(transform(t1), transform(t2))),
                pos,
            },
            &Term::Op1(UnaryOp::Switch(ref cases, ref default), ref t) => {
                let cases = cases
                    .iter()
                    .map(|(id, t)| (id.clone(), transform(t)))
                    .collect();
                let default = default.as_ref().map(|t| transform(t));

                RichTerm {
                    term: Box::new(Term::Op1(UnaryOp::Switch(cases, default), transform(t))),
                    pos,
                }
            }
            &Term::Op1(ref op, ref t) => RichTerm {
                term: Box::new(Term::Op1(op.clone(), transform(t))),
                pos,
            },
            &Term::Op2(ref op, ref t1, ref t2) => RichTerm {
                term: Box::new(Term::Op2(op.clone(), transform(t1), transform(t2))),
                pos,
            },
            &Term::Promise(ref ty, ref l, ref t) => RichTerm {
                term: Box::new(Term::Promise(ty.clone(), l.clone(), transform(t))),
                pos,
            },
            &Term::Assume(ref ty, ref l, ref t) => RichTerm {
                term: Box::new(Term::Assume(ty.clone(), l.clone(), transform(t))),
                pos,
            },
            &Term::Wrapped(i, ref t) => RichTerm {
                term: Box::new(Term::Wrapped(i, transform(t))),
                pos,
            },
            &Term::Record(ref map) => {
                let mut bindings = Vec::with_capacity(map.len());
                let mut new_map = HashMap::with_capacity(map.len());

                for (id, ref t) in map.iter() {
                    if should_share(&*t.term) {
                        let fresh_var = fresh_var();
                        bindings.push((fresh_var.clone(), transform(t)));
                        new_map.insert(id.clone(), Term::Var(fresh_var).into());
                    } else {
                        new_map.insert(id.clone(), transform(t));
                    }
                }

                let result = bindings.into_iter().fold(
                    RichTerm {
                        term: Box::new(Term::Record(new_map)),
                        pos,
                    },
                    |acc, (id, t)| Term::Let(id, t, acc).into(),
                );

                result.into()
            }
            &Term::List(ref ts) => {
                let mut bindings = Vec::with_capacity(ts.len());
                let mut new_list = Vec::with_capacity(ts.len());

                for t in ts.iter() {
                    if should_share(&*t.term) {
                        let fresh_var = fresh_var();
                        bindings.push((fresh_var.clone(), transform(t)));
                        new_list.push(Term::Var(fresh_var).into());
                    } else {
                        new_list.push(transform(t));
                    }
                }

                let result = bindings.into_iter().fold(
                    RichTerm {
                        term: Box::new(Term::List(new_list)),
                        pos,
                    },
                    |acc, (id, t)| Term::Let(id, t, acc).into(),
                );

                result.into()
            }
            &Term::Contract(_, _) => rt.clone(),
            &Term::DefaultValue(ref t) => {
                if should_share(&*t.term) {
                    let fresh_var = fresh_var();
                    let inner = RichTerm {
                        term: Box::new(Term::DefaultValue(Term::Var(fresh_var.clone()).into())),
                        pos,
                    };
                    Term::Let(fresh_var, transform(t), inner).into()
                } else {
                    RichTerm {
                        term: Box::new(Term::DefaultValue(transform(t))),
                        pos,
                    }
                }
            }
            &Term::ContractWithDefault(ref ty, ref lbl, ref t) => {
                if should_share(&*t.term) {
                    let fresh_var = fresh_var();
                    let inner = RichTerm {
                        term: Box::new(Term::ContractWithDefault(
                            ty.clone(),
                            lbl.clone(),
                            Term::Var(fresh_var.clone()).into(),
                        )),
                        pos,
                    };
                    Term::Let(fresh_var, transform(t), inner).into()
                } else {
                    RichTerm {
                        term: Box::new(Term::ContractWithDefault(
                            ty.clone(),
                            lbl.clone(),
                            transform(t),
                        )),
                        pos,
                    }
                }
            }
            &Term::Docstring(ref s, ref t) => {
                if should_share(&*t.term) {
                    let fresh_var = fresh_var();
                    let inner = RichTerm {
                        term: Box::new(Term::Docstring(
                            s.clone(),
                            Term::Var(fresh_var.clone()).into(),
                        )),
                        pos,
                    };
                    Term::Let(fresh_var, transform(t), inner).into()
                } else {
                    RichTerm {
                        term: Box::new(Term::Docstring(s.clone(), transform(t))),
                        pos,
                    }
                }
            }
        }
    }

    /// Generate a new fresh variable which do not clash with user-defined variables.
    fn fresh_var() -> Ident {
        Ident(format!("%{}", FreshVariableCounter::next()))
    }

    /// Determine if a subterm of a WHNF should be wrapped in a thunk in order to be shared.
    ///
    /// Sharing is typically useless if the subterm is already a WHNF which can be copied without
    /// duplicating any work. On the other hand, a WHNF which can contain other shareable
    /// subexpressions, such as a record, should be shared.
    fn should_share(t: &Term) -> bool {
        match t {
            Term::Bool(_)
            | Term::Num(_)
            | Term::Str(_)
            | Term::Lbl(_)
            | Term::Sym(_)
            | Term::Var(_)
            | Term::Enum(_)
            | Term::Fun(_, _) => false,
            _ => true,
        }
    }
}
