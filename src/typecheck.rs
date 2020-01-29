use crate::identifier::Ident;
use crate::term::{BinaryOp, RichTerm, Term, UnaryOp};
use crate::types::{AbsType, Types};
use std::collections::{HashMap, HashSet};

// Type checking
pub fn type_check(t: &Term) -> Result<Types, String> {
    let mut state = GTypes::new();
    let mut constr = GConstr::new();
    let ty = TypeWrapper::Ptr(new_var(&mut state));
    type_check_(
        HashMap::new(),
        &mut state,
        &mut constr,
        t,
        ty.clone(),
        false,
    )?;

    Ok(to_type(&state, ty)?)
}

fn type_check_(
    mut typed_vars: HashMap<Ident, TypeWrapper>,
    state: &mut GTypes,
    constr: &mut GConstr,
    t: &Term,
    ty: TypeWrapper,
    strict: bool,
) -> Result<(), String> {
    match t {
        Term::Bool(_) => unify(
            state,
            constr,
            ty,
            TypeWrapper::Concrete(AbsType::Bool()),
            strict,
        ),
        Term::Num(_) => unify(
            state,
            constr,
            ty,
            TypeWrapper::Concrete(AbsType::Num()),
            strict,
        ),
        Term::Str(_) => unify(
            state,
            constr,
            ty,
            TypeWrapper::Concrete(AbsType::Str()),
            strict,
        ),
        Term::Fun(x, rt) => {
            let src = TypeWrapper::Ptr(new_var(state));
            // TODO what to do here, this makes more sense to me, but it means let x = foo in bar
            // behaves quite different to (\x.bar) foo, worth considering if it's ok to type these two differently
            // let src = TypeWrapper::The(AbsType::Dyn());
            let trg = TypeWrapper::Ptr(new_var(state));
            let arr =
                TypeWrapper::Concrete(AbsType::arrow(Box::new(src.clone()), Box::new(trg.clone())));

            unify(state, constr, ty, arr, strict)?;

            typed_vars.insert(x.clone(), src);
            type_check_(typed_vars, state, constr, rt.as_ref(), trg, strict)
        }
        Term::Lbl(_) => {
            // TODO implement lbl type
            unify(
                state,
                constr,
                ty,
                TypeWrapper::Concrete(AbsType::Dyn()),
                strict,
            )
        }
        Term::Let(x, re, rt) => {
            let e = re.as_ref();

            // If the right hand side has a Promise or Assume, we use it as a
            // type annotation otherwise, x gets type Dyn
            let exp = match e {
                Term::Assume(ty, _, _) | Term::Promise(ty, _, _) => to_typewrapper(ty.clone()),
                _ => TypeWrapper::Concrete(AbsType::Dyn()),
            };

            let instantiated = instantiate_foralls_with(state, exp.clone(), TypeWrapper::Constant)?;
            type_check_(typed_vars.clone(), state, constr, e, instantiated, strict)?;

            // TODO move this up once lets are rec
            typed_vars.insert(x.clone(), exp);
            type_check_(typed_vars, state, constr, rt.as_ref(), ty, strict)
        }
        Term::App(re, rt) => {
            let src = TypeWrapper::Ptr(new_var(state));
            let arr = TypeWrapper::Concrete(AbsType::arrow(Box::new(src.clone()), Box::new(ty)));

            // This order shouldn't be changed, since applying a function to a record
            // may change how it's typed (static or dynamic)
            // This is good hint a bidirectional algorithm would make sense...
            type_check_(typed_vars.clone(), state, constr, re.as_ref(), arr, strict)?;
            type_check_(typed_vars, state, constr, rt.as_ref(), src, strict)
        }
        Term::Var(x) => {
            let x_ty = typed_vars
                .get(&x)
                .ok_or_else(|| format!("Found an unbound var {:?}", x))?;

            let instantiated = instantiate_foralls_with(state, x_ty.clone(), TypeWrapper::Ptr)?;
            unify(state, constr, ty, instantiated, strict)
        }
        Term::Enum(id) => {
            let row = TypeWrapper::Ptr(new_var(state));
            // Do we really need to constraint on enums?
            // What's the meaning of this?
            constraint(state, constr, row.clone(), id.clone())?;
            unify(
                state,
                constr,
                ty,
                TypeWrapper::Concrete(AbsType::Enum(Box::new(TypeWrapper::Concrete(
                    AbsType::RowExtend(id.clone(), None, Box::new(row)),
                )))),
                strict,
            )
        }
        Term::Record(stat_map) => {
            let root_ty = if let TypeWrapper::Ptr(p) = ty {
                get_root(state, p)?
            } else {
                ty.clone()
            };

            if let TypeWrapper::Concrete(AbsType::DynRecord(rec_ty)) = root_ty.clone() {
                // Checking for an dynamic record
                stat_map
                    .into_iter()
                    .try_for_each(|e| -> Result<(), String> {
                        let (_, t) = e;
                        type_check_(
                            typed_vars.clone(),
                            state,
                            constr,
                            t.as_ref(),
                            (*rec_ty).clone(),
                            strict,
                        )
                    })
            } else {
                // infering static record
                let row = stat_map.into_iter().try_fold(
                    TypeWrapper::Concrete(AbsType::RowEmpty()),
                    |acc, e| -> Result<TypeWrapper, String> {
                        let (id, t) = e;

                        let ty = TypeWrapper::Ptr(new_var(state));
                        type_check_(
                            typed_vars.clone(),
                            state,
                            constr,
                            t.as_ref(),
                            ty.clone(),
                            strict,
                        )?;

                        constraint(state, constr, acc.clone(), id.clone())?;

                        Ok(TypeWrapper::Concrete(AbsType::RowExtend(
                            id.clone(),
                            Some(Box::new(ty)),
                            Box::new(acc),
                        )))
                    },
                )?;

                unify(
                    state,
                    constr,
                    ty,
                    TypeWrapper::Concrete(AbsType::StaticRecord(Box::new(row))),
                    strict,
                )
            }
        }
        Term::Op1(op, rt) => {
            let ty_op = get_uop_type(typed_vars.clone(), state, constr, op, strict)?;

            let src = TypeWrapper::Ptr(new_var(state));
            let arr = TypeWrapper::Concrete(AbsType::arrow(Box::new(src.clone()), Box::new(ty)));

            unify(state, constr, arr, ty_op, strict)?;
            type_check_(typed_vars, state, constr, rt.as_ref(), src, strict)
        }
        Term::Op2(op, re, rt) => {
            let ty_op = get_bop_type(typed_vars.clone(), state, constr, op, strict)?;

            let src1 = TypeWrapper::Ptr(new_var(state));
            let src2 = TypeWrapper::Ptr(new_var(state));
            let arr = TypeWrapper::Concrete(AbsType::arrow(
                Box::new(src1.clone()),
                Box::new(TypeWrapper::Concrete(AbsType::arrow(
                    Box::new(src2.clone()),
                    Box::new(ty),
                ))),
            ));

            unify(state, constr, arr, ty_op, strict)?;
            type_check_(typed_vars.clone(), state, constr, re.as_ref(), src1, strict)?;
            type_check_(typed_vars, state, constr, rt.as_ref(), src2, strict)
        }
        Term::Promise(ty2, _, rt) => {
            let tyw2 = to_typewrapper(ty2.clone());

            let instantiated = instantiate_foralls_with(state, tyw2, TypeWrapper::Constant)?;

            unify(
                state,
                constr,
                ty.clone(),
                to_typewrapper(ty2.clone()),
                strict,
            )?;
            type_check_(typed_vars, state, constr, rt.as_ref(), instantiated, true)
        }
        Term::Assume(ty2, _, rt) => {
            unify(
                state,
                constr,
                ty.clone(),
                to_typewrapper(ty2.clone()),
                strict,
            )?;
            let new_ty = TypeWrapper::Ptr(new_var(state));
            type_check_(typed_vars, state, constr, rt.as_ref(), new_ty, false)
        }
        Term::Sym(_) => unify(
            state,
            constr,
            ty,
            TypeWrapper::Concrete(AbsType::Sym()),
            strict,
        ),
        Term::Wrapped(_, rt) => type_check_(typed_vars, state, constr, rt.as_ref(), ty, strict),
    }
}

// TypeWrapper
//   A type can be a concrete type, a constant or a type variable
#[derive(Clone, PartialEq, Debug)]
pub enum TypeWrapper {
    Concrete(AbsType<Box<TypeWrapper>>),
    Constant(usize),
    Ptr(usize),
}

impl TypeWrapper {
    pub fn subst(self, id: Ident, to: TypeWrapper) -> TypeWrapper {
        use self::TypeWrapper::*;
        match self {
            Concrete(AbsType::Var(ref i)) if *i == id => to,
            Concrete(AbsType::Var(i)) => Concrete(AbsType::Var(i)),

            Concrete(AbsType::Forall(i, t)) => {
                if i == id {
                    Concrete(AbsType::Forall(i, t))
                } else {
                    let tt = *t;
                    Concrete(AbsType::Forall(i, Box::new(tt.subst(id, to))))
                }
            }
            // Trivial recursion
            Concrete(AbsType::Dyn()) => Concrete(AbsType::Dyn()),
            Concrete(AbsType::Num()) => Concrete(AbsType::Num()),
            Concrete(AbsType::Bool()) => Concrete(AbsType::Bool()),
            Concrete(AbsType::Str()) => Concrete(AbsType::Str()),
            Concrete(AbsType::Sym()) => Concrete(AbsType::Sym()),
            Concrete(AbsType::Flat(t)) => Concrete(AbsType::Flat(t)),
            Concrete(AbsType::Arrow(s, t)) => {
                let fs = s.subst(id.clone(), to.clone());
                let ft = t.subst(id, to);

                Concrete(AbsType::Arrow(Box::new(fs), Box::new(ft)))
            }
            Concrete(AbsType::RowEmpty()) => Concrete(AbsType::RowEmpty()),
            Concrete(AbsType::RowExtend(tag, ty, rest)) => Concrete(AbsType::RowExtend(
                tag,
                ty.map(|x| Box::new(x.subst(id.clone(), to.clone()))),
                Box::new(rest.subst(id, to)),
            )),
            Concrete(AbsType::Enum(row)) => Concrete(AbsType::Enum(Box::new(row.subst(id, to)))),
            Concrete(AbsType::StaticRecord(row)) => {
                Concrete(AbsType::StaticRecord(Box::new(row.subst(id, to))))
            }
            Concrete(AbsType::DynRecord(def_ty)) => {
                Concrete(AbsType::DynRecord(Box::new(def_ty.subst(id, to))))
            }

            Constant(x) => Constant(x),
            Ptr(x) => Ptr(x),
        }
    }
}

/// Add an identifier to a row.
///
/// If the id is not there, and the row is open, it will add it with the passed type `ty`.
///
/// # Returns
///
/// The type bound with the id and the row without the id.
fn row_add(
    state: &mut GTypes,
    constr: &mut GConstr,
    id: Ident,
    ty: Option<Box<TypeWrapper>>,
    mut r: TypeWrapper,
) -> Result<(Option<Box<TypeWrapper>>, TypeWrapper), String> {
    if let TypeWrapper::Ptr(p) = r {
        r = get_root(state, p)?;
    }
    match r {
        TypeWrapper::Concrete(AbsType::RowEmpty()) => {
            Err("The row didn't have the id and it wasn't open".to_string())
        }
        TypeWrapper::Concrete(AbsType::RowExtend(id2, ty2, r2)) => {
            if id == id2 {
                Ok((ty2, *r2))
            } else {
                let (extracted_type, subrow) = row_add(state, constr, id, ty, *r2)?;
                Ok((
                    extracted_type,
                    TypeWrapper::Concrete(AbsType::RowExtend(id2, ty2, Box::new(subrow))),
                ))
            }
        }
        TypeWrapper::Concrete(not_row) => Err(format!("Expected a row, got {:?}", not_row)),
        TypeWrapper::Ptr(root) => {
            if let Some(set) = constr.get(&root) {
                if set.contains(&id) {
                    return Err(format!(
                        "Tried to add {:?} to the row, but it was constrained.",
                        id
                    ));
                }
            }
            let new_row = TypeWrapper::Ptr(new_var(state));
            constraint(state, constr, new_row.clone(), id.clone())?;
            state.insert(
                root,
                Some(TypeWrapper::Concrete(AbsType::RowExtend(
                    id,
                    ty.clone(),
                    Box::new(new_row.clone()),
                ))),
            );
            Ok((ty, new_row))
        }
        TypeWrapper::Constant(_) => Err("Expected a row, got a constant".to_string()),
    }
}

pub fn unify(
    state: &mut GTypes,
    constr: &mut GConstr,
    mut t1: TypeWrapper,
    mut t2: TypeWrapper,
    strict: bool,
) -> Result<(), String> {
    if !strict {
        // TODO think whether this makes sense, without this we can't write the Y combinator
        return Ok(());
    }
    if let TypeWrapper::Ptr(pt1) = t1 {
        t1 = get_root(state, pt1)?;
    }
    if let TypeWrapper::Ptr(pt2) = t2 {
        t2 = get_root(state, pt2)?;
    }

    // t1 and t2 are roots of the type
    match (t1, t2) {
        (TypeWrapper::Concrete(s1), TypeWrapper::Concrete(s2)) => match (s1, s2) {
            (AbsType::Dyn(), AbsType::Dyn()) => Ok(()),
            (AbsType::Num(), AbsType::Num()) => Ok(()),
            (AbsType::Bool(), AbsType::Bool()) => Ok(()),
            (AbsType::Str(), AbsType::Str()) => Ok(()),
            (AbsType::Sym(), AbsType::Sym()) => Ok(()),
            (AbsType::Arrow(s1s, s1t), AbsType::Arrow(s2s, s2t)) => {
                unify(state, constr, *s1s, *s2s, strict)?;
                unify(state, constr, *s1t, *s2t, strict)
            }
            (AbsType::Flat(s), AbsType::Flat(t)) => {
                if let Term::Var(s) = s.clone().into() {
                    if let Term::Var(t) = t.clone().into() {
                        if s == t {
                            return Ok(());
                        }
                    }
                }
                Err(format!(
                    "Two expressions didn't match {:?} - {:?}",
                    state, t
                ))
            } // Right now it only unifies equally named variables
            (AbsType::RowEmpty(), AbsType::RowEmpty()) => Ok(()),
            (AbsType::RowExtend(id, ty, t), r2 @ AbsType::RowExtend(_, _, _)) => {
                let (ty2, r2) = row_add(state, constr, id, ty.clone(), TypeWrapper::Concrete(r2))?;

                match (ty, ty2) {
                    (None, None) => Ok(()),
                    (Some(ty), Some(ty2)) => unify(state, constr, *ty, *ty2, strict),
                    _ => Err("Couldn't unify two types from a row!".to_string()),
                }?;
                unify(state, constr, *t, r2, strict)
            }
            (AbsType::Enum(r), AbsType::Enum(r2)) => unify(state, constr, *r, *r2, strict),
            (AbsType::StaticRecord(r), AbsType::StaticRecord(r2)) => {
                unify(state, constr, *r, *r2, strict)
            }
            (AbsType::DynRecord(t), AbsType::DynRecord(t2)) => {
                unify(state, constr, *t, *t2, strict)
            }
            (AbsType::Var(ref i1), AbsType::Var(ref i2)) if i1 == i2 => Ok(()),
            (AbsType::Forall(i1, t1t), AbsType::Forall(i2, t2t)) => {
                // Very stupid (slow) implementation
                let constant_type = TypeWrapper::Constant(new_var(state));

                unify(
                    state,
                    constr,
                    t1t.subst(i1, constant_type.clone()),
                    t2t.subst(i2, constant_type),
                    strict,
                )
            }
            (a, b) => Err(format!("The following types dont match {:?} -- {:?}", a, b)),
        },
        (TypeWrapper::Ptr(r1), TypeWrapper::Ptr(r2)) => {
            if r1 != r2 {
                let mut r1_constr = constr.remove(&r1).unwrap_or_default();
                let mut r2_constr = constr.remove(&r2).unwrap_or_default();
                constr.insert(r1, r1_constr.drain().chain(r2_constr.drain()).collect());

                state.insert(r1, Some(TypeWrapper::Ptr(r2)));
            }
            Ok(())
        }

        (TypeWrapper::Ptr(p), s @ TypeWrapper::Concrete(_))
        | (TypeWrapper::Ptr(p), s @ TypeWrapper::Constant(_))
        | (s @ TypeWrapper::Concrete(_), TypeWrapper::Ptr(p))
        | (s @ TypeWrapper::Constant(_), TypeWrapper::Ptr(p)) => {
            state.insert(p, Some(s));
            Ok(())
        }
        (TypeWrapper::Constant(i1), TypeWrapper::Constant(i2)) if i1 == i2 => Ok(()),
        (a, b) => Err(format!("Couldn't unify {:?} and {:?}", a, b)),
    }
}

fn to_typewrapper(t: Types) -> TypeWrapper {
    let Types(t2) = t;

    let t3 = t2.map(|x| Box::new(to_typewrapper(*x)));

    TypeWrapper::Concrete(t3)
}

fn to_type(state: &GTypes, ty: TypeWrapper) -> Result<Types, String> {
    Ok(match ty {
        TypeWrapper::Ptr(p) => match get_root(state, p)? {
            t @ TypeWrapper::Concrete(_) => to_type(state, t)?,
            _ => Types(AbsType::Dyn()),
        },
        TypeWrapper::Constant(_) => Types(AbsType::Dyn()),
        TypeWrapper::Concrete(t) => {
            let mapped = t.map(|btyp| Box::new(to_type(state, *btyp).unwrap()));
            Types(mapped)
        }
    })
}

fn instantiate_foralls_with<F>(
    state: &mut GTypes,
    mut ty: TypeWrapper,
    f: F,
) -> Result<TypeWrapper, String>
where
    F: Fn(usize) -> TypeWrapper,
{
    if let TypeWrapper::Ptr(p) = ty {
        ty = get_root(state, p)?;
    }

    while let TypeWrapper::Concrete(AbsType::Forall(id, forall_ty)) = ty {
        let var = f(new_var(state));
        ty = forall_ty.subst(id, var);
    }
    Ok(ty)
}

pub fn get_uop_type(
    typed_vars: HashMap<Ident, TypeWrapper>,
    state: &mut GTypes,
    constr: &mut GConstr,
    op: &UnaryOp<RichTerm>,
    strict: bool,
) -> Result<TypeWrapper, String> {
    Ok(match op {
        UnaryOp::Ite() => {
            let branches = TypeWrapper::Ptr(new_var(state));

            TypeWrapper::Concrete(AbsType::arrow(
                Box::new(TypeWrapper::Concrete(AbsType::Bool())),
                Box::new(TypeWrapper::Concrete(AbsType::arrow(
                    Box::new(branches.clone()),
                    Box::new(TypeWrapper::Concrete(AbsType::arrow(
                        Box::new(branches.clone()),
                        Box::new(branches),
                    ))),
                ))),
            ))
        }
        UnaryOp::IsZero() => TypeWrapper::Concrete(AbsType::arrow(
            Box::new(TypeWrapper::Concrete(AbsType::Num())),
            Box::new(TypeWrapper::Concrete(AbsType::Bool())),
        )),
        UnaryOp::IsNum() | UnaryOp::IsBool() | UnaryOp::IsStr() | UnaryOp::IsFun() => {
            let inp = TypeWrapper::Ptr(new_var(state));

            TypeWrapper::Concrete(AbsType::arrow(
                Box::new(inp),
                Box::new(TypeWrapper::Concrete(AbsType::Bool())),
            ))
        }
        UnaryOp::Blame() => {
            let res = TypeWrapper::Ptr(new_var(state));

            TypeWrapper::Concrete(AbsType::arrow(
                Box::new(TypeWrapper::Concrete(AbsType::Dyn())),
                Box::new(res),
            ))
        }
        UnaryOp::Pol() => TypeWrapper::Concrete(AbsType::arrow(
            Box::new(TypeWrapper::Concrete(AbsType::Dyn())),
            Box::new(TypeWrapper::Concrete(AbsType::Bool())),
        )),

        UnaryOp::Embed(id) => {
            let row = TypeWrapper::Ptr(new_var(state));
            constraint(state, constr, row.clone(), id.clone())?;

            TypeWrapper::Concrete(AbsType::Arrow(
                Box::new(TypeWrapper::Concrete(AbsType::Enum(Box::new(row.clone())))),
                Box::new(TypeWrapper::Concrete(AbsType::Enum(Box::new(
                    TypeWrapper::Concrete(AbsType::RowExtend(id.clone(), None, Box::new(row))),
                )))),
            ))
        }
        UnaryOp::Switch(l, d) => {
            // Currently, if it has a default value, we typecheck the whole thing as
            // taking ANY enum, since it's more permissive and there's not a loss of information
            let res = TypeWrapper::Ptr(new_var(state));

            for exp in l.values() {
                type_check_(
                    typed_vars.clone(),
                    state,
                    constr,
                    exp.as_ref(),
                    res.clone(),
                    strict,
                )?;
            }

            let row = match d {
                Some(e) => {
                    type_check_(
                        typed_vars.clone(),
                        state,
                        constr,
                        e.as_ref(),
                        res.clone(),
                        strict,
                    )?;
                    TypeWrapper::Ptr(new_var(state))
                }
                None => l.iter().try_fold(
                    TypeWrapper::Concrete(AbsType::RowEmpty()),
                    |acc, x| -> Result<TypeWrapper, String> {
                        constraint(state, constr, acc.clone(), x.0.clone())?;
                        Ok(TypeWrapper::Concrete(AbsType::RowExtend(
                            x.0.clone(),
                            None,
                            Box::new(acc),
                        )))
                    },
                )?,
            };

            TypeWrapper::Concrete(AbsType::Arrow(
                Box::new(TypeWrapper::Concrete(AbsType::Enum(Box::new(row)))),
                Box::new(res),
            ))
        }

        UnaryOp::ChangePolarity() | UnaryOp::GoDom() | UnaryOp::GoCodom() | UnaryOp::Tag(_) => {
            TypeWrapper::Concrete(AbsType::arrow(
                Box::new(TypeWrapper::Concrete(AbsType::Dyn())),
                Box::new(TypeWrapper::Concrete(AbsType::Dyn())),
            ))
        }
        UnaryOp::Wrap() => TypeWrapper::Concrete(AbsType::arrow(
            Box::new(TypeWrapper::Concrete(AbsType::Sym())),
            Box::new(TypeWrapper::Concrete(AbsType::arrow(
                Box::new(TypeWrapper::Concrete(AbsType::Dyn())),
                Box::new(TypeWrapper::Concrete(AbsType::Dyn())),
            ))),
        )),
        UnaryOp::StaticAccess(id) => {
            let row = TypeWrapper::Ptr(new_var(state));
            let res = TypeWrapper::Ptr(new_var(state));

            TypeWrapper::Concrete(AbsType::arrow(
                Box::new(TypeWrapper::Concrete(AbsType::StaticRecord(Box::new(
                    TypeWrapper::Concrete(AbsType::RowExtend(
                        id.clone(),
                        Some(Box::new(res.clone())),
                        Box::new(row),
                    )),
                )))),
                Box::new(res),
            ))
        }
        UnaryOp::MapRec(f) => {
            // Assuming f has type Str -> a -> b,
            // this has type DynRecord(a) -> DynRecord(b)

            let a = TypeWrapper::Ptr(new_var(state));
            let b = TypeWrapper::Ptr(new_var(state));

            let f_type = TypeWrapper::Concrete(AbsType::Arrow(
                Box::new(TypeWrapper::Concrete(AbsType::Str())),
                Box::new(TypeWrapper::Concrete(AbsType::Arrow(
                    Box::new(a.clone()),
                    Box::new(b.clone()),
                ))),
            ));

            type_check_(
                typed_vars.clone(),
                state,
                constr,
                f.as_ref(),
                f_type,
                strict,
            )?;

            TypeWrapper::Concrete(AbsType::Arrow(
                Box::new(TypeWrapper::Concrete(AbsType::DynRecord(Box::new(a)))),
                Box::new(TypeWrapper::Concrete(AbsType::DynRecord(Box::new(b)))),
            ))
        }
    })
}

pub fn get_bop_type(
    typed_vars: HashMap<Ident, TypeWrapper>,
    state: &mut GTypes,
    constr: &mut GConstr,
    op: &BinaryOp<RichTerm>,
    strict: bool,
) -> Result<TypeWrapper, String> {
    match op {
        BinaryOp::Plus() => Ok(TypeWrapper::Concrete(AbsType::arrow(
            Box::new(TypeWrapper::Concrete(AbsType::Num())),
            Box::new(TypeWrapper::Concrete(AbsType::arrow(
                Box::new(TypeWrapper::Concrete(AbsType::Num())),
                Box::new(TypeWrapper::Concrete(AbsType::Num())),
            ))),
        ))),
        BinaryOp::PlusStr() => Ok(TypeWrapper::Concrete(AbsType::arrow(
            Box::new(TypeWrapper::Concrete(AbsType::Str())),
            Box::new(TypeWrapper::Concrete(AbsType::arrow(
                Box::new(TypeWrapper::Concrete(AbsType::Str())),
                Box::new(TypeWrapper::Concrete(AbsType::Str())),
            ))),
        ))),
        BinaryOp::Unwrap() => Ok(TypeWrapper::Concrete(AbsType::arrow(
            Box::new(TypeWrapper::Concrete(AbsType::Sym())),
            Box::new(TypeWrapper::Concrete(AbsType::arrow(
                Box::new(TypeWrapper::Concrete(AbsType::Dyn())),
                Box::new(TypeWrapper::Concrete(AbsType::arrow(
                    Box::new(TypeWrapper::Concrete(AbsType::Dyn())),
                    Box::new(TypeWrapper::Concrete(AbsType::Dyn())),
                ))),
            ))),
        ))),
        BinaryOp::EqBool() => Ok(TypeWrapper::Concrete(AbsType::arrow(
            Box::new(TypeWrapper::Concrete(AbsType::Bool())),
            Box::new(TypeWrapper::Concrete(AbsType::arrow(
                Box::new(TypeWrapper::Concrete(AbsType::Bool())),
                Box::new(TypeWrapper::Concrete(AbsType::Bool())),
            ))),
        ))),
        BinaryOp::DynAccess() => {
            let res = TypeWrapper::Ptr(new_var(state));

            Ok(TypeWrapper::Concrete(AbsType::arrow(
                Box::new(TypeWrapper::Concrete(AbsType::Str())),
                Box::new(TypeWrapper::Concrete(AbsType::arrow(
                    Box::new(TypeWrapper::Concrete(AbsType::DynRecord(Box::new(
                        res.clone(),
                    )))),
                    Box::new(res),
                ))),
            )))
        }
        BinaryOp::DynExtend(t) => {
            let res = TypeWrapper::Ptr(new_var(state));

            type_check_(
                typed_vars.clone(),
                state,
                constr,
                t.as_ref(),
                res.clone(),
                strict,
            )?;

            Ok(TypeWrapper::Concrete(AbsType::arrow(
                Box::new(TypeWrapper::Concrete(AbsType::Str())),
                Box::new(TypeWrapper::Concrete(AbsType::arrow(
                    Box::new(TypeWrapper::Concrete(AbsType::DynRecord(Box::new(
                        res.clone(),
                    )))),
                    Box::new(TypeWrapper::Concrete(AbsType::DynRecord(Box::new(
                        res.clone(),
                    )))),
                ))),
            )))
        }
        BinaryOp::HasField() => Ok(TypeWrapper::Concrete(AbsType::Arrow(
            Box::new(TypeWrapper::Concrete(AbsType::Arrow(
                Box::new(TypeWrapper::Concrete(AbsType::Str())),
                Box::new(TypeWrapper::Concrete(AbsType::Dyn())),
            ))),
            Box::new(TypeWrapper::Concrete(AbsType::Bool())),
        ))),
    }
}

// Global types, where unification happens

pub type GTypes = HashMap<usize, Option<TypeWrapper>>;
pub type GConstr = HashMap<usize, HashSet<Ident>>;

fn new_var(state: &mut GTypes) -> usize {
    let nxt = state.len();
    state.insert(nxt, None);
    nxt
}

fn constraint(
    state: &mut GTypes,
    constr: &mut GConstr,
    x: TypeWrapper,
    id: Ident,
) -> Result<(), String> {
    match x {
        TypeWrapper::Ptr(p) => {
            let ty = get_root(state, p)?;
            match ty {
                ty @ TypeWrapper::Concrete(_) => constraint(state, constr, ty, id),
                TypeWrapper::Ptr(root) => {
                    if let Some(v) = constr.get_mut(&root) {
                        v.insert(id);
                    } else {
                        constr.insert(root, vec![id].into_iter().collect());
                    }
                    Ok(())
                }
                TypeWrapper::Constant(_) => Err("Can't constraint a constant".to_string()),
            }
        }
        TypeWrapper::Concrete(AbsType::RowEmpty()) => Ok(()),
        TypeWrapper::Concrete(AbsType::RowExtend(id2, _, t)) => {
            if id2 == id {
                Err(format!("The id {:?} was present on the row", id))
            } else {
                constraint(state, constr, *t, id)
            }
        }
        _ => Err(format!("Can't constraint a {:?}", x)),
    }
}

// TODO This should be a union find like algorithm
pub fn get_root(state: &GTypes, x: usize) -> Result<TypeWrapper, String> {
    match state
        .get(&x)
        .ok_or_else(|| format!("Unbound type variable {}!", x))?
    {
        None => Ok(TypeWrapper::Ptr(x)),
        Some(TypeWrapper::Ptr(y)) => get_root(state, *y),
        Some(ty @ TypeWrapper::Concrete(_)) => Ok(ty.clone()),
        Some(k @ TypeWrapper::Constant(_)) => Ok(k.clone()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::label::{Label, TyPath};

    use crate::parser;

    fn parse_and_typecheck(s: &str) -> Result<Types, String> {
        if let Ok(p) = parser::grammar::TermParser::new().parse(s) {
            type_check(p.as_ref())
        } else {
            panic!("Couldn't parse")
        }
    }

    fn label() -> Label {
        Label {
            tag: "".into(),
            l: 0,
            r: 0,
            polarity: true,
            path: TyPath::Nil(),
        }
    }

    #[test]
    fn simple_no_promises() -> Result<(), String> {
        // It's easy to check these will never fail, that's why we keep them all together

        type_check(&Term::Bool(true))?;
        type_check(&Term::Num(45.))?;

        type_check(&Term::Fun(Ident("x".into()), RichTerm::var("x".into())))?;
        type_check(&Term::Let(
            Ident("x".into()),
            Term::Num(3.).into(),
            RichTerm::var("x".into()),
        ))?;

        type_check(&Term::App(Term::Num(5.).into(), Term::Bool(true).into()))?;
        type_check(RichTerm::plus(Term::Num(4.).into(), Term::Bool(false).into()).as_ref())?;

        Ok(())
    }

    #[test]
    fn unbound_variable_always_throws() {
        type_check(&Term::Var(Ident("x".into()))).unwrap_err();
    }

    #[test]
    fn promise_simple_checks() {
        type_check(&Term::Promise(
            Types(AbsType::Bool()),
            label(),
            Term::Bool(true).into(),
        ))
        .unwrap();
        type_check(&Term::Promise(
            Types(AbsType::Num()),
            label(),
            Term::Bool(true).into(),
        ))
        .unwrap_err();

        type_check(&Term::Promise(
            Types(AbsType::Num()),
            label(),
            Term::Num(34.5).into(),
        ))
        .unwrap();
        type_check(&Term::Promise(
            Types(AbsType::Bool()),
            label(),
            Term::Num(34.5).into(),
        ))
        .unwrap_err();

        type_check(&Term::Promise(
            Types(AbsType::Num()),
            label(),
            Term::Assume(Types(AbsType::Num()), label(), Term::Bool(true).into()).into(),
        ))
        .unwrap();
        type_check(&Term::Promise(
            Types(AbsType::Num()),
            label(),
            Term::Assume(Types(AbsType::Bool()), label(), Term::Num(34.).into()).into(),
        ))
        .unwrap_err();

        parse_and_typecheck("Promise(Str, \"hello\")").unwrap();
        parse_and_typecheck("Promise(Num, \"hello\")").unwrap_err();
    }

    #[test]
    fn promise_complicated() {
        // Inside Promises we typecheck strictly
        parse_and_typecheck("(fun x => if x then x + 1 else 34) false").unwrap();
        parse_and_typecheck("Promise(Bool -> Num, fun x => if x then x + 1 else 34) false")
            .unwrap_err();

        // not annotated let bindings type to Dyn
        parse_and_typecheck(
            "let id = Promise(Num -> Num, fun x => x) in
            Promise(Num, id 4)",
        )
        .unwrap();
        parse_and_typecheck(
            "let id = fun x => x in
            Promise(Num, id 4)",
        )
        .unwrap_err();

        // lambdas don't annotate to Dyn
        parse_and_typecheck("(fun id => Promise(Num, id 4)) (fun x => x)").unwrap();

        // But they are not polymorphic
        parse_and_typecheck("(fun id => Promise(Num, id 4) + Promise(Bool, id true)) (fun x => x)")
            .unwrap_err();

        // Non strict zones don't unify
        parse_and_typecheck("(fun id => (id 4) + Promise(Bool, id true)) (fun x => x)").unwrap();

        // We can typecheck any contract
        parse_and_typecheck(
            "let alwaysTrue = fun l => fun t => if t then t else blame l in
        Promise(#alwaysTrue -> #alwaysTrue, fun x => x)",
        )
        .unwrap();
        // Only if they're named the same way
        parse_and_typecheck(
            "Promise(#(fun l => fun t => t) -> #(fun l => fun t => t), fun x => x)",
        )
        .unwrap_err();
    }

    #[test]
    fn simple_forall() {
        parse_and_typecheck(
            "let f = Promise(forall a. a -> a, fun x => x) in
        Promise(Num, if (f true) then (f 2) else 3)",
        )
        .unwrap();

        parse_and_typecheck(
            "let f = Promise(forall a. (forall b. a -> b -> a), fun x => fun y => x) in
        Promise(Num, if (f true 3) then (f 2 false) else 3)",
        )
        .unwrap();

        parse_and_typecheck(
            "let f = Promise(forall a. (forall b. b -> b) -> a -> a, fun f => fun x => f x) in
            f Promise(forall y. y -> y, fun z => z)",
        )
        .unwrap();

        parse_and_typecheck(
            "let f = Promise(forall a. (forall b. a -> b -> a), fun x => fun y => y) in
            f",
        )
        .unwrap_err();

        parse_and_typecheck(
            "Promise(
                ((forall a. a -> a) -> Num) -> Num,
                fun f => let g = Promise(forall b. b -> b, fun y => y) in f g)
            (fun x => 3)",
        )
        .unwrap_err();

        parse_and_typecheck(
            "let g = Promise(Num -> Num, fun x => x) in
        let f = Promise(forall a. a -> a, fun x =>  g x) in
        f",
        )
        .unwrap_err();
    }

    #[test]
    fn enum_simple() {
        parse_and_typecheck("Promise(< (| bla, |) >, `bla)").unwrap();
        parse_and_typecheck("Promise(< (| bla, |) >, `blo)").unwrap_err();

        parse_and_typecheck("Promise(< (| bla, blo, |) >, `blo)").unwrap();
        parse_and_typecheck("Promise(forall r. < (| bla, | r ) >, `bla)").unwrap();
        parse_and_typecheck("Promise(forall r. < (| bla, blo, | r ) >, `bla)").unwrap();

        parse_and_typecheck("Promise(Num, switch { bla => 3, } `bla)").unwrap();
        parse_and_typecheck("Promise(Num, switch { bla => 3, } `blo)").unwrap_err();

        parse_and_typecheck("Promise(Num, switch { bla => 3, _ => 2, } `blo)").unwrap();
        parse_and_typecheck("Promise(Num, switch { bla => 3, ble => true, } `bla)").unwrap_err();
    }

    #[test]
    fn enum_complex() {
        parse_and_typecheck(
            "Promise(< (| bla, ble, |) > -> Num, fun x => switch {bla => 1, ble => 2,} x)",
        )
        .unwrap();
        parse_and_typecheck(
            "Promise(< (| bla, ble, |) > -> Num,
        fun x => switch {bla => 1, ble => 2, bli => 4,} x)",
        )
        .unwrap_err();
        parse_and_typecheck(
            "Promise(< (| bla, ble, |) > -> Num,
        fun x => switch {bla => 1, ble => 2, bli => 4,} (embed bli x))",
        )
        .unwrap();

        parse_and_typecheck(
            "Promise(Num, 
            (fun x => 
                (switch {bla => 3, bli => 2,} x) + 
                (switch {bli => 6, bla => 20,} x) ) `bla)",
        )
        .unwrap();
        // TODO typecheck this, I'm not sure how to do it with row variables
        parse_and_typecheck(
            "Promise(Num, 
            (fun x => 
                (switch {bla => 3, bli => 2,} x) + 
                (switch {bla => 6, blo => 20,} x) ) `bla)",
        )
        .unwrap_err();

        parse_and_typecheck(
            "let f = Promise(
                forall r. < (| blo, ble, | r )> -> Num,
                fun x => (switch {blo => 1, ble => 2, _ => 3, } x ) ) in
            Promise(Num, f `bli)",
        )
        .unwrap();
        parse_and_typecheck(
            "let f = Promise(
                forall r. < (| blo, ble, | r )> -> Num,
                fun x => (switch {blo => 1, ble => 2, bli => 3, } x ) ) in
            f",
        )
        .unwrap_err();

        parse_and_typecheck(
            "let f = Promise(
                forall r. (forall p. < (| blo, ble, | r )> -> < (| bla, bli, | p) > ),
                fun x => (switch {blo => `bla, ble => `bli, _ => `bla, } x ) ) in
            f `bli",
        )
        .unwrap();
        parse_and_typecheck(
            "let f = Promise(
                forall r. (forall p. < (| blo, ble, | r )> -> < (| bla, bli, | p) > ),
                fun x => (switch {blo => `bla, ble => `bli, _ => `blo, } x ) ) in
            f `bli",
        )
        .unwrap_err();
    }

    #[test]
    fn static_record_simple() {
        parse_and_typecheck("Promise({ {| bla : Num, |} }, { bla = 1; })").unwrap();
        parse_and_typecheck("Promise({ {| bla : Num, |} }, { bla = true; })").unwrap_err();
        parse_and_typecheck("Promise({ {| bla : Num, |} }, { blo = 1; })").unwrap_err();

        parse_and_typecheck("Promise({ {| bla : Num, blo : Bool, |} }, { blo = true; bla = 1; })")
            .unwrap();

        parse_and_typecheck("Promise(Num, { blo = 1; }.blo)").unwrap();
        parse_and_typecheck("Promise(Num, { bla = true; blo = 1; }.blo)").unwrap();
        parse_and_typecheck("Promise(Bool, { blo = 1; }.blo)").unwrap_err();

        parse_and_typecheck(
            "let r = Promise({ {| bla : Bool, blo : Num, |} }, {blo = 1; bla = true; }) in
        Promise(Num, if r.bla then r.blo else 2)",
        )
        .unwrap();

        // It worked at first try :O
        parse_and_typecheck(
            "let f = Promise(
                forall a. (forall r. { {| bla : Bool, blo : a, ble : a, | r } } -> a),
                fun r => if r.bla then r.blo else r.ble) 
            in
            Promise(Num, 
                if (f {bla = true; blo = false; ble = true; blip = 1; }) then
                    (f {bla = true; blo = 1; ble = 2; blip = `blip; })
                else
                    (f {bla = true; blo = 3; ble = 4; bloppo = `bloppop; }))",
        )
        .unwrap();

        parse_and_typecheck(
            "let f = Promise(
                forall a. (forall r. { {| bla : Bool, blo : a, ble : a, | r } } -> a),
                fun r => if r.bla then r.blo else r.ble) 
            in
            Promise(Num, 
                    f {bla = true; blo = 1; ble = true; blip = `blip; })
                ",
        )
        .unwrap_err();
        parse_and_typecheck(
            "let f = Promise(
                forall a. (forall r. { {| bla : Bool, blo : a, ble : a, | r } } -> a),
                fun r => if r.bla then (r.blo + 1) else r.ble) 
            in
            Promise(Num, 
                    f {bla = true; blo = 1; ble = 2; blip = `blip; })
                ",
        )
        .unwrap_err();
    }

    #[test]
    fn dynamic_record_simple() {
        parse_and_typecheck("Promise({ _ : Num }, { $(if true then \"foo\" else \"bar\") = 2; } )")
            .unwrap();

        parse_and_typecheck(
            "Promise(Num, { $(if true then \"foo\" else \"bar\") = 2; }.$(\"bla\"))",
        )
        .unwrap();

        parse_and_typecheck(
            "Promise(
                Num, 
                { $(if true then \"foo\" else \"bar\") = 2; $(\"foo\") = true; }.$(\"bla\"))",
        )
        .unwrap_err();

        parse_and_typecheck("Promise( { _ : Num}, { foo = 3; bar = 4; })").unwrap();
    }
}
