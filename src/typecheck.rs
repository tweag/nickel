use identifier::Ident;
use std::collections::HashMap;
use term::{BinaryOp, Term, UnaryOp};
use types::{AbsType, Types};

#[derive(Clone, PartialEq, Debug)]
pub enum TypeWrapper {
    The(AbsType<Box<TypeWrapper>>),
    Ptr(usize),
}

// Global types, where unification happens
pub type GTypes = HashMap<usize, Option<TypeWrapper>>;

// This should be a union find like algorithm
pub fn getRoot(s: &GTypes, x: usize) -> (Option<AbsType<Box<TypeWrapper>>>, usize) {
    match s.get(&x).expect("Unbound type variable!") {
        None => (None, x),
        Some(TypeWrapper::Ptr(y)) => getRoot(s, *y),
        Some(TypeWrapper::The(ty)) => (Some(ty.clone()), x),
    }
}

pub fn get_uop_type(s: &mut GTypes, op: UnaryOp) -> TypeWrapper {
    match op {
        UnaryOp::Ite() => {
            let branches = TypeWrapper::Ptr(new_var(s));

            TypeWrapper::The(AbsType::arrow(
                Box::new(TypeWrapper::The(AbsType::bool())),
                Box::new(TypeWrapper::The(AbsType::arrow(
                    Box::new(branches.clone()),
                    Box::new(TypeWrapper::The(AbsType::arrow(
                        Box::new(branches.clone()),
                        Box::new(branches),
                    ))),
                ))),
            ))
        }
        UnaryOp::IsZero() => TypeWrapper::The(AbsType::arrow(
            Box::new(TypeWrapper::The(AbsType::num())),
            Box::new(TypeWrapper::The(AbsType::bool())),
        )),
        UnaryOp::IsNum() | UnaryOp::IsBool() | UnaryOp::IsFun() => {
            let inp = TypeWrapper::Ptr(new_var(s));

            TypeWrapper::The(AbsType::arrow(
                Box::new(inp),
                Box::new(TypeWrapper::The(AbsType::bool())),
            ))
        }
        UnaryOp::Blame() => {
            let res = TypeWrapper::Ptr(new_var(s));

            TypeWrapper::The(AbsType::arrow(
                Box::new(TypeWrapper::The(AbsType::dyn())),
                Box::new(res),
            ))
        }
        UnaryOp::ChangePolarity() | UnaryOp::GoDom() | UnaryOp::GoCodom() | UnaryOp::Tag(_) => {
            TypeWrapper::The(AbsType::arrow(
                Box::new(TypeWrapper::The(AbsType::dyn())),
                Box::new(TypeWrapper::The(AbsType::dyn())),
            ))
        }
    }
}

pub fn get_bop_type(s: &mut GTypes, op: BinaryOp) -> TypeWrapper {
    match op {
        BinaryOp::Plus() => TypeWrapper::The(AbsType::arrow(
            Box::new(TypeWrapper::The(AbsType::num())),
            Box::new(TypeWrapper::The(AbsType::arrow(
                Box::new(TypeWrapper::The(AbsType::num())),
                Box::new(TypeWrapper::The(AbsType::num())),
            ))),
        )),
    }
}

pub fn unify(state: &mut GTypes, t1: TypeWrapper, t2: TypeWrapper) -> Result<(), String> {
    match (t1, t2) {
        (TypeWrapper::The(s1), TypeWrapper::The(s2)) => match (s1, s2) {
            (AbsType::Dyn(), _) => Ok(()),
            (_, AbsType::Dyn()) => Ok(()),
            (AbsType::Num(), AbsType::Num()) => Ok(()),
            (AbsType::Bool(), AbsType::Bool()) => Ok(()),
            (AbsType::Arrow(s1s, s1t), AbsType::Arrow(s2s, s2t)) => {
                unify(state, *s1s, *s2s)?;
                unify(state, *s1t, *s2t)
            }
            (AbsType::Flat(s), AbsType::Flat(t)) => {
                if let Term::Var(s) = s.clone().into() {
                    if let Term::Var(t) = t.clone().into() {
                        if s == t {
                            return Ok(());
                        }
                    }
                }
                Err(format!("Two expressions didn't match {:?} - {:?}", s, t))
            } // Right now it only unifies equally named variables
            (a, b) => Err(format!("The following types dont match {:?} -- {:?}", a, b)),
        },
        (TypeWrapper::Ptr(p1), TypeWrapper::Ptr(p2)) => {
            let (ty1, r1) = getRoot(&state, p1);
            let (ty2, r2) = getRoot(&state, p2);

            match (ty1, ty2) {
                (None, None) => {
                    if r1 != r2 {
                        state.insert(r1, Some(TypeWrapper::Ptr(r2)));
                    }
                    Ok(())
                }
                (Some(_), None) => {
                    state.insert(r2, Some(TypeWrapper::Ptr(r1)));
                    Ok(())
                }
                (None, Some(_)) => {
                    state.insert(r1, Some(TypeWrapper::Ptr(r2)));
                    Ok(())
                }
                (Some(ty1), Some(ty2)) => {
                    unify(state, TypeWrapper::The(ty1), TypeWrapper::The(ty2))
                }
            }
        }
        (TypeWrapper::Ptr(p), TypeWrapper::The(s)) | (TypeWrapper::The(s), TypeWrapper::Ptr(p)) => {
            let (p_ty_opt, root) = getRoot(&state, p);

            if let Some(p_ty) = p_ty_opt {
                unify(state, TypeWrapper::The(p_ty), TypeWrapper::The(s))
            } else {
                state.insert(root, Some(TypeWrapper::The(s)));
                Ok(())
            }
        }
    }
}

fn new_var(state: &mut GTypes) -> usize {
    let nxt = state.len();
    state.insert(nxt, None);
    nxt
}

pub fn typeCheck(
    bound_vars: &HashMap<Ident, Term>,
    mut typed_vars: HashMap<Ident, TypeWrapper>,
    s: &mut GTypes,
    t: Term,
    ty: TypeWrapper,
) -> Result<(), String> {
    match t {
        Term::Bool(_) => unify(s, ty, TypeWrapper::The(AbsType::bool())),
        Term::Num(_) => unify(s, ty, TypeWrapper::The(AbsType::num())),
        Term::Fun(x, rt) => {
            let t = rt.into();
            let src = TypeWrapper::Ptr(new_var(s));
            let trg = TypeWrapper::Ptr(new_var(s));
            let arr =
                TypeWrapper::The(AbsType::arrow(Box::new(src.clone()), Box::new(trg.clone())));

            let is_fun = unify(s, ty, arr)?;

            typed_vars.insert(x, src);
            typeCheck(bound_vars, typed_vars, s, t, trg)
        }
        Term::Lbl(_) => {
            // TODO implement lbl type
            unify(s, ty, TypeWrapper::The(AbsType::dyn()))
        }
        Term::Let(x, re, rt) => {
            let e = re.into();
            let t = rt.into();

            let exp = TypeWrapper::Ptr(new_var(s));

            typed_vars.insert(x, exp.clone());
            typeCheck(bound_vars, typed_vars.clone(), s, e, exp)?;
            typeCheck(bound_vars, typed_vars, s, t, ty)
        }
        Term::App(re, rt) => {
            let e = re.into();
            let t = rt.into();

            let src = TypeWrapper::Ptr(new_var(s));
            let arr = TypeWrapper::The(AbsType::arrow(Box::new(src.clone()), Box::new(ty)));

            typeCheck(bound_vars, typed_vars.clone(), s, e, arr)?;
            typeCheck(bound_vars, typed_vars, s, t, src)
        }
        Term::Var(x) => {
            if typed_vars.contains_key(&x) {
                // This is needed because otherwise typed_vars gets borrowed
                let x_ty = typed_vars.get(&x).expect("Already checked");
                unify(s, ty, x_ty.clone())
            } else if let Some(x_term) = bound_vars.get(&x) {
                typeCheck(bound_vars, typed_vars, s, x_term.clone(), ty)
            } else {
                println!("Found an unbound term, it shall be Dynamic!");
                unify(s, ty, TypeWrapper::The(AbsType::dyn()))
            }
        }
        Term::Op1(op, rt) => {
            let ty_op = get_uop_type(s, op);
            let t = rt.into();

            let src = TypeWrapper::Ptr(new_var(s));
            let trg = TypeWrapper::Ptr(new_var(s));
            let arr =
                TypeWrapper::The(AbsType::arrow(Box::new(src.clone()), Box::new(trg.clone())));

            unify(s, arr, ty_op)?;
            unify(s, trg, ty)?;
            typeCheck(bound_vars, typed_vars, s, t, src)
        }
        Term::Op2(op, re, rt) => {
            let ty_op = get_bop_type(s, op);
            let t = rt.into();
            let e = re.into();

            let src1 = TypeWrapper::Ptr(new_var(s));
            let src2 = TypeWrapper::Ptr(new_var(s));
            let trg = TypeWrapper::Ptr(new_var(s));
            let arr = TypeWrapper::The(AbsType::arrow(
                Box::new(src1.clone()),
                Box::new(TypeWrapper::The(AbsType::arrow(
                    Box::new(src2.clone()),
                    Box::new(trg.clone()),
                ))),
            ));

            unify(s, arr, ty_op)?;
            unify(s, trg, ty)?;
            typeCheck(bound_vars, typed_vars.clone(), s, e, src1)?;
            typeCheck(bound_vars, typed_vars, s, t, src2)
        }
        Term::Promise(ty2, _, rt) => {
            // I think the result would be the same, this Promise will be found
            // by a traverse, so we can just stop
            unify(s, ty, to_typewrapper(ty2))
        }
        Term::Assume(ty2, _, _) => unify(s, ty, to_typewrapper(ty2)),
    }
}

// TODO use RC to clone on write on the bound_vars
pub fn traverse(mut bound_vars: HashMap<Ident, Term>, t: Term) -> Result<(), String> {
    match t {
        Term::Bool(_) => Ok(()),
        Term::Num(_) => Ok(()),
        Term::Fun(_, rt) => {
            let t = rt.into();
            // This means the lambda binds are not automatically dereferenced
            // for type checking, this can be pretty bad
            traverse(bound_vars, t)
        }
        Term::Lbl(_) => Ok(()),
        Term::Let(x, re, rt) => {
            let e: Term = re.into();
            let t = rt.into();

            bound_vars.insert(x, e.clone());
            traverse(bound_vars.clone(), t)?;
            traverse(bound_vars, e)
        }
        Term::App(re, rt) => {
            let e = re.into();
            let t = rt.into();

            traverse(bound_vars.clone(), t)?;
            traverse(bound_vars, e)
        }
        Term::Var(x) => Ok(()),
        Term::Op1(_, rt) => {
            let t = rt.into();

            traverse(bound_vars, t)
        }
        Term::Op2(_, rt, re) => {
            let t = rt.into();
            let e = re.into();

            traverse(bound_vars.clone(), t)?;
            traverse(bound_vars, e)
        }
        Term::Promise(ty, lbl, rt) => {
            let t: Term = rt.into();
            let mut global_types = GTypes::new();

            typeCheck(
                &bound_vars,
                HashMap::new(),
                &mut global_types,
                t.clone(),
                to_typewrapper(ty),
            )?;
            println!(
                "Just checked a promise and it seemed to work, lbl: {:?}",
                lbl
            );
            traverse(bound_vars, t) // Maybe this is not needed
                                    // Or maybe it is, because typecheck stops at assumes
        }
        Term::Assume(_, _, rt) => {
            let t = rt.into();

            traverse(bound_vars, t)
        }
    }
}

fn to_typewrapper(t: Types) -> TypeWrapper {
    let Types(t2) = t;

    let t3 = t2.map(|x| Box::new(to_typewrapper(*x)));

    TypeWrapper::The(t3)
}

// #[cfg(test)]
// mod tests {
//     use super::*;
//     use label::{Label, TyPath};

//     fn go(t: Term) {
//         println!("{:?}", traverse(HashMap::new(), t));
//     }

//     #[test]
//     fn simplest() {
//         println!("{:?}", traverse(HashMap::new(), Term::Bool(false)));
//         println!("{:?}", traverse(HashMap::new(), Term::Num(34.)));

//         let lbl = Label {
//             tag: "Mine".into(),
//             l: 0,
//             r: 1000,
//             polarity: true,
//             path: TyPath::Nil(),
//         };

//         go(Term::Promise(
//             Types(AbsType::bool()),
//             lbl.clone(),
//             Term::Num(45.4).into(),
//         ));

//         panic!("Show me what you got");
//     }
// }
