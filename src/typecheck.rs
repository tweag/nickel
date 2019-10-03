use identifier::Ident;
use std::collections::HashMap;
use term::{BinaryOp, Term, UnaryOp};
use types::{AbsType, Types};

// Type checking
pub fn type_check(t: Term) -> Result<(), String> {
    let mut s = GTypes::new();
    let ty = TypeWrapper::Ptr(new_var(&mut s));
    type_check_(HashMap::new(), &mut s, t, ty, false)
}

fn type_check_(
    mut typed_vars: HashMap<Ident, TypeWrapper>,
    s: &mut GTypes,
    t: Term,
    ty: TypeWrapper,
    strict: bool,
) -> Result<(), String> {
    match t {
        Term::Bool(_) => unify(s, ty, TypeWrapper::Concrete(AbsType::bool()), strict),
        Term::Num(_) => unify(s, ty, TypeWrapper::Concrete(AbsType::num()), strict),
        Term::Fun(x, rt) => {
            let t = rt.into();
            let src = TypeWrapper::Ptr(new_var(s));
            // TODO what to do here, this makes more sense to me, but it means let x = foo in bar
            // behaves quite different to (\x.bar) foo, worth considering if it's ok to type these two differently
            // let src = TypeWrapper::The(AbsType::dyn());
            let trg = TypeWrapper::Ptr(new_var(s));
            let arr =
                TypeWrapper::Concrete(AbsType::arrow(Box::new(src.clone()), Box::new(trg.clone())));

            unify(s, ty, arr, strict)?;

            typed_vars.insert(x, src);
            type_check_(typed_vars, s, t, trg, strict)
        }
        Term::Lbl(_) => {
            // TODO implement lbl type
            unify(s, ty, TypeWrapper::Concrete(AbsType::dyn()), strict)
        }
        Term::Let(x, re, rt) => {
            let e = re.into();
            let t = rt.into();

            // If the right hand side has a Promise or Assume, we use it as a
            // type annotation otherwise, x gets type Dyn
            let exp = match &e {
                Term::Assume(ty, _, _) | Term::Promise(ty, _, _) => to_typewrapper(ty.clone()),
                _ => TypeWrapper::Concrete(AbsType::dyn()),
            };

            type_check_(typed_vars.clone(), s, e, exp.clone(), strict)?;
            // TODO move this up once lets are rec
            typed_vars.insert(x, exp);
            type_check_(typed_vars, s, t, ty, strict)
        }
        Term::App(re, rt) => {
            let e = re.into();
            let t = rt.into();

            let src = TypeWrapper::Ptr(new_var(s));
            let arr = TypeWrapper::Concrete(AbsType::arrow(Box::new(src.clone()), Box::new(ty)));

            type_check_(typed_vars.clone(), s, e, arr, strict)?;
            type_check_(typed_vars, s, t, src, strict)
        }
        Term::Var(x) => {
            let x_ty = typed_vars
                .get(&x)
                .ok_or(format!("Found an unbound var {:?}", x))?;
            unify(s, ty, x_ty.clone(), strict)
        }
        Term::Op1(op, rt) => {
            let ty_op = get_uop_type(s, op);
            let t = rt.into();

            let src = TypeWrapper::Ptr(new_var(s));
            let arr = TypeWrapper::Concrete(AbsType::arrow(Box::new(src.clone()), Box::new(ty)));

            unify(s, arr, ty_op, strict)?;
            type_check_(typed_vars, s, t, src, strict)
        }
        Term::Op2(op, re, rt) => {
            let ty_op = get_bop_type(s, op);
            let t = rt.into();
            let e = re.into();

            let src1 = TypeWrapper::Ptr(new_var(s));
            let src2 = TypeWrapper::Ptr(new_var(s));
            let arr = TypeWrapper::Concrete(AbsType::arrow(
                Box::new(src1.clone()),
                Box::new(TypeWrapper::Concrete(AbsType::arrow(
                    Box::new(src2.clone()),
                    Box::new(ty),
                ))),
            ));

            unify(s, arr, ty_op, strict)?;
            type_check_(typed_vars.clone(), s, e, src1, strict)?;
            type_check_(typed_vars, s, t, src2, strict)
        }
        Term::Promise(ty2, _, rt) => {
            unify(s, ty.clone(), to_typewrapper(ty2.clone()), strict)?;
            type_check_(typed_vars, s, rt.into(), to_typewrapper(ty2.clone()), true)
        }
        Term::Assume(ty2, _, rt) => {
            unify(s, ty.clone(), to_typewrapper(ty2), strict)?;
            let new_ty = TypeWrapper::Ptr(new_var(s));
            type_check_(typed_vars, s, rt.into(), new_ty, false)
        }
    }
}

// TypeWrapper
//   A type can be a concrete type, or a type variable

#[derive(Clone, PartialEq, Debug)]
pub enum TypeWrapper {
    Concrete(AbsType<Box<TypeWrapper>>),
    Ptr(usize),
}

pub fn unify(
    state: &mut GTypes,
    t1: TypeWrapper,
    t2: TypeWrapper,
    strict: bool,
) -> Result<(), String> {
    if !strict {
        // TODO think whether this makes sense, without this we can't write the Y combinator
        return Ok(());
    }
    match (t1, t2) {
        (TypeWrapper::Concrete(s1), TypeWrapper::Concrete(s2)) => match (s1, s2) {
            (AbsType::Dyn(), AbsType::Dyn()) => Ok(()),
            (AbsType::Num(), AbsType::Num()) => Ok(()),
            (AbsType::Bool(), AbsType::Bool()) => Ok(()),
            (AbsType::Arrow(s1s, s1t), AbsType::Arrow(s2s, s2t)) => {
                unify(state, *s1s, *s2s, strict)?;
                unify(state, *s1t, *s2t, strict)
            }
            (AbsType::Flat(s), AbsType::Flat(t)) => {
                if let Term::Var(s) = s.clone().into() {
                    if let Term::Var(t) = t.clone().into() {
                        if s == t {
                            return Ok(());
                        }
                    }
                }
                if strict {
                    Err(format!("Two expressions didn't match {:?} - {:?}", s, t))
                } else {
                    Ok(())
                }
            } // Right now it only unifies equally named variables
            (a, b) => {
                if strict {
                    Err(format!("The following types dont match {:?} -- {:?}", a, b))
                } else {
                    println!(
                        "Found two not matching types on non strict mode: {:?} -- {:?}",
                        a, b
                    );
                    Ok(())
                }
            }
        },
        (TypeWrapper::Ptr(p1), TypeWrapper::Ptr(p2)) => {
            let (ty1, r1) = get_root(&state, p1)?;
            let (ty2, r2) = get_root(&state, p2)?;

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
                (Some(ty1), Some(ty2)) => unify(
                    state,
                    TypeWrapper::Concrete(ty1),
                    TypeWrapper::Concrete(ty2),
                    strict,
                ),
            }
        }
        (TypeWrapper::Ptr(p), TypeWrapper::Concrete(s))
        | (TypeWrapper::Concrete(s), TypeWrapper::Ptr(p)) => {
            let (p_ty_opt, root) = get_root(&state, p)?;

            if let Some(p_ty) = p_ty_opt {
                unify(
                    state,
                    TypeWrapper::Concrete(p_ty),
                    TypeWrapper::Concrete(s),
                    strict,
                )
            } else {
                state.insert(root, Some(TypeWrapper::Concrete(s)));
                Ok(())
            }
        }
    }
}

fn to_typewrapper(t: Types) -> TypeWrapper {
    let Types(t2) = t;

    let t3 = t2.map(|x| Box::new(to_typewrapper(*x)));

    TypeWrapper::Concrete(t3)
}

pub fn get_uop_type(s: &mut GTypes, op: UnaryOp) -> TypeWrapper {
    match op {
        UnaryOp::Ite() => {
            let branches = TypeWrapper::Ptr(new_var(s));

            TypeWrapper::Concrete(AbsType::arrow(
                Box::new(TypeWrapper::Concrete(AbsType::bool())),
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
            Box::new(TypeWrapper::Concrete(AbsType::num())),
            Box::new(TypeWrapper::Concrete(AbsType::bool())),
        )),
        UnaryOp::IsNum() | UnaryOp::IsBool() | UnaryOp::IsFun() => {
            let inp = TypeWrapper::Ptr(new_var(s));

            TypeWrapper::Concrete(AbsType::arrow(
                Box::new(inp),
                Box::new(TypeWrapper::Concrete(AbsType::bool())),
            ))
        }
        UnaryOp::Blame() => {
            let res = TypeWrapper::Ptr(new_var(s));

            TypeWrapper::Concrete(AbsType::arrow(
                Box::new(TypeWrapper::Concrete(AbsType::dyn())),
                Box::new(res),
            ))
        }
        UnaryOp::ChangePolarity() | UnaryOp::GoDom() | UnaryOp::GoCodom() | UnaryOp::Tag(_) => {
            TypeWrapper::Concrete(AbsType::arrow(
                Box::new(TypeWrapper::Concrete(AbsType::dyn())),
                Box::new(TypeWrapper::Concrete(AbsType::dyn())),
            ))
        }
    }
}

pub fn get_bop_type(_s: &mut GTypes, op: BinaryOp) -> TypeWrapper {
    match op {
        BinaryOp::Plus() => TypeWrapper::Concrete(AbsType::arrow(
            Box::new(TypeWrapper::Concrete(AbsType::num())),
            Box::new(TypeWrapper::Concrete(AbsType::arrow(
                Box::new(TypeWrapper::Concrete(AbsType::num())),
                Box::new(TypeWrapper::Concrete(AbsType::num())),
            ))),
        )),
    }
}

// Global types, where unification happens

pub type GTypes = HashMap<usize, Option<TypeWrapper>>;

fn new_var(state: &mut GTypes) -> usize {
    let nxt = state.len();
    state.insert(nxt, None);
    nxt
}

// TODO This should be a union find like algorithm
pub fn get_root(
    s: &GTypes,
    x: usize,
) -> Result<(Option<AbsType<Box<TypeWrapper>>>, usize), String> {
    match s.get(&x).ok_or(format!("Unbound type variable {}!", x))? {
        None => Ok((None, x)),
        Some(TypeWrapper::Ptr(y)) => get_root(s, *y),
        Some(TypeWrapper::Concrete(ty)) => Ok((Some(ty.clone()), x)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use label::{Label, TyPath};
    use term::RichTerm;

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

        type_check(Term::Bool(true))?;
        type_check(Term::Num(45.))?;

        type_check(Term::Fun(Ident("x".into()), RichTerm::var("x".into())))?;
        type_check(Term::Let(
            Ident("x".into()),
            Term::Num(3.).into(),
            RichTerm::var("x".into()),
        ))?;

        type_check(Term::App(Term::Num(5.).into(), Term::Bool(true).into()))?;
        type_check(RichTerm::plus(Term::Num(4.).into(), Term::Bool(false).into()).into())?;

        Ok(())
    }

    #[test]
    fn unbound_variable_always_throws() {
        type_check(Term::Var(Ident("x".into()))).unwrap_err();
    }

    #[test]
    fn promise_simple_checks() {
        type_check(Term::Promise(
            Types(AbsType::bool()),
            label(),
            Term::Bool(true).into(),
        ))
        .unwrap();
        type_check(Term::Promise(
            Types(AbsType::num()),
            label(),
            Term::Bool(true).into(),
        ))
        .unwrap_err();

        type_check(Term::Promise(
            Types(AbsType::num()),
            label(),
            Term::Num(34.5).into(),
        ))
        .unwrap();
        type_check(Term::Promise(
            Types(AbsType::bool()),
            label(),
            Term::Num(34.5).into(),
        ))
        .unwrap_err();

        type_check(Term::Promise(
            Types(AbsType::num()),
            label(),
            Term::Assume(Types(AbsType::num()), label(), Term::Bool(true).into()).into(),
        ))
        .unwrap();
        type_check(Term::Promise(
            Types(AbsType::num()),
            label(),
            Term::Assume(Types(AbsType::bool()), label(), Term::Num(34.).into()).into(),
        ))
        .unwrap_err();
    }

    #[test]
    fn promise_complicated() {
        use parser;

        fn parse_and_typecheck(s: &str) -> Result<(), String> {
            if let Ok(p) = parser::grammar::TermParser::new().parse(s) {
                type_check(p.into())
            } else {
                panic!("Couldn't parse")
            }
        }

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
}
