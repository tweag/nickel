use identifier::Ident;
use std::collections::HashMap;
use term::{BinaryOp, Term, UnaryOp};
use types::{AbsType, Types};

// Type checking
pub fn type_check(t: &Term) -> Result<(), String> {
    let mut s = GTypes::new();
    let ty = TypeWrapper::Ptr(new_var(&mut s));
    type_check_(HashMap::new(), &mut s, t, ty, false)
}

fn type_check_(
    mut typed_vars: HashMap<Ident, TypeWrapper>,
    s: &mut GTypes,
    t: &Term,
    ty: TypeWrapper,
    strict: bool,
) -> Result<(), String> {
    match t {
        Term::Bool(_) => unify(s, ty, TypeWrapper::Concrete(AbsType::Bool()), strict),
        Term::Num(_) => unify(s, ty, TypeWrapper::Concrete(AbsType::Num()), strict),
        Term::Fun(x, rt) => {
            let src = TypeWrapper::Ptr(new_var(s));
            // TODO what to do here, this makes more sense to me, but it means let x = foo in bar
            // behaves quite different to (\x.bar) foo, worth considering if it's ok to type these two differently
            // let src = TypeWrapper::The(AbsType::Dyn());
            let trg = TypeWrapper::Ptr(new_var(s));
            let arr =
                TypeWrapper::Concrete(AbsType::arrow(Box::new(src.clone()), Box::new(trg.clone())));

            unify(s, ty, arr, strict)?;

            typed_vars.insert(x.clone(), src);
            type_check_(typed_vars, s, rt.as_ref(), trg, strict)
        }
        Term::Lbl(_) => {
            // TODO implement lbl type
            unify(s, ty, TypeWrapper::Concrete(AbsType::Dyn()), strict)
        }
        Term::Let(x, re, rt) => {
            let e = re.as_ref();

            // If the right hand side has a Promise or Assume, we use it as a
            // type annotation otherwise, x gets type Dyn
            let exp = match e {
                Term::Assume(ty, _, _) | Term::Promise(ty, _, _) => to_typewrapper(ty.clone()),
                _ => TypeWrapper::Concrete(AbsType::Dyn()),
            };

            let instantiated = instantiate_foralls_with(s, exp.clone(), TypeWrapper::Constant)?;
            type_check_(typed_vars.clone(), s, e, instantiated, strict)?;

            // TODO move this up once lets are rec
            typed_vars.insert(x.clone(), exp);
            type_check_(typed_vars, s, rt.as_ref(), ty, strict)
        }
        Term::App(re, rt) => {
            let src = TypeWrapper::Ptr(new_var(s));
            let arr = TypeWrapper::Concrete(AbsType::arrow(Box::new(src.clone()), Box::new(ty)));

            type_check_(typed_vars.clone(), s, re.as_ref(), arr, strict)?;
            type_check_(typed_vars, s, rt.as_ref(), src, strict)
        }
        Term::Var(x) => {
            let x_ty = typed_vars
                .get(&x)
                .ok_or(format!("Found an unbound var {:?}", x))?;

            let instantiated = instantiate_foralls_with(s, x_ty.clone(), TypeWrapper::Ptr)?;
            unify(s, ty, instantiated, strict)
        }
        Term::Op1(op, rt) => {
            let ty_op = get_uop_type(s, op);

            let src = TypeWrapper::Ptr(new_var(s));
            let arr = TypeWrapper::Concrete(AbsType::arrow(Box::new(src.clone()), Box::new(ty)));

            unify(s, arr, ty_op, strict)?;
            type_check_(typed_vars, s, rt.as_ref(), src, strict)
        }
        Term::Op2(op, re, rt) => {
            let ty_op = get_bop_type(s, op);

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
            type_check_(typed_vars.clone(), s, re.as_ref(), src1, strict)?;
            type_check_(typed_vars, s, rt.as_ref(), src2, strict)
        }
        Term::Promise(ty2, _, rt) => {
            let tyw2 = to_typewrapper(ty2.clone());

            let instantiated = instantiate_foralls_with(s, tyw2, TypeWrapper::Constant)?;

            unify(s, ty.clone(), to_typewrapper(ty2.clone()), strict)?;
            type_check_(typed_vars, s, rt.as_ref(), instantiated, true)
        }
        Term::Assume(ty2, _, rt) => {
            unify(s, ty.clone(), to_typewrapper(ty2.clone()), strict)?;
            let new_ty = TypeWrapper::Ptr(new_var(s));
            type_check_(typed_vars, s, rt.as_ref(), new_ty, false)
        }
        Term::Sym(_) => unify(s, ty, TypeWrapper::Concrete(AbsType::Sym()), strict),
        Term::Wrapped(_, rt) => type_check_(typed_vars, s, rt.as_ref(), ty, strict),
    }
}

// TypeWrapper
//   A type can be a concrete type, or a type variable

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
            Concrete(AbsType::Sym()) => Concrete(AbsType::Sym()),
            Concrete(AbsType::Flat(t)) => Concrete(AbsType::Flat(t)),
            Concrete(AbsType::Arrow(s, t)) => {
                let fs = s.subst(id.clone(), to.clone());
                let ft = t.subst(id, to);

                Concrete(AbsType::Arrow(Box::new(fs), Box::new(ft)))
            }
            Constant(x) => Constant(x),
            Ptr(x) => Ptr(x),
        }
    }
}

pub fn unify(
    state: &mut GTypes,
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
            (AbsType::Sym(), AbsType::Sym()) => Ok(()),
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
            (AbsType::Arrow(s1s, s1t), AbsType::Arrow(s2s, s2t)) => {
                unify(state, *s1s, *s2s, strict)?;
                unify(state, *s1t, *s2t, strict)
            }
            (AbsType::Var(ref i1), AbsType::Var(ref i2)) if i1 == i2 => Ok(()),
            (AbsType::Forall(i1, t1t), AbsType::Forall(i2, t2t)) => {
                // Very stupid (slow) implementation
                let constant_type = TypeWrapper::Constant(new_var(state));

                unify(
                    state,
                    t1t.subst(i1, constant_type.clone()),
                    t2t.subst(i2, constant_type),
                    strict,
                )
            }
            (a, b) => Err(format!("The following types dont match {:?} -- {:?}", a, b)),
        },
        (TypeWrapper::Ptr(r1), TypeWrapper::Ptr(r2)) => {
            if r1 != r2 {
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

fn instantiate_foralls_with<F>(
    s: &mut GTypes,
    mut ty: TypeWrapper,
    f: F,
) -> Result<TypeWrapper, String>
where
    F: Fn(usize) -> TypeWrapper,
{
    if let TypeWrapper::Ptr(p) = ty {
        ty = get_root(s, p)?;
    }

    while let TypeWrapper::Concrete(AbsType::Forall(id, forall_ty)) = ty {
        let var = f(new_var(s));
        ty = forall_ty.subst(id, var);
    }
    Ok(ty)
}

pub fn get_uop_type(s: &mut GTypes, op: &UnaryOp) -> TypeWrapper {
    match op {
        UnaryOp::Ite() => {
            let branches = TypeWrapper::Ptr(new_var(s));

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
        UnaryOp::IsNum() | UnaryOp::IsBool() | UnaryOp::IsFun() => {
            let inp = TypeWrapper::Ptr(new_var(s));

            TypeWrapper::Concrete(AbsType::arrow(
                Box::new(inp),
                Box::new(TypeWrapper::Concrete(AbsType::Bool())),
            ))
        }
        UnaryOp::Blame() => {
            let res = TypeWrapper::Ptr(new_var(s));

            TypeWrapper::Concrete(AbsType::arrow(
                Box::new(TypeWrapper::Concrete(AbsType::Dyn())),
                Box::new(res),
            ))
        }
        UnaryOp::Pol() => TypeWrapper::Concrete(AbsType::arrow(
            Box::new(TypeWrapper::Concrete(AbsType::Dyn())),
            Box::new(TypeWrapper::Concrete(AbsType::Bool())),
        )),
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
    }
}

pub fn get_bop_type(_s: &mut GTypes, op: &BinaryOp) -> TypeWrapper {
    match op {
        BinaryOp::Plus() => TypeWrapper::Concrete(AbsType::arrow(
            Box::new(TypeWrapper::Concrete(AbsType::Num())),
            Box::new(TypeWrapper::Concrete(AbsType::arrow(
                Box::new(TypeWrapper::Concrete(AbsType::Num())),
                Box::new(TypeWrapper::Concrete(AbsType::Num())),
            ))),
        )),
        BinaryOp::Unwrap() => TypeWrapper::Concrete(AbsType::arrow(
            Box::new(TypeWrapper::Concrete(AbsType::Sym())),
            Box::new(TypeWrapper::Concrete(AbsType::arrow(
                Box::new(TypeWrapper::Concrete(AbsType::Dyn())),
                Box::new(TypeWrapper::Concrete(AbsType::arrow(
                    Box::new(TypeWrapper::Concrete(AbsType::Dyn())),
                    Box::new(TypeWrapper::Concrete(AbsType::Dyn())),
                ))),
            ))),
        )),
        BinaryOp::EqBool() => TypeWrapper::Concrete(AbsType::arrow(
            Box::new(TypeWrapper::Concrete(AbsType::Bool())),
            Box::new(TypeWrapper::Concrete(AbsType::arrow(
                Box::new(TypeWrapper::Concrete(AbsType::Bool())),
                Box::new(TypeWrapper::Concrete(AbsType::Bool())),
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
pub fn get_root(s: &GTypes, x: usize) -> Result<TypeWrapper, String> {
    match s.get(&x).ok_or(format!("Unbound type variable {}!", x))? {
        None => Ok(TypeWrapper::Ptr(x)),
        Some(TypeWrapper::Ptr(y)) => get_root(s, *y),
        Some(ty @ TypeWrapper::Concrete(_)) => Ok(ty.clone()),
        Some(k @ TypeWrapper::Constant(_)) => Ok(k.clone()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use label::{Label, TyPath};
    use term::RichTerm;

    use parser;

    fn parse_and_typecheck(s: &str) -> Result<(), String> {
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
}
