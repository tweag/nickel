use crate::identifier::Ident;
use crate::term::{BinaryOp, Term, UnaryOp};
use crate::types::{AbsType, Types};
use std::collections::{HashMap, HashSet};

// Type checking
pub fn type_check(t: &Term) -> Result<Types, String> {
    let mut s = GTypes::new();
    let mut c = GConstr::new();
    let ty = TypeWrapper::Ptr(new_var(&mut s));
    type_check_(HashMap::new(), &mut s, &mut c, t, ty.clone(), false)?;

    Ok(to_type(&s, ty)?)
}

fn type_check_(
    mut typed_vars: HashMap<Ident, TypeWrapper>,
    s: &mut GTypes,
    c: &mut GConstr,
    t: &Term,
    ty: TypeWrapper,
    strict: bool,
) -> Result<(), String> {
    match t {
        Term::Bool(_) => unify(s, c, ty, TypeWrapper::Concrete(AbsType::Bool()), strict),
        Term::Num(_) => unify(s, c, ty, TypeWrapper::Concrete(AbsType::Num()), strict),
        Term::Str(_) => unify(s, c, ty, TypeWrapper::Concrete(AbsType::Str()), strict),
        Term::Fun(x, rt) => {
            let src = TypeWrapper::Ptr(new_var(s));
            // TODO what to do here, this makes more sense to me, but it means let x = foo in bar
            // behaves quite different to (\x.bar) foo, worth considering if it's ok to type these two differently
            // let src = TypeWrapper::The(AbsType::Dyn());
            let trg = TypeWrapper::Ptr(new_var(s));
            let arr =
                TypeWrapper::Concrete(AbsType::arrow(Box::new(src.clone()), Box::new(trg.clone())));

            unify(s, c, ty, arr, strict)?;

            typed_vars.insert(x.clone(), src);
            type_check_(typed_vars, s, c, rt.as_ref(), trg, strict)
        }
        Term::Lbl(_) => {
            // TODO implement lbl type
            unify(s, c, ty, TypeWrapper::Concrete(AbsType::Dyn()), strict)
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
            type_check_(typed_vars.clone(), s, c, e, instantiated, strict)?;

            // TODO move this up once lets are rec
            typed_vars.insert(x.clone(), exp);
            type_check_(typed_vars, s, c, rt.as_ref(), ty, strict)
        }
        Term::App(re, rt) => {
            let src = TypeWrapper::Ptr(new_var(s));
            let arr = TypeWrapper::Concrete(AbsType::arrow(Box::new(src.clone()), Box::new(ty)));

            type_check_(typed_vars.clone(), s, c, re.as_ref(), arr, strict)?;
            type_check_(typed_vars, s, c, rt.as_ref(), src, strict)
        }
        Term::Var(x) => {
            let x_ty = typed_vars
                .get(&x)
                .ok_or_else(|| format!("Found an unbound var {:?}", x))?;

            let instantiated = instantiate_foralls_with(s, x_ty.clone(), TypeWrapper::Ptr)?;
            unify(s, c, ty, instantiated, strict)
        }
        Term::Enum(id) => {
            let row = TypeWrapper::Ptr(new_var(s));
            // Do we really need to constraint on enums?
            // What's the meaning of this?
            constraint(s, c, row.clone(), id.clone())?;
            unify(
                s,
                c,
                ty,
                TypeWrapper::Concrete(AbsType::Enum(Box::new(TypeWrapper::Concrete(
                    AbsType::RowExtend(id.clone(), Box::new(row)),
                )))),
                strict,
            )
        }
        Term::Op1(op, rt) => {
            let ty_op = get_uop_type(typed_vars.clone(), s, c, op, strict)?;

            let src = TypeWrapper::Ptr(new_var(s));
            let arr = TypeWrapper::Concrete(AbsType::arrow(Box::new(src.clone()), Box::new(ty)));

            unify(s, c, arr, ty_op, strict)?;
            type_check_(typed_vars, s, c, rt.as_ref(), src, strict)
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

            unify(s, c, arr, ty_op, strict)?;
            type_check_(typed_vars.clone(), s, c, re.as_ref(), src1, strict)?;
            type_check_(typed_vars, s, c, rt.as_ref(), src2, strict)
        }
        Term::Promise(ty2, _, rt) => {
            let tyw2 = to_typewrapper(ty2.clone());

            let instantiated = instantiate_foralls_with(s, tyw2, TypeWrapper::Constant)?;

            unify(s, c, ty.clone(), to_typewrapper(ty2.clone()), strict)?;
            type_check_(typed_vars, s, c, rt.as_ref(), instantiated, true)
        }
        Term::Assume(ty2, _, rt) => {
            unify(s, c, ty.clone(), to_typewrapper(ty2.clone()), strict)?;
            let new_ty = TypeWrapper::Ptr(new_var(s));
            type_check_(typed_vars, s, c, rt.as_ref(), new_ty, false)
        }
        Term::Sym(_) => unify(s, c, ty, TypeWrapper::Concrete(AbsType::Sym()), strict),
        Term::Wrapped(_, rt) => type_check_(typed_vars, s, c, rt.as_ref(), ty, strict),
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
            Concrete(AbsType::RowExtend(tag, rest)) => {
                Concrete(AbsType::RowExtend(tag, Box::new(rest.subst(id, to))))
            }
            Concrete(AbsType::Enum(row)) => Concrete(AbsType::Enum(Box::new(row.subst(id, to)))),

            Constant(x) => Constant(x),
            Ptr(x) => Ptr(x),
        }
    }
}

/// Add an identifier to a row.
///
/// If the id is not there, and the row is open, it will add it.
///
/// # Returns
///
/// The row without the added id.
fn row_add(
    state: &mut GTypes,
    c: &mut GConstr,
    id: Ident,
    mut r: TypeWrapper,
) -> Result<TypeWrapper, String> {
    if let TypeWrapper::Ptr(p) = r {
        r = get_root(state, p)?;
    }
    match r {
        TypeWrapper::Concrete(AbsType::RowEmpty()) => Err("The row didn't have the id".to_string()),
        TypeWrapper::Concrete(AbsType::RowExtend(id2, r2)) => {
            if id == id2 {
                Ok(*r2)
            } else {
                let subrow = row_add(state, c, id, *r2)?;
                Ok(TypeWrapper::Concrete(AbsType::RowExtend(
                    id2,
                    Box::new(subrow),
                )))
            }
        }
        TypeWrapper::Concrete(not_row) => Err(format!("Expected a row, got {:?}", not_row)),
        TypeWrapper::Ptr(root) => {
            if let Some(set) = c.get(&root) {
                if set.contains(&id) {
                    return Err(format!(
                        "Tried to add {:?} to the row, but it was constrained.",
                        id
                    ));
                }
            }
            let new_row = TypeWrapper::Ptr(new_var(state));
            constraint(state, c, new_row.clone(), id.clone())?;
            state.insert(
                root,
                Some(TypeWrapper::Concrete(AbsType::RowExtend(
                    id,
                    Box::new(new_row.clone()),
                ))),
            );
            Ok(new_row)
        }
        TypeWrapper::Constant(_) => Err("Expected a row, got a constant".to_string()),
    }
}

pub fn unify(
    state: &mut GTypes,
    c: &mut GConstr,
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
                unify(state, c, *s1s, *s2s, strict)?;
                unify(state, c, *s1t, *s2t, strict)
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
            (AbsType::RowEmpty(), AbsType::RowEmpty()) => Ok(()),
            (AbsType::RowExtend(id, t), r2 @ AbsType::RowExtend(_, _)) => {
                let r2 = row_add(state, c, id, TypeWrapper::Concrete(r2))?;
                unify(state, c, *t, r2, strict)
            }
            (AbsType::Enum(r), AbsType::Enum(r2)) => unify(state, c, *r, *r2, strict),
            (AbsType::Var(ref i1), AbsType::Var(ref i2)) if i1 == i2 => Ok(()),
            (AbsType::Forall(i1, t1t), AbsType::Forall(i2, t2t)) => {
                // Very stupid (slow) implementation
                let constant_type = TypeWrapper::Constant(new_var(state));

                unify(
                    state,
                    c,
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

fn to_type(s: &GTypes, ty: TypeWrapper) -> Result<Types, String> {
    Ok(match ty {
        TypeWrapper::Ptr(p) => match get_root(s, p)? {
            t @ TypeWrapper::Concrete(_) => to_type(s, t)?,
            _ => Types(AbsType::Dyn()),
        },
        TypeWrapper::Constant(_) => Types(AbsType::Dyn()),
        TypeWrapper::Concrete(t) => {
            let mapped = t.map(|btyp| Box::new(to_type(s, *btyp).unwrap()));
            Types(mapped)
        }
    })
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

pub fn get_uop_type(
    typed_vars: HashMap<Ident, TypeWrapper>,
    s: &mut GTypes,
    c: &mut GConstr,
    op: &UnaryOp,
    strict: bool,
) -> Result<TypeWrapper, String> {
    Ok(match op {
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
        UnaryOp::IsNum() | UnaryOp::IsBool() | UnaryOp::IsStr() | UnaryOp::IsFun() => {
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

        UnaryOp::Embed(id) => {
            let row = TypeWrapper::Ptr(new_var(s));
            constraint(s, c, row.clone(), id.clone())?;

            TypeWrapper::Concrete(AbsType::Arrow(
                Box::new(TypeWrapper::Concrete(AbsType::Enum(Box::new(row.clone())))),
                Box::new(TypeWrapper::Concrete(AbsType::Enum(Box::new(
                    TypeWrapper::Concrete(AbsType::RowExtend(id.clone(), Box::new(row))),
                )))),
            ))
        }
        UnaryOp::Switch(l, d) => {
            // Currently, if it has a default value, we typecheck the whole thing as
            // taking ANY enum, since it's more permissive and there's not a loss of information
            let res = TypeWrapper::Ptr(new_var(s));

            for exp in l.values() {
                type_check_(typed_vars.clone(), s, c, exp.as_ref(), res.clone(), strict)?;
            }

            let row = match d {
                Some(e) => {
                    type_check_(typed_vars.clone(), s, c, e.as_ref(), res.clone(), strict)?;
                    TypeWrapper::Ptr(new_var(s))
                }
                None => l.iter().try_fold(
                    TypeWrapper::Concrete(AbsType::RowEmpty()),
                    |acc, x| -> Result<TypeWrapper, String> {
                        constraint(s, c, acc.clone(), x.0.clone())?;
                        Ok(TypeWrapper::Concrete(AbsType::RowExtend(
                            x.0.clone(),
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
    })
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
        BinaryOp::PlusStr() => TypeWrapper::Concrete(AbsType::arrow(
            Box::new(TypeWrapper::Concrete(AbsType::Str())),
            Box::new(TypeWrapper::Concrete(AbsType::arrow(
                Box::new(TypeWrapper::Concrete(AbsType::Str())),
                Box::new(TypeWrapper::Concrete(AbsType::Str())),
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
pub type GConstr = HashMap<usize, HashSet<Ident>>;

fn new_var(state: &mut GTypes) -> usize {
    let nxt = state.len();
    state.insert(nxt, None);
    nxt
}

fn constraint(s: &mut GTypes, cons: &mut GConstr, x: TypeWrapper, id: Ident) -> Result<(), String> {
    match x {
        TypeWrapper::Ptr(p) => {
            let ty = get_root(s, p)?;
            match ty {
                ty @ TypeWrapper::Concrete(_) => constraint(s, cons, ty, id),
                TypeWrapper::Ptr(root) => {
                    if let Some(v) = cons.get_mut(&root) {
                        v.insert(id);
                    } else {
                        cons.insert(root, vec![id].into_iter().collect());
                    }
                    Ok(())
                }
                TypeWrapper::Constant(_) => Err("Can't constraint a constant".to_string()),
            }
        }
        TypeWrapper::Concrete(AbsType::RowEmpty()) => Ok(()),
        TypeWrapper::Concrete(AbsType::RowExtend(id2, t)) => {
            if id2 == id {
                Err(format!("The id {:?} was present on the row", id))
            } else {
                constraint(s, cons, *t, id)
            }
        }
        _ => Err(format!("Can't constraint a {:?}", x)),
    }
}

// TODO This should be a union find like algorithm
pub fn get_root(s: &GTypes, x: usize) -> Result<TypeWrapper, String> {
    match s
        .get(&x)
        .ok_or_else(|| format!("Unbound type variable {}!", x))?
    {
        None => Ok(TypeWrapper::Ptr(x)),
        Some(TypeWrapper::Ptr(y)) => get_root(s, *y),
        Some(ty @ TypeWrapper::Concrete(_)) => Ok(ty.clone()),
        Some(k @ TypeWrapper::Constant(_)) => Ok(k.clone()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::label::{Label, TyPath};
    use crate::term::RichTerm;

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
}
