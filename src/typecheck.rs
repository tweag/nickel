use std::collections::HashMap;
use term::Term;
use types::{AbsType, Types};

#[derive(Clone, PartialEq, Debug)]
pub enum TypeWrapper {
    The(AbsType<Box<TypeWrapper>>),
    Ptr(usize),
}

pub type GTypes = HashMap<usize, Option<TypeWrapper>>;

pub fn getRoot(mut s: GTypes, x: usize) -> Option<AbsType<Box<TypeWrapper>>> {
    match s.get(&x).expect("Unbound type variable!") {
        None => None,
        Some(TypeWrapper::Ptr(y)) => getRoot(s.clone(), *y),
        Some(TypeWrapper::The(ty)) => Some(ty.clone()),
    }
}

pub fn unify(mut s: GTypes, t1: TypeWrapper, t2: TypeWrapper) -> bool {
    match (t1, t2) {
        (TypeWrapper::The(s1), TypeWrapper::The(s2)) => match (s1, s2) {
            (AbsType::Dyn(), AbsType::Dyn()) => true,
            (AbsType::Num(), AbsType::Num()) => true,
            (AbsType::Bool(), AbsType::Bool()) => true,
            (AbsType::Arrow(s1s, s1t), AbsType::Arrow(s2s, s2t)) => {
                let srcs = unify(s.clone(), *s1s, *s2s);
                let trgs = unify(s.clone(), *s1t, *s2t);

                srcs && trgs
            }
            (_, _) => false,
        },
        (TypeWrapper::Ptr(p1), TypeWrapper::Ptr(p2)) => false,
        (TypeWrapper::Ptr(p), TypeWrapper::The(s)) | (TypeWrapper::The(s), TypeWrapper::Ptr(p)) => {
            false
        }
    }
}

pub fn typeCheck(mut s: GTypes, t: Term, ty: TypeWrapper) -> Result<(), String> {
    Ok(())
}
