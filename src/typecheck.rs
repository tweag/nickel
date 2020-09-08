//! Implementation of the typechecker.
//!
//! # Mode
//!
//! Typechecking can be made in to different modes:
//! - **Strict**: correspond to traditional typechecking in strongly, statically typed languages.
//! This happens inside a `Promise` block.
//! - **Non strict**: do not enforce any typing, but still store the annotations of let bindings in
//! the environment, and continue to traverse the AST looking for other `Promise` blocks to
//! typecheck.
//!
//! The algorithm starts in non strict mode. It is switched to strict mode when entering a
//! `Promise` block, and is switched to non-strict mode when entering an `Assume` block.  `Promise`
//! and `Assume` thus serve both two purposes: annotate a term with a type, and set the
//! typechecking mode.
//!
//! # Type inference
//!
//! Type inference is done via a standard unification algorithm. The type of unannotated let-bound
//! expressions (the type of `bound_exp` in `let x = bound_exp in body`) is inferred in strict
//! mode, but it is currently not generalized. For example, the following program is rejected:
//!
//! ```
//! // Rejected
//! Promise(Num, let id = fun x => x in seq (id "a") (id 5))
//! ```
//!
//! Indeed, `id` is given the type `_a -> _a`, where `_a` is a unification variable, but is not
//! generalized to `forall a. a -> a`. At the first call site, `_a` is unified with `Str`, and at the second
//! call site the typechecker complains that `5` is not of type `Str`. A polymorphic annotation simply solves the
//! problem:
//!
//! ```
//! // Accepted
//! Promise(Num, let id = Promise(forall a. a -> a, fun x => x) in seq (id "a") (id 5))
//! ```
//!
//! Note that this restriction is not motivated by soundness, as can be various limitations on
//! polymorphism in other languages (e.g the value restriction). It is rather that efficient
//! generalization is not trivial and is not currently implemented, but it could be very well be
//! the case in the future.
//!
//! In non-strict mode, all let-bound expressions are given type `Dyn`, unless annotated.
use crate::error::TypecheckError;
use crate::identifier::Ident;
use crate::position::RawSpan;
use crate::program::ImportResolver;
use crate::term::{BinaryOp, RichTerm, StrChunk, Term, UnaryOp};
use crate::types::{AbsType, Types};
use std::collections::{HashMap, HashSet};

/// Error during the unification of two row types.
#[derive(Debug, PartialEq)]
pub enum RowUnifError {
    /// The LHS had a binding that was missing in the RHS.
    MissingRow(Ident),
    /// One of the row was ill-formed (typically, a tail was neither a row nor a variable).
    ///
    /// This should probably not happen with proper restrictions on the parser and a correct
    /// typechecking algorithm. We let it as an error for now, but it could be removed and turned
    /// into a panic! in the future.
    IllformedRow(TypeWrapper),
    /// A [row constraint](./type.RowConstr.html) was violated.
    IncompatibleConstraints(Ident, Option<TypeWrapper>),
}

impl RowUnifError {
    /// Convert a row unification error to a unification error.
    ///
    /// There is a hierarchy between error types, from the most local/specific to the most high-level:
    /// - [`RowUnifError`](./enum.RowUnifError.html)
    /// - [`UnifError`](./enum.UnifError.html)
    /// - [`TypecheckError`](../errors/enum.TypecheckError.html)
    ///
    /// Each level usually adds information (such as types or positions) and group different
    /// specific errors into most general ones.
    pub fn to_unif_err(self, left: TypeWrapper, right: TypeWrapper) -> UnifError {
        match self {
            RowUnifError::MissingRow(id) => UnifError::MissingRow(id, left, right),
            RowUnifError::IllformedRow(tyw) => UnifError::IllformedRow(tyw),
            RowUnifError::IncompatibleConstraints(id, tyw) => {
                UnifError::RowConflict(id, tyw, left, right)
            }
        }
    }
}

/// Error during the unification of two types.
#[derive(Debug, PartialEq)]
pub enum UnifError {
    /// Tried to unify two incompatible types.
    IncompatibleTypes(TypeWrapper, TypeWrapper),
    /// Tried to unify an enum row and a record row.
    IncompatibleRows(Ident, Option<TypeWrapper>, Option<TypeWrapper>),
    /// Tried to unify two distinct type constants.
    IncompatibleConst(usize, usize),
    /// Tried to unify two rows, but an identifier of the LHS was absent from the RHS.
    MissingRow(Ident, TypeWrapper, TypeWrapper),
    /// A row was ill-formed (see []()).
    IllformedRow(TypeWrapper),
    /// Tried to unify a unification variable with a row type violating the [row
    /// constraints](./type.RowConstr.html) of the variable.
    RowConflict(Ident, Option<TypeWrapper>, TypeWrapper, TypeWrapper),
    /// Tried to unify a type constant with another different type.
    WithConst(usize, TypeWrapper),
    /// A flat type, which is an opaque type corresponding to custom contracts, contains a Nickel
    /// term different from a variable. Only a variables is a legal inner term of a flat type.
    IllformedFlatType(RichTerm),
    /// An unbound type variable was referenced.
    UnboundTypeVariable(Ident),
}

impl UnifError {
    /// Convert a unification error to a typechecking error.
    ///
    /// There is a hierarchy between error types, from the most local/specific to the most high-level:
    /// - [`RowUnifError`](./enum.RowUnifError.html)
    /// - [`UnifError`](./enum.UnifError.html)
    /// - [`TypecheckError`](../errors/enum.TypecheckError.html)
    ///
    /// Each level usually adds information (such as types or positions) and group different
    /// specific errors into most general ones.
    pub fn to_typecheck_err(self, table: &UnifTable, pos_opt: &Option<RawSpan>) -> TypecheckError {
        let pos_opt = pos_opt.as_ref().cloned();
        match self {
            UnifError::IncompatibleTypes(ty1, ty2) => {
                TypecheckError::TypeMismatch(to_type(table, ty1), to_type(table, ty2), pos_opt)
            }
            UnifError::IncompatibleRows(id, ty1, ty2) => TypecheckError::RowMismatch(
                id,
                ty1.map(|tw| to_type(table, tw)),
                ty2.map(|tw| to_type(table, tw)),
                pos_opt,
            ),
            // FIXME: For now we give arbitrary names (a or b) to quantified type variables. In the
            // future, we may want to remember their original name and create a specific variant in
            // `TypecheckError` for better error reporting.
            UnifError::IncompatibleConst(_c1, _c2) => TypecheckError::TypeMismatch(
                Types(AbsType::Var(Ident(String::from("a")))),
                Types(AbsType::Var(Ident(String::from("b")))),
                pos_opt,
            ),
            UnifError::WithConst(_c, ty) => TypecheckError::TypeMismatch(
                Types(AbsType::Var(Ident(String::from("a")))),
                to_type(table, ty),
                pos_opt,
            ),
            UnifError::IllformedFlatType(rt) => {
                TypecheckError::IllformedType(Types(AbsType::Flat(rt)))
            }
            UnifError::MissingRow(id, tyw1, tyw2) => {
                TypecheckError::MissingRow(id, to_type(table, tyw1), to_type(table, tyw2), pos_opt)
            }
            UnifError::IllformedRow(tyw) => TypecheckError::IllformedType(to_type(table, tyw)),
            UnifError::RowConflict(id, tyw, left, right) => TypecheckError::RowConflict(
                id,
                tyw.map(|tyw| to_type(table, tyw)),
                to_type(table, left),
                to_type(table, right),
                pos_opt,
            ),
            UnifError::UnboundTypeVariable(ident) => {
                TypecheckError::UnboundTypeVariable(ident, pos_opt)
            }
        }
    }
}

/// The typing environment.
type Environment = HashMap<Ident, TypeWrapper>;

/// The shared state of unification.
pub struct State<'a> {
    /// The import resolver, to retrieve and typecheck imports.
    resolver: &'a mut dyn ImportResolver,
    /// The unification table (see [`GTypes`](type.GTypes.html)).
    table: &'a mut UnifTable,
    /// Row constraints (see [`GConstr`](type.GConstr.html)).
    constr: &'a mut RowConstr,
}

/// Typecheck a term.
///
/// Return the inferred type in case of success. This is just a wrapper that calls
/// [`type_check_`](fn.type_check_.html) with a fresh unification variable as goal.
pub fn type_check(t: &Term, resolver: &mut dyn ImportResolver) -> Result<Types, String> {
    let mut state = State {
        resolver,
        table: &mut UnifTable::new(),
        constr: &mut RowConstr::new(),
    };
    let ty = TypeWrapper::Ptr(new_var(&mut state.table));
    type_check_(&mut state, Environment::new(), false, t, ty.clone())?;

    Ok(to_type(&state.table, ty))
}

/// Typecheck a term against a specific type.
///
/// # Arguments
///
/// - `state`: the unification state (see [`State`](struct.State.html)).
/// - `env`: the typing environment, mapping free variable to types.
/// - `strict`: the typechecking mode.
/// - `t`: the term to check.
/// - `ty`: the type to check the term against.
fn type_check_(
    state: &mut State,
    mut env: Environment,
    strict: bool,
    t: &Term,
    ty: TypeWrapper,
) -> Result<(), String> {
    match t {
        Term::Bool(_) => unify(state, strict, ty, TypeWrapper::Concrete(AbsType::Bool())),
        Term::Num(_) => unify(state, strict, ty, TypeWrapper::Concrete(AbsType::Num())),
        Term::Str(_) => unify(state, strict, ty, TypeWrapper::Concrete(AbsType::Str())),
        Term::StrChunks(chunks) => {
            unify(state, strict, ty, TypeWrapper::Concrete(AbsType::Str()))?;

            chunks.iter().try_for_each(|chunk| -> Result<(), String> {
                match chunk {
                    StrChunk::Literal(_) => Ok(()),
                    StrChunk::Expr(t) => type_check_(
                        state,
                        env.clone(),
                        strict,
                        t.as_ref(),
                        TypeWrapper::Concrete(AbsType::Dyn()),
                    ),
                }
            })
        }
        Term::Fun(x, rt) => {
            let src = TypeWrapper::Ptr(new_var(state.table));
            // TODO what to do here, this makes more sense to me, but it means let x = foo in bar
            // behaves quite different to (\x.bar) foo, worth considering if it's ok to type these two differently
            // let src = TypeWrapper::The(AbsType::Dyn());
            let trg = TypeWrapper::Ptr(new_var(state.table));
            let arr =
                TypeWrapper::Concrete(AbsType::arrow(Box::new(src.clone()), Box::new(trg.clone())));

            unify(state, strict, ty, arr)?;

            env.insert(x.clone(), src);
            type_check_(state, env, strict, rt.as_ref(), trg)
        }
        Term::List(terms) => {
            unify(state, strict, ty, TypeWrapper::Concrete(AbsType::List()))?;

            terms.iter().try_for_each(|t| -> Result<(), String> {
                type_check_(
                    state,
                    env.clone(),
                    false,
                    t.as_ref(),
                    TypeWrapper::Concrete(AbsType::Dyn()),
                )
            })
        }
        Term::Lbl(_) => {
            // TODO implement lbl type
            unify(state, strict, ty, TypeWrapper::Concrete(AbsType::Dyn()))
        }
        Term::Let(x, re, rt) => {
            let e = re.as_ref();
            let ty_let = apparent_type(e, state.table, strict);
            type_check_(state, env.clone(), strict, e, ty_let.clone())?;

            // TODO move this up once lets are rec
            env.insert(x.clone(), ty_let);
            type_check_(state, env, strict, rt.as_ref(), ty)
        }
        Term::App(re, rt) => {
            let src = TypeWrapper::Ptr(new_var(state.table));
            let arr = TypeWrapper::Concrete(AbsType::arrow(Box::new(src.clone()), Box::new(ty)));

            // This order shouldn't be changed, since applying a function to a record
            // may change how it's typed (static or dynamic)
            // This is good hint a bidirectional algorithm would make sense...
            type_check_(state, env.clone(), strict, re.as_ref(), arr)?;
            type_check_(state, env, strict, rt.as_ref(), src)
        }
        Term::Var(x) => {
            let x_ty = env
                .get(&x)
                .ok_or_else(|| format!("Found an unbound var {:?}", x))?;

            let instantiated =
                instantiate_foralls_with(state.table, x_ty.clone(), TypeWrapper::Ptr)?;
            unify(state, strict, ty, instantiated)
        }
        Term::Enum(id) => {
            let row = TypeWrapper::Ptr(new_var(state.table));
            unify(
                state,
                strict,
                ty,
                TypeWrapper::Concrete(AbsType::Enum(Box::new(TypeWrapper::Concrete(
                    AbsType::RowExtend(id.clone(), None, Box::new(row)),
                )))),
            )
        }
        Term::Record(stat_map) | Term::RecRecord(stat_map) => {
            // For recursive records, we look at the apparent type of each field and bind it in
            // typed_vars before actually typechecking the content of fields
            if let Term::RecRecord(_) = t {
                env.extend(
                    stat_map.iter().map(|(id, rt)| {
                        (id.clone(), apparent_type(rt.as_ref(), state.table, strict))
                    }),
                );
            }

            let root_ty = if let TypeWrapper::Ptr(p) = ty {
                get_root(state.table, p)
            } else {
                ty.clone()
            };

            if let TypeWrapper::Concrete(AbsType::DynRecord(rec_ty)) = root_ty.clone() {
                // Checking for an dynamic record
                stat_map
                    .into_iter()
                    .try_for_each(|e| -> Result<(), String> {
                        let (_, t) = e;
                        type_check_(state, env.clone(), strict, t.as_ref(), (*rec_ty).clone())
                    })
            } else {
                // inferring static record
                let row = stat_map.into_iter().try_fold(
                    TypeWrapper::Concrete(AbsType::RowEmpty()),
                    |acc, e| -> Result<TypeWrapper, String> {
                        let (id, t) = e;

                        let ty = TypeWrapper::Ptr(new_var(state.table));
                        type_check_(state, env.clone(), strict, t.as_ref(), ty.clone())?;

                        Ok(TypeWrapper::Concrete(AbsType::RowExtend(
                            id.clone(),
                            Some(Box::new(ty)),
                            Box::new(acc),
                        )))
                    },
                )?;

                unify(
                    state,
                    strict,
                    ty,
                    TypeWrapper::Concrete(AbsType::StaticRecord(Box::new(row))),
                )
            }
        }
        Term::Op1(op, rt) => {
            let ty_op = get_uop_type(state, env.clone(), strict, op)?;

            let src = TypeWrapper::Ptr(new_var(state.table));
            let arr = TypeWrapper::Concrete(AbsType::arrow(Box::new(src.clone()), Box::new(ty)));

            unify(state, strict, arr, ty_op)?;
            type_check_(state, env, strict, rt.as_ref(), src)
        }
        Term::Op2(op, re, rt) => {
            let ty_op = get_bop_type(state, env.clone(), strict, op)?;

            let src1 = TypeWrapper::Ptr(new_var(state.table));
            let src2 = TypeWrapper::Ptr(new_var(state.table));
            let arr = TypeWrapper::Concrete(AbsType::arrow(
                Box::new(src1.clone()),
                Box::new(TypeWrapper::Concrete(AbsType::arrow(
                    Box::new(src2.clone()),
                    Box::new(ty),
                ))),
            ));

            unify(state, strict, arr, ty_op)?;
            type_check_(state, env.clone(), strict, re.as_ref(), src1)?;
            type_check_(state, env, strict, rt.as_ref(), src2)
        }
        Term::Promise(ty2, _, rt) => {
            let tyw2 = to_typewrapper(ty2.clone());

            let instantiated = instantiate_foralls_with(state.table, tyw2, TypeWrapper::Constant)?;

            unify(state, strict, ty.clone(), to_typewrapper(ty2.clone()))?;
            type_check_(state, env, true, rt.as_ref(), instantiated)
        }
        Term::Assume(ty2, _, rt) => {
            unify(state, strict, ty.clone(), to_typewrapper(ty2.clone()))?;
            let new_ty = TypeWrapper::Ptr(new_var(state.table));
            type_check_(state, env, false, rt.as_ref(), new_ty)
        }
        Term::Sym(_) => unify(state, strict, ty, TypeWrapper::Concrete(AbsType::Sym())),
        Term::Wrapped(_, rt)
        | Term::DefaultValue(rt)
        | Term::ContractWithDefault(_, _, rt)
        | Term::Docstring(_, rt) => type_check_(state, env, strict, rt.as_ref(), ty),
        Term::Contract(_, _) => Ok(()),
        Term::Import(_) => unify(state, strict, ty, TypeWrapper::Concrete(AbsType::Dyn())),
        Term::ResolvedImport(file_id) => {
            let t = state
                .resolver
                .get(file_id.clone())
                .expect("Internal error: resolved import not found ({:?}) during typechecking.");
            type_check(t.as_ref(), state.resolver).map(|_ty| ())
        }
    }
}

/// Determine the apparent type of a let-bound expression.
///
/// When a let-binding `let x = bound_exp in body` is processed, the type of `bound_exp` must be
/// determined to be associated to the bound variable `x` in the typing environment (`typed_vars`).
/// Then, future occurrences of `x` can be given this type when used in a `Promise` block.
///
/// The role of `apparent_type` is precisely to determine the type of `bound_exp`:
/// - if `bound_exp` is annotated by an `Assume` or a `Promise`, use the user-provided type.
/// - Otherwise:
///     * in non strict mode, we won't (and possibly can't) infer the type of `bound_exp`: just
///       return `Dyn`.
///     * in strict mode, we will typecheck `bound_exp`: return a new unification variable to be
///       associated to `bound_exp`.
fn apparent_type(t: &Term, table: &mut UnifTable, strict: bool) -> TypeWrapper {
    match t {
        Term::Assume(ty, _, _) | Term::Promise(ty, _, _) => to_typewrapper(ty.clone()),
        _ if strict => TypeWrapper::Ptr(new_var(table)),
        _ => TypeWrapper::Concrete(AbsType::Dyn()),
    }
}

/// The types on which the unification algorithm operates, which may be either a concrete type, a
/// type constant or a unification variable.
#[derive(Clone, PartialEq, Debug)]
pub enum TypeWrapper {
    /// A concrete type (like `Num` or `Str -> Str`).
    Concrete(AbsType<Box<TypeWrapper>>),
    /// A rigid type constant which cannot be unified with anything but itself.
    Constant(usize),
    /// A unification variable.
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
            Concrete(AbsType::List()) => Concrete(AbsType::List()),
            Constant(x) => Constant(x),
            Ptr(x) => Ptr(x),
        }
    }
}

/// Look for a binding in a row, or add a new one if it is not present and if allowed by [row
/// constraints](type.GConstr.html).
///
/// The row may be given as a concrete type or as a unification variable.
///
/// # Return
///
/// The type newly bound to `id` in the row together with the tail of the new row. If `id` was
/// already in `r`, it does not change the binding and return the corresponding type instead as a
/// first component.
fn row_add(
    state: &mut State,
    id: Ident,
    ty: Option<Box<TypeWrapper>>,
    mut r: TypeWrapper,
) -> Result<(Option<Box<TypeWrapper>>, TypeWrapper), String> {
    if let TypeWrapper::Ptr(p) = r {
        r = get_root(state.table, p);
    }
    match r {
        TypeWrapper::Concrete(AbsType::RowEmpty()) => {
            Err("The row didn't have the id and it wasn't open".to_string())
        }
        TypeWrapper::Concrete(AbsType::RowExtend(id2, ty2, r2)) => {
            if id == id2 {
                Ok((ty2, *r2))
            } else {
                let (extracted_type, subrow) = row_add(state, id, ty, *r2)?;
                Ok((
                    extracted_type,
                    TypeWrapper::Concrete(AbsType::RowExtend(id2, ty2, Box::new(subrow))),
                ))
            }
        }
        TypeWrapper::Concrete(not_row) => Err(format!("Expected a row, got {:?}", not_row)),
        TypeWrapper::Ptr(root) => {
            if let Some(set) = state.constr.get(&root) {
                if set.contains(&id) {
                    return Err(format!(
                        "Tried to add {:?} to the row, but it was constrained.",
                        id
                    ));
                }
            }
            let new_row = TypeWrapper::Ptr(new_var(state.table));
            constraint(state, new_row.clone(), id.clone())?;
            state.table.insert(
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

/// Try to unify two types.
pub fn unify(
    state: &mut State,
    strict: bool,
    mut t1: TypeWrapper,
    mut t2: TypeWrapper,
) -> Result<(), String> {
    if !strict {
        // TODO think whether this makes sense, without this we can't write the Y combinator
        return Ok(());
    }
    if let TypeWrapper::Ptr(pt1) = t1 {
        t1 = get_root(state.table, pt1);
    }
    if let TypeWrapper::Ptr(pt2) = t2 {
        t2 = get_root(state.table, pt2);
    }

    // t1 and t2 are roots of the type
    match (t1, t2) {
        (TypeWrapper::Concrete(s1), TypeWrapper::Concrete(s2)) => match (s1, s2) {
            (AbsType::Dyn(), AbsType::Dyn()) => Ok(()),
            (AbsType::Num(), AbsType::Num()) => Ok(()),
            (AbsType::Bool(), AbsType::Bool()) => Ok(()),
            (AbsType::Str(), AbsType::Str()) => Ok(()),
            (AbsType::List(), AbsType::List()) => Ok(()),
            (AbsType::Sym(), AbsType::Sym()) => Ok(()),
            (AbsType::Arrow(s1s, s1t), AbsType::Arrow(s2s, s2t)) => {
                unify(state, strict, *s1s, *s2s)?;
                unify(state, strict, *s1t, *s2t)
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
                    state.table, t
                ))
            } // Right now it only unifies equally named variables
            (AbsType::RowEmpty(), AbsType::RowEmpty()) => Ok(()),
            (AbsType::RowExtend(id, ty, t), r2 @ AbsType::RowExtend(_, _, _)) => {
                let (ty2, r2) = row_add(state, id, ty.clone(), TypeWrapper::Concrete(r2))?;

                match (ty, ty2) {
                    (None, None) => Ok(()),
                    (Some(ty), Some(ty2)) => unify(state, strict, *ty, *ty2),
                    _ => Err("Couldn't unify two types from a row!".to_string()),
                }?;
                unify(state, strict, *t, r2)
            }
            (AbsType::Enum(r), AbsType::Enum(r2)) => unify(state, strict, *r, *r2),
            (AbsType::StaticRecord(r), AbsType::StaticRecord(r2)) => unify(state, strict, *r, *r2),
            (AbsType::DynRecord(t), AbsType::DynRecord(t2)) => unify(state, strict, *t, *t2),
            (AbsType::Var(ref i1), AbsType::Var(ref i2)) if i1 == i2 => Ok(()),
            (AbsType::Forall(i1, t1t), AbsType::Forall(i2, t2t)) => {
                // Very stupid (slow) implementation
                let constant_type = TypeWrapper::Constant(new_var(state.table));

                unify(
                    state,
                    strict,
                    t1t.subst(i1, constant_type.clone()),
                    t2t.subst(i2, constant_type),
                )
            }
            (a, b) => Err(format!("The following types dont match {:?} -- {:?}", a, b)),
        },
        (TypeWrapper::Ptr(r1), TypeWrapper::Ptr(r2)) => {
            if r1 != r2 {
                let mut r1_constr = state.constr.remove(&r1).unwrap_or_default();
                let mut r2_constr = state.constr.remove(&r2).unwrap_or_default();
                state
                    .constr
                    .insert(r1, r1_constr.drain().chain(r2_constr.drain()).collect());

                state.table.insert(r1, Some(TypeWrapper::Ptr(r2)));
            }
            Ok(())
        }

        (TypeWrapper::Ptr(p), s @ TypeWrapper::Concrete(_))
        | (TypeWrapper::Ptr(p), s @ TypeWrapper::Constant(_))
        | (s @ TypeWrapper::Concrete(_), TypeWrapper::Ptr(p))
        | (s @ TypeWrapper::Constant(_), TypeWrapper::Ptr(p)) => {
            state.table.insert(p, Some(s));
            Ok(())
        }
        (TypeWrapper::Constant(i1), TypeWrapper::Constant(i2)) if i1 == i2 => Ok(()),
        (a, b) => Err(format!("Couldn't unify {:?} and {:?}", a, b)),
    }
}

/// Convert a vanilla Nickel type to a type wrapper.
fn to_typewrapper(t: Types) -> TypeWrapper {
    let Types(t2) = t;

    let t3 = t2.map(|x| Box::new(to_typewrapper(*x)));

    TypeWrapper::Concrete(t3)
}

/// Extract the concrete type (if any) corresponding to a type wrapper. Free unification variables
/// are replaced with the type `Dyn`.
fn to_type(table: &UnifTable, ty: TypeWrapper) -> Types {
    match ty {
        TypeWrapper::Ptr(p) => match get_root(table, p) {
            t @ TypeWrapper::Concrete(_) => to_type(table, t),
            _ => Types(AbsType::Dyn()),
        },
        TypeWrapper::Constant(_) => Types(AbsType::Dyn()),
        TypeWrapper::Concrete(t) => {
            let mapped = t.map(|btyp| Box::new(to_type(table, *btyp)));
            Types(mapped)
        }
    }
}

/// Instantiate the type variables which are quantified in head position with type constants.
///
/// For example, `forall a. forall b. a -> (forall c. b -> c)` is transformed to `cst1 -> (forall
/// c. cst2 -> c)` where `cst1` and `cst2` are fresh type constants.  This is used when
/// typechecking `forall`s: all quantified type variables in head position are replaced by rigid
/// type constants, and the term is then typechecked normally. As these constants cannot be unified
/// with anything, this forces all the occurrences of a type variable to be the same type.
fn instantiate_foralls_with<F>(
    table: &mut UnifTable,
    mut ty: TypeWrapper,
    f: F,
) -> Result<TypeWrapper, String>
where
    F: Fn(usize) -> TypeWrapper,
{
    if let TypeWrapper::Ptr(p) = ty {
        ty = get_root(table, p);
    }

    while let TypeWrapper::Concrete(AbsType::Forall(id, forall_ty)) = ty {
        let var = f(new_var(table));
        ty = forall_ty.subst(id, var);
    }
    Ok(ty)
}

/// Type of unary operations.
pub fn get_uop_type(
    state: &mut State,
    env: Environment,
    strict: bool,
    op: &UnaryOp<RichTerm>,
) -> Result<TypeWrapper, String> {
    Ok(match op {
        // forall a. bool -> a -> a -> a
        UnaryOp::Ite() => {
            let branches = TypeWrapper::Ptr(new_var(state.table));

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
        // Num -> Bool
        UnaryOp::IsZero() => TypeWrapper::Concrete(AbsType::arrow(
            Box::new(TypeWrapper::Concrete(AbsType::Num())),
            Box::new(TypeWrapper::Concrete(AbsType::Bool())),
        )),
        // forall a. a -> Bool
        UnaryOp::IsNum()
        | UnaryOp::IsBool()
        | UnaryOp::IsStr()
        | UnaryOp::IsFun()
        | UnaryOp::IsList() => {
            let inp = TypeWrapper::Ptr(new_var(state.table));

            TypeWrapper::Concrete(AbsType::arrow(
                Box::new(inp),
                Box::new(TypeWrapper::Concrete(AbsType::Bool())),
            ))
        }
        // forall a. Dyn -> a
        UnaryOp::Blame() => {
            let res = TypeWrapper::Ptr(new_var(state.table));

            TypeWrapper::Concrete(AbsType::arrow(
                Box::new(TypeWrapper::Concrete(AbsType::Dyn())),
                Box::new(res),
            ))
        }
        // Dyn -> Bool
        UnaryOp::Pol() => TypeWrapper::Concrete(AbsType::arrow(
            Box::new(TypeWrapper::Concrete(AbsType::Dyn())),
            Box::new(TypeWrapper::Concrete(AbsType::Bool())),
        )),
        // forall rows. ( rows ) -> ( `id, rows )
        UnaryOp::Embed(id) => {
            let row = TypeWrapper::Ptr(new_var(state.table));
            constraint(state, row.clone(), id.clone())?;

            TypeWrapper::Concrete(AbsType::Arrow(
                Box::new(TypeWrapper::Concrete(AbsType::Enum(Box::new(row.clone())))),
                Box::new(TypeWrapper::Concrete(AbsType::Enum(Box::new(
                    TypeWrapper::Concrete(AbsType::RowExtend(id.clone(), None, Box::new(row))),
                )))),
            ))
        }
        // 1. rows -> a
        // 2. forall b. b -> a
        // Rows is ( `label1, .., `labeln ) for label in l.keys().
        // Unify each branch in l.values() with a.
        // If the switch has a default case, the more general type 2. is used.
        UnaryOp::Switch(l, d) => {
            // Currently, if it has a default value, we typecheck the whole thing as
            // taking ANY enum, since it's more permissive and there's not a loss of information
            let res = TypeWrapper::Ptr(new_var(state.table));

            for exp in l.values() {
                type_check_(state, env.clone(), strict, exp.as_ref(), res.clone())?;
            }

            let row = match d {
                Some(e) => {
                    type_check_(state, env.clone(), strict, e.as_ref(), res.clone())?;
                    TypeWrapper::Ptr(new_var(state.table))
                }
                None => l.iter().try_fold(
                    TypeWrapper::Concrete(AbsType::RowEmpty()),
                    |acc, x| -> Result<TypeWrapper, String> {
                        constraint(state, acc.clone(), x.0.clone())?;
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
        // Dyn -> Dyn
        UnaryOp::ChangePolarity() | UnaryOp::GoDom() | UnaryOp::GoCodom() | UnaryOp::Tag(_) => {
            TypeWrapper::Concrete(AbsType::arrow(
                Box::new(TypeWrapper::Concrete(AbsType::Dyn())),
                Box::new(TypeWrapper::Concrete(AbsType::Dyn())),
            ))
        }
        // Sym -> Dyn -> Dyn
        UnaryOp::Wrap() => TypeWrapper::Concrete(AbsType::arrow(
            Box::new(TypeWrapper::Concrete(AbsType::Sym())),
            Box::new(TypeWrapper::Concrete(AbsType::arrow(
                Box::new(TypeWrapper::Concrete(AbsType::Dyn())),
                Box::new(TypeWrapper::Concrete(AbsType::Dyn())),
            ))),
        )),
        // forall rows a. { rows, id: a } -> a
        UnaryOp::StaticAccess(id) => {
            let row = TypeWrapper::Ptr(new_var(state.table));
            let res = TypeWrapper::Ptr(new_var(state.table));

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
        // { _ : a} -> { _ : b }
        // Unify f with Str -> a -> b.
        UnaryOp::MapRec(f) => {
            // Assuming f has type Str -> a -> b,
            // this has type DynRecord(a) -> DynRecord(b)

            let a = TypeWrapper::Ptr(new_var(state.table));
            let b = TypeWrapper::Ptr(new_var(state.table));

            let f_type = TypeWrapper::Concrete(AbsType::Arrow(
                Box::new(TypeWrapper::Concrete(AbsType::Str())),
                Box::new(TypeWrapper::Concrete(AbsType::Arrow(
                    Box::new(a.clone()),
                    Box::new(b.clone()),
                ))),
            ));

            type_check_(state, env.clone(), strict, f.as_ref(), f_type)?;

            TypeWrapper::Concrete(AbsType::Arrow(
                Box::new(TypeWrapper::Concrete(AbsType::DynRecord(Box::new(a)))),
                Box::new(TypeWrapper::Concrete(AbsType::DynRecord(Box::new(b)))),
            ))
        }
        // forall a b. a -> b -> b
        UnaryOp::Seq() | UnaryOp::DeepSeq() => {
            let fst = TypeWrapper::Ptr(new_var(state.table));
            let snd = TypeWrapper::Ptr(new_var(state.table));

            TypeWrapper::Concrete(AbsType::Arrow(
                Box::new(fst),
                Box::new(TypeWrapper::Concrete(AbsType::Arrow(
                    Box::new(snd.clone()),
                    Box::new(snd),
                ))),
            ))
        }
        // List -> Dyn
        UnaryOp::ListHead() => TypeWrapper::Concrete(AbsType::Arrow(
            Box::new(TypeWrapper::Concrete(AbsType::List())),
            Box::new(TypeWrapper::Concrete(AbsType::Dyn())),
        )),
        // List -> List
        UnaryOp::ListTail() => TypeWrapper::Concrete(AbsType::Arrow(
            Box::new(TypeWrapper::Concrete(AbsType::List())),
            Box::new(TypeWrapper::Concrete(AbsType::List())),
        )),
        // List -> Num
        UnaryOp::ListLength() => TypeWrapper::Concrete(AbsType::Arrow(
            Box::new(TypeWrapper::Concrete(AbsType::List())),
            Box::new(TypeWrapper::Concrete(AbsType::Num())),
        )),
        // This should not happen, as ChunksConcat() is only produced during evaluation.
        UnaryOp::ChunksConcat(_, _) => panic!("cannot type ChunksConcat()"),
    })
}

/// Type of a binary operation.
pub fn get_bop_type(
    state: &mut State,
    env: Environment,
    strict: bool,
    op: &BinaryOp<RichTerm>,
) -> Result<TypeWrapper, String> {
    match op {
        // Num -> Num -> Num
        BinaryOp::Plus() => Ok(TypeWrapper::Concrete(AbsType::arrow(
            Box::new(TypeWrapper::Concrete(AbsType::Num())),
            Box::new(TypeWrapper::Concrete(AbsType::arrow(
                Box::new(TypeWrapper::Concrete(AbsType::Num())),
                Box::new(TypeWrapper::Concrete(AbsType::Num())),
            ))),
        ))),
        // Str -> Str -> Str
        BinaryOp::PlusStr() => Ok(TypeWrapper::Concrete(AbsType::arrow(
            Box::new(TypeWrapper::Concrete(AbsType::Str())),
            Box::new(TypeWrapper::Concrete(AbsType::arrow(
                Box::new(TypeWrapper::Concrete(AbsType::Str())),
                Box::new(TypeWrapper::Concrete(AbsType::Str())),
            ))),
        ))),
        // Sym -> Dyn -> Dyn -> Dyn
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
        // Bool -> Bool -> Bool
        BinaryOp::EqBool() => Ok(TypeWrapper::Concrete(AbsType::arrow(
            Box::new(TypeWrapper::Concrete(AbsType::Bool())),
            Box::new(TypeWrapper::Concrete(AbsType::arrow(
                Box::new(TypeWrapper::Concrete(AbsType::Bool())),
                Box::new(TypeWrapper::Concrete(AbsType::Bool())),
            ))),
        ))),
        // forall a. Str -> { _ : a} -> a
        BinaryOp::DynAccess() => {
            let res = TypeWrapper::Ptr(new_var(state.table));

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
        // Str -> { _ : a } -> { _ : a }
        // Unify t with a.
        BinaryOp::DynExtend(t) => {
            let res = TypeWrapper::Ptr(new_var(state.table));

            type_check_(state, env.clone(), strict, t.as_ref(), res.clone())?;

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
        // forall a. Str -> { _ : a } -> { _ : a}
        BinaryOp::DynRemove() => {
            let res = TypeWrapper::Ptr(new_var(state.table));

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
        // Str -> Dyn -> Bool
        BinaryOp::HasField() => Ok(TypeWrapper::Concrete(AbsType::Arrow(
            Box::new(TypeWrapper::Concrete(AbsType::Arrow(
                Box::new(TypeWrapper::Concrete(AbsType::Str())),
                Box::new(TypeWrapper::Concrete(AbsType::Dyn())),
            ))),
            Box::new(TypeWrapper::Concrete(AbsType::Bool())),
        ))),
        // List -> List -> List
        BinaryOp::ListConcat() => Ok(TypeWrapper::Concrete(AbsType::Arrow(
            Box::new(TypeWrapper::Concrete(AbsType::List())),
            Box::new(TypeWrapper::Concrete(AbsType::Arrow(
                Box::new(TypeWrapper::Concrete(AbsType::List())),
                Box::new(TypeWrapper::Concrete(AbsType::List())),
            ))),
        ))),
        // forall a b. (a -> b) -> List -> List
        BinaryOp::ListMap() => {
            let src = TypeWrapper::Ptr(new_var(state.table));
            let tgt = TypeWrapper::Ptr(new_var(state.table));
            let arrow = TypeWrapper::Concrete(AbsType::Arrow(Box::new(src), Box::new(tgt)));

            Ok(TypeWrapper::Concrete(AbsType::Arrow(
                Box::new(arrow),
                Box::new(TypeWrapper::Concrete(AbsType::Arrow(
                    Box::new(TypeWrapper::Concrete(AbsType::List())),
                    Box::new(TypeWrapper::Concrete(AbsType::List())),
                ))),
            )))
        }
        // List -> Num -> Dyn
        BinaryOp::ListElemAt() => Ok(TypeWrapper::Concrete(AbsType::Arrow(
            Box::new(TypeWrapper::Concrete(AbsType::List())),
            Box::new(TypeWrapper::Concrete(AbsType::Arrow(
                Box::new(TypeWrapper::Concrete(AbsType::Num())),
                Box::new(TypeWrapper::Concrete(AbsType::Dyn())),
            ))),
        ))),
        // Dyn -> Dyn -> Dyn
        BinaryOp::Merge() => Ok(TypeWrapper::Concrete(AbsType::arrow(
            Box::new(TypeWrapper::Concrete(AbsType::Dyn())),
            Box::new(TypeWrapper::Concrete(AbsType::arrow(
                Box::new(TypeWrapper::Concrete(AbsType::Dyn())),
                Box::new(TypeWrapper::Concrete(AbsType::Dyn())),
            ))),
        ))),
    }
}

/// The unification table.
///
/// Map each unification variable to either another type variable or a concrete type it has been
/// unified with. Each binding `(ty, var)` in this map should be thought of an edge in a
/// unification graph.
pub type UnifTable = HashMap<usize, Option<TypeWrapper>>;

/// Row constraints.
///
/// A row constraint applies to a unification variable appearing inside a row type (such as `r` in
/// `{ someId: SomeType, r }`). It is a set of identifiers that said row must NOT contain, to
/// forbid ill-formed types with multiple declaration of the same id, for example `{ a: Num, a:
/// String}`.
pub type RowConstr = HashMap<usize, HashSet<Ident>>;

/// Create a fresh unification variable.
fn new_var(state: &mut UnifTable) -> usize {
    let nxt = state.len();
    state.insert(nxt, None);
    nxt
}

/// Add a row constraint on a type.
///
/// See [`GConstr`](type.GConstr.html).
fn constraint(state: &mut State, x: TypeWrapper, id: Ident) -> Result<(), String> {
    match x {
        TypeWrapper::Ptr(p) => {
            let ty = get_root(state.table, p);
            match ty {
                ty @ TypeWrapper::Concrete(_) => constraint(state, ty, id),
                TypeWrapper::Ptr(root) => {
                    if let Some(v) = state.constr.get_mut(&root) {
                        v.insert(id);
                    } else {
                        state.constr.insert(root, vec![id].into_iter().collect());
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
                constraint(state, *t, id)
            }
        }
        _ => Err(format!("Can't constraint a {:?}", x)),
    }
}

/// Follow the links in the unification table to find the representative of the equivalence class
/// of unification variable `x`.
///
/// This corresponds to the find in union-find.
// TODO This should be a union find like algorithm
pub fn get_root(table: &UnifTable, x: usize) -> TypeWrapper {
    // All queried variable must have been introduced by `new_var` and thus a corresponding entry
    // must always exist in `state`. If not, the typechecking algorithm is not correct, and we
    // panic.
    match table.get(&x).unwrap() {
        None => TypeWrapper::Ptr(x),
        Some(TypeWrapper::Ptr(y)) => get_root(table, *y),
        Some(ty @ TypeWrapper::Concrete(_)) => ty.clone(),
        Some(k @ TypeWrapper::Constant(_)) => k.clone(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error::ImportError;
    use crate::label::Label;
    use crate::parser::lexer;
    use crate::program::resolvers::{DummyResolver, SimpleResolver};
    use crate::transformations::transform;
    use codespan::Files;

    use crate::parser;

    fn type_check_no_import(t: &Term) -> Result<Types, String> {
        type_check(t, &mut DummyResolver {})
    }

    fn parse_and_typecheck(s: &str) -> Result<Types, String> {
        let id = Files::new().add("<test>", s);

        if let Ok(p) = parser::grammar::TermParser::new().parse(id, lexer::Lexer::new(&s)) {
            type_check_no_import(p.as_ref())
        } else {
            panic!("Couldn't parse {}", s)
        }
    }

    #[test]
    fn simple_no_promises() -> Result<(), String> {
        // It's easy to check these will never fail, that's why we keep them all together

        type_check_no_import(&Term::Bool(true))?;
        type_check_no_import(&Term::Num(45.))?;

        type_check_no_import(&Term::Fun(Ident("x".into()), RichTerm::var("x".into())))?;
        type_check_no_import(&Term::Let(
            Ident("x".into()),
            Term::Num(3.).into(),
            RichTerm::var("x".into()),
        ))?;

        type_check_no_import(&Term::App(Term::Num(5.).into(), Term::Bool(true).into()))?;
        type_check_no_import(
            RichTerm::plus(Term::Num(4.).into(), Term::Bool(false).into()).as_ref(),
        )?;

        Ok(())
    }

    #[test]
    fn unbound_variable_always_throws() {
        type_check_no_import(&Term::Var(Ident("x".into()))).unwrap_err();
    }

    #[test]
    fn promise_simple_checks() {
        type_check_no_import(&Term::Promise(
            Types(AbsType::Bool()),
            Label::dummy(),
            Term::Bool(true).into(),
        ))
        .unwrap();
        type_check_no_import(&Term::Promise(
            Types(AbsType::Num()),
            Label::dummy(),
            Term::Bool(true).into(),
        ))
        .unwrap_err();

        type_check_no_import(&Term::Promise(
            Types(AbsType::Num()),
            Label::dummy(),
            Term::Num(34.5).into(),
        ))
        .unwrap();
        type_check_no_import(&Term::Promise(
            Types(AbsType::Bool()),
            Label::dummy(),
            Term::Num(34.5).into(),
        ))
        .unwrap_err();

        type_check_no_import(&Term::Promise(
            Types(AbsType::Num()),
            Label::dummy(),
            Term::Assume(
                Types(AbsType::Num()),
                Label::dummy(),
                Term::Bool(true).into(),
            )
            .into(),
        ))
        .unwrap();
        type_check_no_import(&Term::Promise(
            Types(AbsType::Num()),
            Label::dummy(),
            Term::Assume(
                Types(AbsType::Bool()),
                Label::dummy(),
                Term::Num(34.).into(),
            )
            .into(),
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
            "let alwaysTrue = fun l t => if t then t else blame l in
        Promise(#alwaysTrue -> #alwaysTrue, fun x => x)",
        )
        .unwrap();
        // Only if they're named the same way
        parse_and_typecheck("Promise(#(fun l t => t) -> #(fun l t => t), fun x => x)").unwrap_err();
    }

    #[test]
    fn simple_forall() {
        parse_and_typecheck(
            "let f = Promise(forall a. a -> a, fun x => x) in
        Promise(Num, if (f true) then (f 2) else 3)",
        )
        .unwrap();

        parse_and_typecheck(
            "let f = Promise(forall a. (forall b. a -> b -> a), fun x y => x) in
        Promise(Num, if (f true 3) then (f 2 false) else 3)",
        )
        .unwrap();

        parse_and_typecheck(
            "let f = Promise(forall a. (forall b. b -> b) -> a -> a, fun f x => f x) in
            f Promise(forall y. y -> y, fun z => z)",
        )
        .unwrap();

        parse_and_typecheck(
            "let f = Promise(forall a. (forall b. a -> b -> a), fun x y => y) in
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
    fn forall_nested() {
        parse_and_typecheck(
            "let f = Promise(forall a. a -> a, let g = Assume(forall a. (a -> a), fun x => x) in g) in
            Promise(Num, if (f true) then (f 2) else 3)",
        )
        .unwrap();

        parse_and_typecheck(
            "let f = Promise(forall a. a -> a, let g = Promise(forall a. (a -> a), fun x => x) in g g) in
            Promise(Num, if (f true) then (f 2) else 3)",
        )
        .unwrap();

        parse_and_typecheck(
            "let f = Promise(forall a. a -> a, let g = Promise(forall a. (forall b. (b -> (a -> a))), fun y x => x) in g 0) in
            Promise(Num, if (f true) then (f 2) else 3)",
        )
        .unwrap();
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

    #[test]
    fn seq() {
        parse_and_typecheck("Promise(Num, seq false 1)").unwrap();
        parse_and_typecheck("Promise(forall a. (forall b. a -> b -> b), fun x y => seq x y)")
            .unwrap();
        parse_and_typecheck("let xDyn = false in let yDyn = 1 in Promise(Dyn, seq xDyn yDyn)")
            .unwrap();
    }

    #[test]
    fn simple_list() {
        parse_and_typecheck("[1, \"2\", false]").unwrap();
        parse_and_typecheck("Promise(List, [\"a\", 3, true])").unwrap();
        parse_and_typecheck("Promise(List, [Promise(forall a. a -> a, fun x => x), 3, true])")
            .unwrap();
        parse_and_typecheck("Promise(forall a. a -> List, fun x => [x])").unwrap();

        parse_and_typecheck("[1, Promise(Num, \"2\"), false]").unwrap_err();
        parse_and_typecheck("Promise(List, [Promise(String,1), true, \"b\"])").unwrap_err();
        parse_and_typecheck("Promise(Num, [1, 2, \"3\"])").unwrap_err();
    }

    #[test]
    fn lists_operations() {
        parse_and_typecheck("Promise(List -> List, fun l => tail l)").unwrap();
        parse_and_typecheck("Promise(List -> Dyn, fun l => head l)").unwrap();
        parse_and_typecheck(
            "Promise(forall a. (forall b. (a -> b) -> List -> List), fun f l => map f l)",
        )
        .unwrap();
        parse_and_typecheck("Promise(List -> List -> List, fun l1 => fun l2 => l1 @ l2)").unwrap();
        parse_and_typecheck("Promise(Num -> List -> Dyn , fun i l => elemAt l i)").unwrap();

        parse_and_typecheck("Promise(forall a. (List -> a), fun l => head l)").unwrap_err();
        parse_and_typecheck(
            "Promise(forall a. (forall b. (a -> b) -> List -> b), fun f l => elemAt (map f l) 0)",
        )
        .unwrap_err();
    }

    #[test]
    fn imports() {
        let mut resolver = SimpleResolver::new();
        resolver.add_source(String::from("good"), String::from("Promise(Num, 1 + 1)"));
        resolver.add_source(String::from("bad"), String::from("Promise(Num, false)"));
        resolver.add_source(
            String::from("proxy"),
            String::from("let x = import \"bad\" in x"),
        );

        fn mk_import<R>(import: &str, resolver: &mut R) -> Result<RichTerm, ImportError>
        where
            R: ImportResolver,
        {
            transform(
                RichTerm::let_in(
                    "x",
                    Term::Import(String::from(import)).into(),
                    RichTerm::var(String::from("x")),
                ),
                resolver,
            )
        };

        type_check(
            mk_import("good", &mut resolver).unwrap().as_ref(),
            &mut resolver,
        )
        .unwrap();

        type_check(
            mk_import("proxy", &mut resolver).unwrap().as_ref(),
            &mut resolver,
        )
        .unwrap_err();
    }

    #[test]
    fn recursive_records() {
        parse_and_typecheck(
            "Promise({ {| a : Num, b : Num, |} }, { a = Promise(Num,1); b = a + 1})",
        )
        .unwrap();
        parse_and_typecheck(
            "Promise({ {| a : Num, b : Num, |} }, { a = Promise(Num,true); b = a + 1})",
        )
        .unwrap_err();
        parse_and_typecheck(
            "Promise({ {| a : Num, b : Bool, |} }, { a = 1; b = Promise(Bool, a) } )",
        )
        .unwrap_err();
        parse_and_typecheck("Promise({ {| a : Num, |} }, { a = Promise(Num, 1 + a) })").unwrap();
    }

    #[test]
    fn let_inference() {
        parse_and_typecheck("Promise(Num, let x = 1 + 2 in let f = fun x => x + 1 in f x)")
            .unwrap();
        parse_and_typecheck("Promise(Num, let x = 1 + 2 in let f = fun x => x ++ \"a\" in f x)")
            .unwrap_err();

        // Fields in recursive records are treated in the type environment in the same way as let-bound expressions
        parse_and_typecheck("Promise({ {| a : Num, b : Num, |} }, { a = 1; b = 1 + a })").unwrap();
        parse_and_typecheck(
            "Promise({ {| f : Num -> Num, |} }, { f = fun x => if isZero x then 1 else 1 + (f (x + (-1)));})"
        ).unwrap();
        parse_and_typecheck(
            "Promise({ {| f : Num -> Num, |} }, { f = fun x => if isZero x then false else 1 + (f (x + (-1)))})"
        ).unwrap_err();
    }
}
