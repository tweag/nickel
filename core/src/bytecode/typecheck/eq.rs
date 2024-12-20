//! Computation of type equality for contracts.
//!
//! Determine if two contracts are equal as opaque types. Used to decide if two contract should
//! unify.
//!
//! ## Aliases
//!
//! One basic case we want to handle is aliases, which come in handy for parametrized contracts.
//! For example, in the following:
//!
//! `let Alias = Foo "bar" "baz" in ...`.
//!
//! We want to equate `Alias` with `Foo "bar" "baz"`.
//!
//! We also want to equate different aliases with the same definition:
//! `let Alias' = Foo "bar" "baz" in ...`, or `let Alias' = Alias in ...`.
//!
//! We want that `Alias type_eq Alias'`.
//!
//! ## Recursion
//!
//! We must refrain from following all variables links blindly, as there could be cycles in the
//! graph leading to an infinite loop:
//!
//! ```nickel
//! {
//!   Foo = Bar,
//!   Bar = Foo,
//! }
//! ```
//!
//! Because we just follows variables, and don't apply functions, we can detect cycles while
//! walking the graph. Still, as it is potentially performed many times during typechecking, type
//! equality ought to stay reasonably cheap. We choose to just set an arbitrary limit (the gas) on
//! the number of variable links that the type equality may follow. Doing so, we don't have to
//! worry about loops anymore.
//!
//! Note: we currently don't support recursive let or recursive record definitions, so this example
//! wouldn't work anyway, and there's no way to have an infinite cycle. Style, gas is a simple way
//! to bound the work done by the type equality computation.
//!
//! ## Equality on terms
//!
//! The terms inside a type may be arbitrarily complex. Primops applications, `match`, and the like
//! are quite unlikely to appear inside an annotation (they surely appear inside contract
//! definitions, both are usually put in a variable first - they unlinkely to appear _inline_ in a
//! type or contract annotation is what we mean)
//!
//! We don't want to compare functions syntactically either. The spirit of this implementation is
//! to structurally equate aliases and simple constructs that are likely to appear inlined inside
//! an annotation (applications, records, primitive constants and arrays, mostly).
//!
//! We first test for physical equality (both as an optimization and to detect two variables
//! pointing to the same contract definition in the AST). If the comparison fails, we perform
//! structural recursion, unfolding simple forms and following variables with a limited number of
//! times. For anything more complex, we bail out (returning `false`).

use super::*;
use crate::{
    bytecode::ast::{primop::PrimOp, record},
    identifier::LocIdent,
    term::IndexMap,
    typ::VarKind,
};

/// The maximal number of variable links we want to unfold before abandoning the check. It should
/// stay low, but has been fixed arbitrarily: feel fee to increase reasonably if it turns out
/// legitimate type equalities between simple contracts are unduly rejected in practice.
pub const MAX_GAS: u8 = 12;

/// State threaded through the type equality computation.
#[derive(Copy, Clone, Default)]
struct State {
    /// Used to generate temporary rigid type variables for substituting type variables when
    /// comparing foralls. Those ids never escape the type equality computations and are used
    /// solely as rigid type variables: this is why they don't need proper allocation in the
    /// unification table or to care about those ids clashing with the one generated by the
    /// typechecker. Generated type constants simply needs to be unique for the duration of the
    /// type equality computation.
    var_uid: usize,

    /// The current gas remaining for variable substitutions. Once it reaches zero and we encounter
    /// a variable, we abort the computation and return false.
    gas: u8,
}

impl State {
    fn new() -> Self {
        State {
            var_uid: 0,
            gas: MAX_GAS,
        }
    }

    /// Create a fresh unique id for a rigid type variable.
    fn fresh_cst_id(&mut self) -> VarId {
        let result = self.var_uid;
        self.var_uid += 1;
        result
    }

    /// Try to consume one unit of gas for a variable substitution. Return true in case of success,
    /// or false if the gas was already at zero.
    fn use_gas(&mut self) -> bool {
        if self.gas == 0 {
            false
        } else {
            self.gas -= 1;
            true
        }
    }
}

pub trait TypeEq<'ast> {
    /// Compute type equality.
    ///
    /// # Parameters
    ///
    /// - `env`: an environment mapping variables to their definition
    fn type_eq(&self, other: &Self, env1: &TermEnv<'ast>, env2: &TermEnv<'ast>) -> bool;
}

/// Values that can be statically compared for equality as Nickel types. This trait provides the
/// "internal" implementation that does the actual work but isn't public facing. [TypeEq]
trait TypeEqBounded<'ast> {
    /// Compute type equality with a bounded number of variable links, stored in `state`.
    fn type_eq_bounded(
        &self,
        other: &Self,
        state: &mut State,
        env1: &TermEnv<'ast>,
        env2: &TermEnv<'ast>,
    ) -> bool;
}

impl<'ast, T> TypeEqBounded<'ast> for [T]
where
    T: TypeEqBounded<'ast>,
{
    fn type_eq_bounded(
        &self,
        other: &Self,
        state: &mut State,
        env1: &TermEnv<'ast>,
        env2: &TermEnv<'ast>,
    ) -> bool {
        self.len() == other.len()
            && self
                .iter()
                .zip(other.iter())
                .all(|(x1, x2)| x1.type_eq_bounded(x2, state, env1, env2))
    }
}

impl<'ast, T> TypeEqBounded<'ast> for Option<T>
where
    T: TypeEqBounded<'ast>,
{
    fn type_eq_bounded(
        &self,
        other: &Self,
        state: &mut State,
        env1: &TermEnv<'ast>,
        env2: &TermEnv<'ast>,
    ) -> bool {
        match (self, other) {
            (Some(x1), Some(x2)) => x1.type_eq_bounded(x2, state, env1, env2),
            (None, None) => true,
            _ => false,
        }
    }
}

impl<'ast, T> TypeEqBounded<'ast> for &T
where
    T: TypeEqBounded<'ast>,
{
    fn type_eq_bounded(
        &self,
        other: &Self,
        state: &mut State,
        env1: &TermEnv<'ast>,
        env2: &TermEnv<'ast>,
    ) -> bool {
        (*self).type_eq_bounded(*other, state, env1, env2)
    }
}

impl<'ast> TypeEqBounded<'ast> for Ast<'ast> {
    fn type_eq_bounded(
        &self,
        other: &Self,
        state: &mut State,
        env1: &TermEnv<'ast>,
        env2: &TermEnv<'ast>,
    ) -> bool {
        // Test for physical equality as both an optimization and a way to cheaply equate complex
        // contracts that happen to point to the same definition (while the purposely limited
        // structural checks below may reject the equality)
        if std::ptr::eq(self, other) && Environment::ptr_eq(&env1.0, &env2.0) {
            return true;
        }

        match (&self.node, &other.node) {
            (Node::Null, Node::Null) => true,
            (Node::Bool(b1), Node::Bool(b2)) => b1 == b2,
            (Node::Number(n1), Node::Number(n2)) => n1 == n2,
            (Node::String(s1), Node::String(s2)) => s1 == s2,
            (
                Node::EnumVariant {
                    tag: tag1,
                    arg: arg1,
                },
                Node::EnumVariant {
                    tag: tag2,
                    arg: arg2,
                },
            ) => {
                let arg_eq = match (arg1.as_ref(), arg2.as_ref()) {
                    (Some(arg1), Some(arg2)) => arg1.type_eq_bounded(arg2, state, env1, env2),
                    (None, None) => true,
                    _ => false,
                };

                tag1 == tag2 && arg_eq
            }
            // We only compare string chunks when they represent a plain string (they don't contain any
            // interpolated expression), as static string may be currently parsed as such. We return
            // false for anything more complex.
            (Node::StringChunks(scs1), Node::StringChunks(scs2)) => {
                scs1.len() == scs2.len()
                    && scs1
                        .iter()
                        .zip(scs2.iter())
                        .all(|(chunk1, chunk2)| match (chunk1, chunk2) {
                            (StringChunk::Literal(s1), StringChunk::Literal(s2)) => s1 == s2,
                            _ => false,
                        })
            }
            (
                Node::App {
                    head: head1,
                    args: args1,
                },
                Node::App {
                    head: head2,
                    args: args2,
                },
            ) => {
                head1.type_eq_bounded(head2, state, env1, env2)
                    && args1.type_eq_bounded(args2, state, env1, env2)
            }
            // All variables must be bound at this stage. This is checked by the typechecker when
            // walking annotations. However, we may assume that `env` is a local environment (e.g. that
            // it doesn't include the stdlib). In that case, free variables (unbound) may be deemed
            // equal if they have the same identifier: whatever global environment the term will be put
            // in, free variables are not redefined locally and will be bound to the same value in any
            // case.
            (Node::Var(id1), Node::Var(id2))
                if env1.0.get(&id1.ident()).is_none() && env2.0.get(&id2.ident()).is_none() =>
            {
                id1 == id2
            }
            // If both variables are equal and their environment are physically equal, then they point
            // to the same thing.
            //
            // This case is supposed to handle co-recursive contracts such as `{Foo = Bar, Bar = Foo}`.
            // Although we don't build recursive environment yet, we might in the future.
            (Node::Var(id1), Node::Var(id2))
                if id1 == id2 && Environment::ptr_eq(&env1.0, &env2.0) =>
            {
                true
            }
            (Node::Var(id), _) => {
                state.use_gas()
                    && env1
                        .0
                        .get(&id.ident())
                        .map(|(ast1, env1)| ast1.type_eq_bounded(other, state, env1, env2))
                        .unwrap_or(false)
            }
            (_, Node::Var(id)) => {
                state.use_gas()
                    && env2
                        .0
                        .get(&id.ident())
                        .map(|(ast2, env2)| self.type_eq_bounded(ast2, state, env1, env2))
                        .unwrap_or(false)
            }
            (Node::Record(r1), Node::Record(r2)) => r1.type_eq_bounded(r2, state, env1, env2),
            (Node::Array(elts1), Node::Array(elts2)) => {
                elts1.type_eq_bounded(elts2, state, env1, env2)
            }
            // We must compare the inner values as well as the corresponding contracts or type
            // annotations.
            (
                Node::Annotated {
                    annot: annot1,
                    inner: inner1,
                },
                Node::Annotated {
                    annot: annot2,
                    inner: inner2,
                },
            ) => {
                // Questions:
                // - does it really make sense to compare the annotations?
                // - does it even happen to have contracts having themselves type annotations?
                // - and in the latter case, should they be declared unequal because of that?
                //
                // The answer to the last question is probably yes, because contracts are fundamentally
                // as powerful as function application, so they can change their argument.

                annot1.type_eq_bounded(annot2, state, env1, env2)
                    && inner1.type_eq_bounded(inner2, state, env1, env2)
            }
            (
                Node::PrimOpApp {
                    op: PrimOp::RecordStatAccess(id1),
                    args: args1,
                },
                Node::PrimOpApp {
                    op: PrimOp::RecordStatAccess(id2),
                    args: args2,
                },
            ) => id1 == id2 && args1.type_eq_bounded(args2, state, env1, env2),
            (Node::Type(ty1), Node::Type(ty2)) => ty1.type_eq_bounded(ty2, state, env1, env2),
            // We don't treat imports, parse errors, nor pairs of terms that don't have the same shape
            _ => false,
        }
    }
}

impl<'ast> TypeEqBounded<'ast> for Type<'ast> {
    /// Perform the type equality comparison on types. Structurally recurse into type constructors and
    /// test that subtypes or subterms (contracts) are equals.
    ///
    /// This function piggy backs on the type equality for [super::UnifType] implementation,
    /// because we need to instantiate `foralls` with rigid type variables to properly compare
    /// them.
    ///
    fn type_eq_bounded(
        &self,
        other: &Self,
        state: &mut State,
        env1: &TermEnv<'ast>,
        env2: &TermEnv<'ast>,
    ) -> bool {
        let self_as_utype = UnifType::from_type(self.clone(), env1);
        let other_as_utype = UnifType::from_type(other.clone(), env2);
        self_as_utype.type_eq_bounded(&other_as_utype, state, env1, env2)
    }
}

impl<'ast, T> TypeEqBounded<'ast> for IndexMap<LocIdent, T>
where
    T: TypeEqBounded<'ast>,
{
    fn type_eq_bounded(
        &self,
        other: &Self,
        state: &mut State,
        env1: &TermEnv<'ast>,
        env2: &TermEnv<'ast>,
    ) -> bool {
        self.len() == other.len()
            && self.iter().all(|(id, v1)| {
                other
                    .get(id)
                    .map(|v2| v1.type_eq_bounded(v2, state, env1, env2))
                    .unwrap_or(false)
            })
    }
}

impl<'ast> TypeEqBounded<'ast> for UnifEnumRows<'ast> {
    fn type_eq_bounded(
        &self,
        other: &Self,
        state: &mut State,
        env1: &TermEnv<'ast>,
        env2: &TermEnv<'ast>,
    ) -> bool {
        let map_self: Option<IndexMap<LocIdent, Option<_>>> = self
            .iter()
            .map(|item| match item {
                EnumRowsElt::Row(EnumRowF { id, typ: types }) => Some((id, types)),
                _ => None,
            })
            .collect();

        let map_other: Option<IndexMap<LocIdent, Option<_>>> = other
            .iter()
            .map(|item| match item {
                EnumRowsElt::Row(EnumRowF { id, typ: types }) => Some((id, types)),
                _ => None,
            })
            .collect();

        let (Some(map_self), Some(map_other)) = (map_self, map_other) else {
            return false;
        };

        map_self.type_eq_bounded(&map_other, state, env1, env2)
    }
}

impl<'ast> TypeEqBounded<'ast> for UnifRecordRows<'ast> {
    fn type_eq_bounded(
        &self,
        other: &Self,
        state: &mut State,
        env1: &TermEnv<'ast>,
        env2: &TermEnv<'ast>,
    ) -> bool {
        let map_self: Option<IndexMap<LocIdent, _>> = self
            .iter()
            .map(|item| match item {
                RecordRowsElt::Row(RecordRowF { id, typ: types }) => Some((id, types)),
                _ => None,
            })
            .collect();

        let map_other: Option<IndexMap<LocIdent, _>> = other
            .iter()
            .map(|item| match item {
                RecordRowsElt::Row(RecordRowF { id, typ: types }) => Some((id, types)),
                _ => None,
            })
            .collect();

        let (Some(map_self), Some(map_other)) = (map_self, map_other) else {
            return false;
        };

        map_self.type_eq_bounded(&map_other, state, env1, env2)
    }
}

impl<'ast> TypeEqBounded<'ast> for Annotation<'ast> {
    fn type_eq_bounded(
        &self,
        other: &Self,
        state: &mut State,
        env1: &TermEnv<'ast>,
        env2: &TermEnv<'ast>,
    ) -> bool {
        self.typ.type_eq_bounded(&other.typ, state, env1, env2)
            && self
                .contracts
                .type_eq_bounded(other.contracts, state, env1, env2)
    }
}

impl<'ast> TypeEqBounded<'ast> for record::FieldMetadata<'ast> {
    fn type_eq_bounded(
        &self,
        other: &Self,
        state: &mut State,
        env1: &TermEnv<'ast>,
        env2: &TermEnv<'ast>,
    ) -> bool {
        self.annotation
            .type_eq_bounded(&other.annotation, state, env1, env2)
            && self.opt == other.opt
            && self.not_exported == other.not_exported
            && self.priority == other.priority
    }
}

impl<'ast> TypeEqBounded<'ast> for record::FieldPathElem<'ast> {
    fn type_eq_bounded(
        &self,
        other: &Self,
        _state: &mut State,
        _env1: &TermEnv<'ast>,
        _env2: &TermEnv<'ast>,
    ) -> bool {
        // For now, we don't even try to compare interpolated expressions at all, and only compare
        // static field definitions.
        match (self.try_as_ident(), other.try_as_ident()) {
            (Some(id1), Some(id2)) => id1 == id2,
            _ => false,
        }
    }
}

impl<'ast> TypeEqBounded<'ast> for record::Record<'ast> {
    fn type_eq_bounded(
        &self,
        other: &Self,
        state: &mut State,
        env1: &TermEnv<'ast>,
        env2: &TermEnv<'ast>,
    ) -> bool {
        use crate::identifier::FastOrdIdent;

        // We sort the field definitions based on their path. For dynamic fields, we don't have a
        // good ordering (we could derive it, albeit it would be a bit artificial), so we just
        // ignore this part - it might lead to equate a bit less than we could, in presence of
        // dynamic fields on both side that are in different order, but this is at least sound.
        fn sort_field_defs<'ast>(field_defs: &mut Vec<&'ast FieldDef<'ast>>) {
            field_defs.sort_by_cached_key(|field| -> Vec<Option<FastOrdIdent>> {
                field
                    .path
                    .iter()
                    .map(|path_elem| {
                        path_elem
                            .try_as_ident()
                            .as_ref()
                            .map(LocIdent::ident)
                            .map(FastOrdIdent)
                    })
                    .collect()
            });
        }

        let mut sorted_self: Vec<_> = self.field_defs.iter().collect();
        let mut sorted_other: Vec<_> = other.field_defs.iter().collect();
        sort_field_defs(&mut sorted_self);
        sort_field_defs(&mut sorted_other);

        sorted_self
            .as_slice()
            .type_eq_bounded(sorted_other.as_slice(), state, env1, env2)
            && self.open == other.open
    }
}

impl<'ast> TypeEqBounded<'ast> for FieldDef<'ast> {
    /// Check for contract equality between record fields. Fields are equal if they are both without a
    /// definition, or are both defined and their values are equal.
    ///
    /// The attached metadata must be equal as well: most record contracts are written as field with
    /// metadata but without definition. For example, take `{ foo | {bar | Number}}` and `{foo | {bar |
    /// String}}`. Those two record contracts are obviously not equal, but to know that, we have to
    /// look at the contracts of each bar field.
    fn type_eq_bounded(
        &self,
        other: &Self,
        state: &mut State,
        env1: &TermEnv<'ast>,
        env2: &TermEnv<'ast>,
    ) -> bool {
        self.metadata
            .type_eq_bounded(&other.metadata, state, env1, env2)
            && self.path.type_eq_bounded(other.path, state, env1, env2)
            && self.value.type_eq_bounded(&other.value, state, env1, env2)
    }
}

impl<'ast> TypeEqBounded<'ast> for UnifType<'ast> {
    /// This function is used internally by the implementation of [TypeEq] for
    /// [crate::bytecode::ast::typ::Type], but it makes a number of assumptions and isn't supposed
    /// to be called from the outside. And indeed, computing type equality recursively on unifiable
    /// type doesn't make a lot of sense: we can unify them instead.
    ///
    /// For example, we expect to never meet unification variables, and that all the rigid type
    /// variables encountered have been introduced by `type_eq_bounded` itself. This is why we don't
    /// need unique identifiers that are distinct from the one used during typechecking, and we can
    /// just start from `0` when creating a new [State].
    fn type_eq_bounded(
        &self,
        other: &Self,
        state: &mut State,
        env1: &TermEnv<'ast>,
        env2: &TermEnv<'ast>,
    ) -> bool {
        match (self, other) {
            (UnifType::Concrete { typ: s1, .. }, UnifType::Concrete { typ: s2, .. }) => {
                match (s1, s2) {
                    (TypeF::Wildcard(id1), TypeF::Wildcard(id2)) => id1 == id2,
                    (TypeF::Dyn, TypeF::Dyn)
                    | (TypeF::Number, TypeF::Number)
                    | (TypeF::Bool, TypeF::Bool)
                    | (TypeF::Symbol, TypeF::Symbol)
                    | (TypeF::String, TypeF::String) => true,
                    (
                        TypeF::Dict {
                            type_fields: uty1,
                            flavour: attrs1,
                        },
                        TypeF::Dict {
                            type_fields: uty2,
                            flavour: attrs2,
                        },
                    ) if attrs1 == attrs2 => uty1.type_eq_bounded(uty2, state, env1, env2),
                    (TypeF::Array(uty1), TypeF::Array(uty2)) => {
                        uty1.type_eq_bounded(uty2, state, env1, env2)
                    }
                    (TypeF::Arrow(s1, t1), TypeF::Arrow(s2, t2)) => {
                        s1.type_eq_bounded(s2, state, env1, env2)
                            && t1.type_eq_bounded(t2, state, env1, env2)
                    }
                    (TypeF::Enum(uty1), TypeF::Enum(uty2)) => {
                        uty1.type_eq_bounded(uty2, state, env1, env2)
                    }
                    (TypeF::Record(uty1), TypeF::Record(uty2)) => {
                        uty1.type_eq_bounded(uty2, state, env1, env2)
                    }
                    (TypeF::Contract((t1, env1)), TypeF::Contract((t2, env2))) => {
                        t1.type_eq_bounded(t2, state, env1, env2)
                    }
                    (
                        TypeF::Forall {
                            var: var1,
                            var_kind: var_kind1,
                            body: body1,
                        },
                        TypeF::Forall {
                            var: var2,
                            var_kind: var_kind2,
                            body: body2,
                        },
                    ) => {
                        let cst_id = state.fresh_cst_id();

                        if var_kind1 != var_kind2 {
                            return false;
                        }

                        let body1 = body1.clone();
                        let body2 = body2.clone();

                        let (uty1_subst, uty2_subst) = match var_kind1 {
                            VarKind::Type => (
                                body1.subst(var1, &UnifType::Constant(cst_id)),
                                body2.subst(var2, &UnifType::Constant(cst_id)),
                            ),
                            VarKind::RecordRows { .. } => (
                                body1.subst(var1, &UnifRecordRows::Constant(cst_id)),
                                body2.subst(var2, &UnifRecordRows::Constant(cst_id)),
                            ),
                            VarKind::EnumRows { .. } => (
                                body1.subst(var1, &UnifEnumRows::Constant(cst_id)),
                                body2.subst(var2, &UnifEnumRows::Constant(cst_id)),
                            ),
                        };

                        uty1_subst.type_eq_bounded(&uty2_subst, state, env1, env2)
                    }
                    // We can't compare type variables without knowing what they are instantiated to,
                    // and all type variables should have been substituted at this point, so we bail
                    // out.
                    _ => false,
                }
            }
            (UnifType::UnifVar { id: id1, .. }, UnifType::UnifVar { id: id2, .. }) => {
                debug_assert!(
                false,
                "we shouldn't come across unification variables during type equality computation"
            );
                id1 == id2
            }
            (UnifType::Constant(i1), UnifType::Constant(i2)) => i1 == i2,
            _ => false,
        }
    }
}

/// Derive a [TypeEq] implementation from the internal [TypeEqBounded] implementation. We don't
/// necessarily want to do that for every type that implements [TypeEqBounded], for example the
/// implementation for [UnifType] makes some assumptions that make it unsuited for public usage.
macro_rules! derive_type_eq {
    ($ty:ty) => {
        impl<'ast> TypeEq<'ast> for $ty {
            fn type_eq(&self, other: &Self, env1: &TermEnv<'ast>, env2: &TermEnv<'ast>) -> bool {
                self.type_eq_bounded(other, &mut State::new(), env1, env2)
            }
        }
    };
}

derive_type_eq!(Ast<'ast>);
