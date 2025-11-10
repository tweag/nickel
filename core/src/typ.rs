//! Nickel static types.
//!
//! The type system of Nickel is comprised of primitive types, arrays, records, functions, and
//! opaque types (contracts). This is a structural type system with row polymorphism for both
//! records and enums.
//!
//! ## Record types (rows)
//!
//! A row type for a record is represented as a linked list of pairs `(id, type)` indicating the
//! name and the type of each field. Row-polymorphism means that the tail of this list can be a
//! type variable which can be abstracted over, leaving the row open for future extension. A simple
//! illustration is record field access:
//!
//! ```nickel
//! let f : forall a. { some_field : Number; a} -> Number =
//!   fun record => record.some_field
//! ```
//!
//! The type `{ some_field: Number; a }` indicates that an argument to this function must have at
//! least the field `some_field` of type `Number`, but may contain other fields (or not).
//!
//! ## Dictionaries
//!
//! A dictionary type `{ _ : Type }` represents a record whose fields all have the type `Type`. The
//! count and the name of the fields aren't constrained. Dictionaries can be mapped over, extended,
//! shrinked and accessed in a type-safe manner.
//!
//! # Enum types
//!
//! An enum type is also a row type where each element is a tag, such as `[| 'foo, 'bar, 'baz |]`.
//! This type represent values that can be either `'foo`, `'bar` or `'baz`. Enums support row
//! polymorphism as well.
//!
//! # Contracts
//!
//! To each type corresponds a contract, which is equivalent to a Nickel function which checks at
//! runtime that its argument is of the given type. Contract checks are introduced by a contract
//! annotation or propagated via merging. They ensure sane interaction between typed and untyped
//! parts.
//!
//! Conversely, any Nickel term seen as a contract corresponds to a type, which is opaque and can
//! only be equated with itself.
use crate::{
    environment::Environment,
    error::{EvalErrorKind, ParseError, ParseErrors, TypecheckErrorData},
    eval::value::{Array, NickelValue},
    identifier::{Ident, LocIdent},
    impl_display_from_pretty,
    label::Polarity,
    metrics::increment,
    mk_app, mk_fun,
    position::{PosIdx, PosTable, TermPos},
    pretty::PrettyPrintCap,
    stdlib::internals,
    term::{
        IndexMap, MatchBranch, MatchData, make as mk_term, pattern::compile::Compile,
        record::RecordData,
    },
    traverse::*,
};

use std::{collections::HashSet, convert::Infallible};

pub use nickel_lang_parser::typ::{
    DictTypeFlavour, EnumRowF, EnumRowsF, RecordRowF, RecordRowsF, TypeF, VarKind,
    VarKindDiscriminant,
};

// Concrete, recursive definition of Nickel types from the generic `XxxF` definitions. This is
// "tying" the knot. We have to put `Box` in the appropriate positions (otherwise, Rust will
// complain that the type has an infinite size), but also avoid putting in more than necessary.
//
// For example, `RecordRows` contains a `RecordRow`. The latter doesn't need to be boxed, because a
// `RecordRow` itself potentially contains occurrences of `Type` and `RecordRows`, which need to
// be boxed. Hence, we don't need to additionally box `RecordRow`.

/// Concrete, recursive definition for an enum row.
///
/// This is a newtype just so that we can implement `Display` on it (because
/// `EnumRowF` is from a different crate).
pub struct EnumRow(pub EnumRowF<Box<Type>>);
/// Concrete, recursive definition for enum rows.
#[derive(Clone, PartialEq, Debug)]
pub struct EnumRows(pub EnumRowsF<Box<Type>, Box<EnumRows>>);
/// Concrete, recursive definition for a record row.
///
/// This is a newtype just so that we can implement `Display` on it (because
/// `RecordRowF` is from a different crate).
pub struct RecordRow(pub RecordRowF<Box<Type>>);
#[derive(Clone, PartialEq, Debug)]
/// Concrete, recursive definition for record rows.
pub struct RecordRows(pub RecordRowsF<Box<Type>, Box<RecordRows>>);

/// Concrete, recursive type for a Nickel type.
#[derive(Clone, PartialEq, Debug)]
pub struct Type {
    pub typ: TypeF<Box<Type>, RecordRows, EnumRows, NickelValue>,
    pub pos: TermPos,
}

impl Traverse<Type> for RecordRows {
    fn traverse<F, E>(self, f: &mut F, order: TraverseOrder) -> Result<RecordRows, E>
    where
        F: FnMut(Type) -> Result<Type, E>,
    {
        // traverse keeps track of state in the FnMut function. try_map_state
        // keeps track of it in a separate state variable. we can pass the
        // former into the latter by treating the function itself as the state
        let rows = self.0.try_map_state(
            |ty, f| Ok(Box::new(ty.traverse(f, order)?)),
            |rrows, f| Ok(Box::new(rrows.traverse(f, order)?)),
            f,
        )?;

        Ok(RecordRows(rows))
    }

    fn traverse_ref<S, U>(
        &self,
        f: &mut dyn FnMut(&Type, &S) -> TraverseControl<S, U>,
        state: &S,
    ) -> Option<U> {
        match &self.0 {
            RecordRowsF::Extend { row, tail } => row
                .typ
                .traverse_ref(f, state)
                .or_else(|| tail.traverse_ref(f, state)),
            _ => None,
        }
    }
}

impl Traverse<Type> for EnumRows {
    fn traverse<F, E>(self, f: &mut F, order: TraverseOrder) -> Result<EnumRows, E>
    where
        F: FnMut(Type) -> Result<Type, E>,
    {
        // traverse keeps track of state in the FnMut function. try_map_state
        // keeps track of it in a separate state variable. we can pass the
        // former into the latter by treating the function itself as the state
        let rows = self.0.try_map_state(
            |ty, f| Ok(Box::new(ty.traverse(f, order)?)),
            |rrows, f| Ok(Box::new(rrows.traverse(f, order)?)),
            f,
        )?;

        Ok(EnumRows(rows))
    }

    fn traverse_ref<S, U>(
        &self,
        f: &mut dyn FnMut(&Type, &S) -> TraverseControl<S, U>,
        state: &S,
    ) -> Option<U> {
        match &self.0 {
            EnumRowsF::Extend { row, tail } => row
                .typ
                .as_ref()
                .and_then(|ty| ty.traverse_ref(f, state))
                .or_else(|| tail.traverse_ref(f, state)),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct UnboundTypeVariableError(pub LocIdent);

impl From<UnboundTypeVariableError> for EvalErrorKind {
    fn from(err: UnboundTypeVariableError) -> Self {
        let UnboundTypeVariableError(id) = err;
        let pos = id.pos;
        EvalErrorKind::UnboundIdentifier(id, pos)
    }
}

impl From<UnboundTypeVariableError> for TypecheckErrorData {
    fn from(err: UnboundTypeVariableError) -> Self {
        use crate::{ast::alloc::AstAlloc, error::TypecheckErrorKind};

        TypecheckErrorData::new(AstAlloc::new(), |_alloc| {
            TypecheckErrorKind::UnboundTypeVariable(err.0)
        })
    }
}

impl From<UnboundTypeVariableError> for ParseError {
    fn from(err: UnboundTypeVariableError) -> Self {
        ParseError::UnboundTypeVariables(vec![err.0])
    }
}

impl From<UnboundTypeVariableError> for ParseErrors {
    fn from(err: UnboundTypeVariableError) -> Self {
        ParseErrors::from(ParseError::from(err))
    }
}

pub struct RecordRowsIterator<'a, Ty, RRows> {
    pub(crate) rrows: Option<&'a RRows>,
    pub(crate) ty: std::marker::PhantomData<Ty>,
}

pub enum RecordRowsIteratorItem<'a, Ty> {
    TailDyn,
    TailVar(&'a LocIdent),
    Row(RecordRowF<&'a Ty>),
}

impl<'a> Iterator for RecordRowsIterator<'a, Type, RecordRows> {
    type Item = RecordRowsIteratorItem<'a, Type>;

    fn next(&mut self) -> Option<Self::Item> {
        self.rrows.and_then(|next| match next.0 {
            RecordRowsF::Empty => {
                self.rrows = None;
                None
            }
            RecordRowsF::TailDyn => {
                self.rrows = None;
                Some(RecordRowsIteratorItem::TailDyn)
            }
            RecordRowsF::TailVar(ref id) => {
                self.rrows = None;
                Some(RecordRowsIteratorItem::TailVar(id))
            }
            RecordRowsF::Extend { ref row, ref tail } => {
                self.rrows = Some(tail);
                Some(RecordRowsIteratorItem::Row(RecordRowF {
                    id: row.id,
                    typ: row.typ.as_ref(),
                }))
            }
        })
    }
}

pub struct EnumRowsIterator<'a, Ty, ERows> {
    pub(crate) erows: Option<&'a ERows>,
    pub(crate) ty: std::marker::PhantomData<Ty>,
}

pub enum EnumRowsIteratorItem<'a, Ty> {
    TailVar(&'a LocIdent),
    Row(EnumRowF<&'a Ty>),
}

impl<'a> Iterator for EnumRowsIterator<'a, Type, EnumRows> {
    type Item = EnumRowsIteratorItem<'a, Type>;

    fn next(&mut self) -> Option<Self::Item> {
        self.erows.and_then(|next| match next.0 {
            EnumRowsF::Empty => {
                self.erows = None;
                None
            }
            EnumRowsF::TailVar(ref id) => {
                self.erows = None;
                Some(EnumRowsIteratorItem::TailVar(id))
            }
            EnumRowsF::Extend { ref row, ref tail } => {
                self.erows = Some(tail);
                Some(EnumRowsIteratorItem::Row(EnumRowF {
                    id: row.id,
                    typ: row.typ.as_ref().map(AsRef::as_ref),
                }))
            }
        })
    }
}

trait Subcontract {
    /// Return the contract corresponding to a type component of a larger type.
    ///
    /// # Arguments
    ///
    /// - `pos_table` is the table to be used to convert position index to actual positions, or to
    ///   allocate new ones, in the generated contracts.
    /// - `vars` is an environment mapping type variables to contracts. Type variables are
    ///   introduced locally when opening a `forall`. Note that we don't need to keep separate
    ///   environments for different kind of type variables, as by shadowing, one name can only
    ///   refer to one type of variable at any given time.
    /// - `pol` is the current polarity, which is toggled when generating a contract for the
    ///   argument of an arrow type (see [`crate::label::Label`]).
    /// - `sy` is a counter used to generate fresh symbols for `forall` contracts (see
    ///   [`crate::term::Term::Sealed`]).
    fn subcontract(
        &self,
        pos_table: &mut PosTable,
        vars: Environment<Ident, NickelValue>,
        pol: Polarity,
        sy: &mut i32,
    ) -> Result<NickelValue, UnboundTypeVariableError>;
}

/// Retrieve the contract corresponding to a type variable occurrence in a type as a `NickelValue`.
/// Helper used by the `subcontract` functions.
fn get_var_contract(
    vars: &Environment<Ident, NickelValue>,
    sym: Ident,
    pos: TermPos,
) -> Result<NickelValue, UnboundTypeVariableError> {
    Ok(vars
        .get(&sym)
        .ok_or(UnboundTypeVariableError(LocIdent::from(sym).with_pos(pos)))?
        .clone())
}

impl Subcontract for Type {
    fn subcontract(
        &self,
        pos_table: &mut PosTable,
        mut vars: Environment<Ident, NickelValue>,
        pol: Polarity,
        sy: &mut i32,
    ) -> Result<NickelValue, UnboundTypeVariableError> {
        let ctr = match self.typ {
            TypeF::Dyn => internals::dynamic(),
            TypeF::Number => internals::num(),
            TypeF::Bool => internals::bool(),
            TypeF::String => internals::string(),
            TypeF::ForeignId => internals::foreign_id(),
            // Array Dyn is specialized to array_dyn, which is constant time
            TypeF::Array(ref ty) if matches!(ty.typ, TypeF::Dyn) => internals::array_dyn(),
            TypeF::Array(ref ty) => mk_app!(
                internals::array(),
                ty.subcontract(pos_table, vars, pol, sy)?
            ),
            TypeF::Symbol => panic!("unexpected Symbol type during contract elaboration"),
            // Similarly, any variant of `A -> B` where either `A` or `B` is `Dyn` get specialized
            // to the corresponding builtin contract.
            TypeF::Arrow(ref s, ref t) if matches!((&s.typ, &t.typ), (TypeF::Dyn, TypeF::Dyn)) => {
                internals::func_dyn()
            }
            TypeF::Arrow(ref s, ref t) if matches!(s.typ, TypeF::Dyn) => {
                mk_app!(
                    internals::func_codom(),
                    t.subcontract(pos_table, vars, pol, sy)?
                )
            }
            TypeF::Arrow(ref s, ref t) if matches!(t.typ, TypeF::Dyn) => {
                mk_app!(
                    internals::func_dom(),
                    s.subcontract(pos_table, vars.clone(), pol.flip(), sy)?
                )
            }
            TypeF::Arrow(ref s, ref t) => mk_app!(
                internals::func(),
                s.subcontract(pos_table, vars.clone(), pol.flip(), sy)?,
                t.subcontract(pos_table, vars, pol, sy)?
            ),
            // Note that we do an early return here.
            //
            // All builtin contracts needs an additional wrapping as a `CustomContract`: they're
            // written as a custom contract but they miss the `%contract/custom%` (mostly because
            // it's nicer to do it just once at the end than littering the internals module with
            // `%contract/custom%` applications).
            //
            // However, in the case of a contract embedded in a type, we don't want to do the
            // additional wrapping, as `t` should already be a fully constructed contract.
            TypeF::Contract(ref t) => return Ok(t.clone()),
            TypeF::Var(id) => get_var_contract(&vars, id, self.pos)?,
            TypeF::Forall {
                ref var,
                ref body,
                ref var_kind,
            } => {
                let sealing_key = NickelValue::sealing_key_posless(*sy);
                let contract = match var_kind {
                    VarKind::Type => mk_app!(internals::forall_var(), sealing_key.clone()),
                    // For now, the enum contract doesn't enforce parametricity: see the
                    // implementation of `forall_enum_tail` inside the internal module for more
                    // details.
                    VarKind::EnumRows { .. } => internals::forall_enum_tail(),
                    VarKind::RecordRows { excluded } => {
                        let excluded_ncl: NickelValue = NickelValue::array_posless(
                            Array::from_iter(
                                excluded
                                    .iter()
                                    .map(|id| NickelValue::string_posless(id.into_label())),
                            ),
                            Vec::new(),
                        );

                        mk_app!(
                            internals::forall_record_tail(),
                            sealing_key.clone(),
                            excluded_ncl
                        )
                    }
                };
                vars.insert(var.ident(), contract);

                *sy += 1;
                mk_app!(
                    internals::forall(),
                    sealing_key,
                    NickelValue::from(pol),
                    body.subcontract(pos_table, vars, pol, sy)?
                )
            }
            TypeF::Enum(ref erows) => erows.subcontract(pos_table, vars, pol, sy)?,
            TypeF::Record(ref rrows) => rrows.subcontract(pos_table, vars, pol, sy)?,
            // `{_: Dyn}` and `{_ | Dyn}` are equivalent, and both specialied to the constant-time
            // `dict_dyn`.
            TypeF::Dict {
                ref type_fields,
                flavour: _,
            } if matches!(type_fields.typ, TypeF::Dyn) => internals::dict_dyn(),
            TypeF::Dict {
                ref type_fields,
                flavour: DictTypeFlavour::Contract,
            } => {
                mk_app!(
                    internals::dict_contract(),
                    type_fields.subcontract(pos_table, vars, pol, sy)?
                )
            }
            TypeF::Dict {
                ref type_fields,
                flavour: DictTypeFlavour::Type,
            } => {
                mk_app!(
                    internals::dict_type(),
                    type_fields.subcontract(pos_table, vars, pol, sy)?
                )
            }
            TypeF::Wildcard(_) => internals::dynamic(),
        };

        Ok(mk_term::custom_contract(ctr))
    }
}

impl EnumRows {
    /// Find the row with the given identifier in the enum type. Return `None` if there is no such
    /// row.
    pub fn find_row(&self, id: Ident) -> Option<EnumRow> {
        self.iter().find_map(|row_item| match row_item {
            EnumRowsIteratorItem::Row(row) if row.id.ident() == id => Some(EnumRow(EnumRowF {
                id: row.id,
                typ: row.typ.cloned().map(Box::new),
            })),
            _ => None,
        })
    }

    pub fn iter(&self) -> EnumRowsIterator<Type, EnumRows> {
        EnumRowsIterator {
            erows: Some(self),
            ty: std::marker::PhantomData,
        }
    }

    /// Simplify enum rows for contract generation when the rows are part of a static type
    /// annotation. See [Type::contract_static].
    ///
    /// The following simplification are applied:
    ///
    /// - the type of the argument of each enum variant is simplified as well
    /// - if the polarity is positive and the rows are composed entirely of enum tags and enum
    ///   variants whose argument's simplified type is `Dyn`, the entire rows are elided by returning
    ///   `None`
    /// - a tail variable in tail position is currently left unchanged, because it doesn't give
    ///   rise to any sealing at runtime currently (see documentation of `$forall_enum_tail` in the
    ///   internals module of the stdlib)
    fn simplify(
        self,
        contract_env: &mut Environment<Ident, NickelValue>,
        simplify_vars: SimplifyVars,
        polarity: Polarity,
    ) -> Option<Self> {
        // Actual simplification logic. We can elide the initial enum row type if **each**
        // individual row can be elided and this enum has an empty tail. Thus, when making
        // recursive calls, we can't just return `None` when some subrows can be elided, as we
        // might still need them. Instead, `do_simplify` returns a tuple of the simplified rows and
        // a boolean indicating if this part can be elided.
        fn do_simplify(
            rows: EnumRows,
            contract_env: &mut Environment<Ident, NickelValue>,
            simplify_vars: SimplifyVars,
            polarity: Polarity,
        ) -> (EnumRows, bool) {
            match rows {
                EnumRows(EnumRowsF::Empty) => (EnumRows(EnumRowsF::Empty), true),
                // Currently, we could actually return `true` here, because the tail contract for
                // enums doesn't do anything (see documentation of `$forall_enum_tail` in the
                // `internals` module of the stdlib). However, if we ever fix this shortcoming of
                // enum tail contracts and actually enforce parametricity, returning `true` would
                // become incorrect. So let's be future proof.
                EnumRows(EnumRowsF::TailVar(id)) => (EnumRows(EnumRowsF::TailVar(id)), false),
                EnumRows(EnumRowsF::Extend { row, tail }) => {
                    let (tail, mut elide) =
                        do_simplify(*tail, contract_env, simplify_vars.clone(), polarity);

                    let typ = row.typ.map(|mut typ| {
                        *typ = typ.simplify(contract_env, simplify_vars, polarity);
                        elide = elide && matches!(typ.typ, TypeF::Dyn);
                        typ
                    });

                    let row = EnumRowF { id: row.id, typ };

                    (
                        EnumRows(EnumRowsF::Extend {
                            row,
                            tail: Box::new(tail),
                        }),
                        elide,
                    )
                }
            }
        }

        let (result, elide) = do_simplify(self, contract_env, simplify_vars, polarity);

        if matches!((elide, polarity), (true, Polarity::Positive)) {
            None
        } else {
            Some(result)
        }
    }
}

impl Subcontract for EnumRows {
    fn subcontract(
        &self,
        pos_table: &mut PosTable,
        vars: Environment<Ident, NickelValue>,
        pol: Polarity,
        sy: &mut i32,
    ) -> Result<NickelValue, UnboundTypeVariableError> {
        use crate::term::{
            BinaryOp,
            pattern::{EnumPattern, Pattern, PatternData},
        };

        let mut branches = Vec::new();
        let mut tail_var = None;

        let value_arg = LocIdent::fresh();
        let label_arg = LocIdent::fresh();
        // We don't need to generate a different fresh variable for each match branch, as they have
        // their own scope, so we use the same name instead.
        let variant_arg = LocIdent::fresh();

        // We build a match where each row corresponds to a branch, such that:
        //
        // - if the row is a simple enum tag, we just return the original contract argument
        // - if the row is an enum variant, we extract the argument and apply the corresponding
        //   contract to it
        //
        // For the default branch, depending on the tail:
        //
        // - if the tail is an enum type variable, we perform the required sealing/unsealing
        // - otherwise, if the enum type is closed, we add a default case which blames
        //
        // For example, for an enum type [| 'foo, 'bar, 'Baz T |], the function looks like:
        //
        // ```
        // fun label value =>
        //   value |> match {
        //     'foo => 'Ok x,
        //     'bar => 'Ok x,
        //     'Baz variant_arg => 'Ok ('Baz (%apply_contract% T label_arg variant_arg)),
        //     _ => $enum_fail l
        //   }
        // ```
        for row in self.iter() {
            match row {
                EnumRowsIteratorItem::Row(row) => {
                    let arg_pattern = row.typ.as_ref().map(|_| {
                        Box::new(Pattern {
                            data: PatternData::Any(variant_arg),
                            alias: None,
                            pos: PosIdx::NONE,
                        })
                    });

                    let body = if let Some(ty) = row.typ.as_ref() {
                        // 'Tag (%apply_contract% T label_arg variant_arg)
                        let arg = mk_app!(
                            mk_term::op2(
                                BinaryOp::ContractApply,
                                ty.subcontract(pos_table, vars.clone(), pol, sy)?,
                                mk_term::var(label_arg)
                            ),
                            mk_term::var(variant_arg)
                        );

                        mk_term::enum_variant(row.id, arg)
                    } else {
                        mk_term::var(value_arg)
                    };

                    let body = mk_term::enum_variant("Ok", body);

                    let pattern = Pattern {
                        data: PatternData::Enum(EnumPattern {
                            tag: row.id,
                            pattern: arg_pattern,
                            pos: pos_table.push(row.id.pos),
                        }),
                        alias: None,
                        pos: pos_table.push(row.id.pos),
                    };

                    branches.push(MatchBranch {
                        pattern,
                        guard: None,
                        body,
                    });
                }
                EnumRowsIteratorItem::TailVar(var) => {
                    tail_var = Some(var);
                }
            }
        }

        let (default, default_pos) = if let Some(var) = tail_var {
            (
                mk_app!(
                    mk_term::op2(
                        BinaryOp::ContractApply,
                        get_var_contract(&vars, var.ident(), var.pos)?,
                        mk_term::var(label_arg)
                    ),
                    mk_term::var(value_arg)
                ),
                var.pos,
            )
        } else {
            (
                mk_app!(internals::enum_fail(), mk_term::var(label_arg)),
                TermPos::None,
            )
        };

        branches.push(MatchBranch {
            pattern: Pattern {
                data: PatternData::Wildcard,
                alias: None,
                pos: pos_table.push(default_pos),
            },
            guard: None,
            body: default,
        });

        // We pre-compile the match expression, so that it's not compiled again and again at each
        // application of the contract.
        let match_expr =
            MatchData { branches }.compile(pos_table, mk_term::var(value_arg), PosIdx::NONE);

        let case = mk_fun!(label_arg, value_arg, match_expr);
        Ok(mk_app!(internals::enumeration(), case))
    }
}

impl RecordRows {
    /// Find a nested binding in a record row type. The nested field is given as a list of
    /// successive fields, that is, as a path. Return `None` if there is no such binding.
    ///
    /// # Example
    ///
    /// - self: ` {a : {b : Number }}`
    /// - path: `["a", "b"]`
    /// - result: `Some(Number)`
    pub fn find_path(&self, path: &[Ident]) -> Option<RecordRow> {
        if path.is_empty() {
            return None;
        }

        // While going through the record rows, we use this helper for recursion instead of
        // `find_path`, to avoid cloning a lot of intermediate rows, and rather only clone the
        // final one to return.
        fn find_path_ref<'a>(
            rrows: &'a RecordRows,
            path: &[Ident],
        ) -> Option<RecordRowF<&'a Type>> {
            let next = rrows.iter().find_map(|item| match item {
                RecordRowsIteratorItem::Row(row) if row.id.ident() == path[0] => Some(row.clone()),
                _ => None,
            });

            if path.len() == 1 {
                next
            } else {
                match next.map(|row| &row.typ.typ) {
                    Some(TypeF::Record(rrows)) => find_path_ref(rrows, &path[1..]),
                    _ => None,
                }
            }
        }

        find_path_ref(self, path).map(|row| {
            RecordRow(RecordRowF {
                id: row.id,
                typ: Box::new(row.typ.clone()),
            })
        })
    }

    /// Find the row with the given identifier in the record type. Return `None` if there is no such
    /// row.
    ///
    /// Equivalent to `find_path(&[id])`.
    pub fn find_row(&self, id: Ident) -> Option<RecordRow> {
        self.find_path(&[id])
    }

    pub fn iter(&self) -> RecordRowsIterator<Type, RecordRows> {
        RecordRowsIterator {
            rrows: Some(self),
            ty: std::marker::PhantomData,
        }
    }

    /// Simplify record rows for contract generation when the rows are part of a static type
    /// annotation. See [Type::contract_static].
    ///
    /// The following simplifications are applied:
    ///
    /// - the type of each field is simplified
    /// - if the polarity is positive and the tail is known to be simplified to `Dyn`, any field
    ///   whose simplified type is `Dyn` is entirely elided from the final result. That is, `{foo :
    ///   Number, bar : Number -> Number}` in positive position is simplified to `{bar : Number ->
    ///   Dyn; Dyn}`. `{foo : Number, bar : Dyn}` is simplified to `{; Dyn}` which is simplified
    ///   further to `Dyn` by [Type::simplify].
    ///
    ///   We can't do that when the tail can't be simplified to `Dyn` (this is the case when the
    ///   tail variable has been introduced by a forall in negative position).
    /// - The case of a tail variable is a bit more complex and is detailed below.
    ///
    /// # Simplification of tail variable
    ///
    /// The following paragraphs distinguish between a tail variable (and thus an enclosing record
    /// type) in positive position and a tail variable in negative position (the polarity of the
    /// introducing forall is yet another dimension, covered in each paragraph).
    ///
    /// ## Negative polarity
    ///
    /// The simplification of record row variables is made harder by the presence of excluded
    /// fields. It's tempting to replace all `r` in tail position by `Dyn` if the introducing
    /// `forall r` was in positive position, as we do for regular type variables, but it's
    /// unfortunately not sound. For example:
    ///
    /// ```nickel
    /// inject_meta: forall r. {; r} -> {meta : String; r}
    /// ```
    ///
    /// In this case, the `forall r` is in positive position, but the `r` in `{; r}` in negative
    /// position can't be entirely elided, because it checks that `meta` isn't already present in
    /// the original argument. In particular, the original contract rejects `inject_meta {meta =
    /// "hello"}`, but blindly simplifying the type of `inject_meta` to `{; Dyn} -> {; Dyn}`
    /// would allow this argument.
    ///
    /// We can simplify it to something like `{; $forall_record_tail_excluded_only ["meta"]} -> {;
    /// Dyn}` (further simplified to `{; $forall_record_tail_excluded_only ["meta"]} -> Dyn` by
    /// `Type::simplify`). `$forall_record_tail_excluded_only` is a specialized version of the
    /// record tail contract `$forall_record_tail$ which doesn't seal but still check for the
    /// absence of excluded fields.
    ///
    /// Note that we can't actually put an arbitrary contract in the tail of a record type. This is
    /// why the `simplify()` methods take a mutable reference to an environment `contract_env`
    /// which will be passed to the contract generation methods. What we do in practice is to bind
    /// a fresh record row variable to `$forall_record_tail_excluded_only ["meta"]` in this
    /// environment and we substitute the tail var for this fresh variable.
    ///
    /// On the other hand, in
    ///
    /// ```nickel
    /// update_meta: forall r. {meta : String; r} -> {meta : String; r}
    /// ```
    ///
    /// The contract can be simplified to `{meta : String; Dyn} -> Dyn`. To detect this case, we
    /// record the set of fields that are present in the current rows and check if this set is
    /// equal to the excluded fields for this row variable (it's always a subset of the excluded
    /// fields by definition of row constraints), or if there are some excluded fields left to
    /// check.
    ///
    /// ## Positive polarity
    ///
    /// In a positive position, we can replace the tail with `Dyn` if the tail isn't a variable
    /// introduced in negative position, because a typed term can never violate row constraints.
    ///
    /// For a variable introduced by a forall in negative position, we need to keep the unsealing
    /// operation to make sure the corresponding forall type doesn't blame. But the unsealing
    /// operation needs prior sealing to work out, so we need to keep the tail even for record
    /// types in positive position. What's more, _we can't elide any field in this case_. Indeed,
    /// eliding fields can change what goes in the tail, and can only be done when the tail is
    /// ensured to be `Dyn`.
    fn simplify(
        self,
        contract_env: &mut Environment<Ident, NickelValue>,
        simplify_vars: SimplifyVars,
        polarity: Polarity,
    ) -> Self {
        // This helper does a first traversal of record rows in order to peek the tail and check if
        // it's a variable introduced by a forall in negative position. In this case, the second
        // component of the result will be `false` and we can't elide any field during the actual
        // simplification.
        //
        // As we will need the set of all fields later, we build during this first traversal as well.
        fn peek_tail(rrows: &RecordRows, simplify_vars: &SimplifyVars) -> (HashSet<Ident>, bool) {
            let mut fields = HashSet::new();
            let mut can_elide = true;

            for row in rrows.iter() {
                match row {
                    RecordRowsIteratorItem::Row(row) => {
                        fields.insert(row.id.ident());
                    }
                    RecordRowsIteratorItem::TailVar(id)
                        if simplify_vars.rrows_vars_elide.get(&id.ident()).is_none() =>
                    {
                        can_elide = false;
                    }
                    _ => {}
                }
            }

            (fields, can_elide)
        }

        // Actual simplification logic, which additionally remembers if a field has been elided and
        // if we thus need to change the tail to `Dyn` to account for the elision.
        fn do_simplify(
            rrows: RecordRows,
            contract_env: &mut Environment<Ident, NickelValue>,
            simplify_vars: SimplifyVars,
            polarity: Polarity,
            fields: &HashSet<Ident>,
            can_elide: bool,
        ) -> RecordRows {
            let rrows = match rrows.0 {
                // Because we may have elided some fields, we always make this record type open
                // when in positive position, which never hurts
                RecordRowsF::Empty if matches!(polarity, Polarity::Positive) => {
                    RecordRowsF::TailDyn
                }
                RecordRowsF::Empty => RecordRowsF::Empty,
                RecordRowsF::Extend { row, tail } => {
                    let typ_simplified =
                        row.typ
                            .simplify(contract_env, simplify_vars.clone(), polarity);
                    // If we are in a positive position, the simplified type of the field is `Dyn`,
                    // and we can elide fields given the current tail of these rows, we get rid of
                    // the whole field.
                    let elide_field = matches!(
                        (&typ_simplified.typ, polarity),
                        (TypeF::Dyn, Polarity::Positive)
                    ) && can_elide;

                    let row = RecordRowF {
                        id: row.id,
                        typ: Box::new(typ_simplified),
                    };

                    let tail = Box::new(do_simplify(
                        *tail,
                        contract_env,
                        simplify_vars,
                        polarity,
                        fields,
                        can_elide,
                    ));

                    if elide_field {
                        tail.0
                    } else {
                        RecordRowsF::Extend { row, tail }
                    }
                }
                RecordRowsF::TailDyn => RecordRowsF::TailDyn,
                RecordRowsF::TailVar(id) => {
                    let excluded = simplify_vars.rrows_vars_elide.get(&id.ident());

                    match (excluded, polarity) {
                        // If the variable was introduced in positive position, it won't cause any
                        // (un)sealing. So if we are in positive position as well, we can just get
                        // rid of the tail
                        (Some(_), Polarity::Positive) => RecordRowsF::TailDyn,
                        // If the variable was introduced in negative position, we can't elide the
                        // unsealing contracts which are in negative position as well, and thus
                        // neither elide the corresponding sealing contract.
                        (None, Polarity::Positive | Polarity::Negative) => RecordRowsF::TailVar(id),
                        // If the variable was introduced by a forall in positive position and we
                        // are in a negative position, we can do some simplifying and at least get
                        // rid of the sealing operation, and sometimes of the whole tail contract.
                        (Some(excluded), Polarity::Negative) => {
                            // We don't have to check for the absence of fields that appear in this row
                            // type
                            let excluded = excluded - fields;

                            // If all the excluded fields are listed in the record type, the
                            // contract will never blame on the presence of an excluded field, so
                            // we can get rid of the tail altogether
                            if excluded.is_empty() {
                                RecordRowsF::TailDyn
                            }
                            // Otherwise, we need to check for the absence of the remaining excluded
                            // fields with a specialized tail contract which doesn't seal the tail.
                            else {
                                let fresh_var = LocIdent::fresh();

                                let excluded_ncl =
                                    NickelValue::array_posless(
                                        Array::from_iter(excluded.iter().map(|id| {
                                            NickelValue::string_posless(id.into_label())
                                        })),
                                        Vec::new(),
                                    );

                                contract_env.insert(
                                    fresh_var.ident(),
                                    mk_app!(
                                        internals::forall_record_tail_excluded_only(),
                                        excluded_ncl
                                    ),
                                );

                                RecordRowsF::TailVar(fresh_var)
                            }
                        }
                    }
                }
            };

            RecordRows(rrows)
        }

        let (fields, can_elide) = peek_tail(&self, &simplify_vars);
        do_simplify(
            self,
            contract_env,
            simplify_vars,
            polarity,
            &fields,
            can_elide,
        )
    }
}

impl Subcontract for RecordRows {
    fn subcontract(
        &self,
        pos_table: &mut PosTable,
        vars: Environment<Ident, NickelValue>,
        pol: Polarity,
        sy: &mut i32,
    ) -> Result<NickelValue, UnboundTypeVariableError> {
        // We begin by building a record whose arguments are contracts
        // derived from the types of the statically known fields.
        let mut rrows = self;
        let mut fcs = IndexMap::new();

        while let RecordRowsF::Extend {
            row: RecordRowF { id, typ: ty },
            tail,
        } = &rrows.0
        {
            fcs.insert(*id, ty.subcontract(pos_table, vars.clone(), pol, sy)?);
            rrows = tail
        }

        let has_tail = !matches!(&rrows.0, RecordRowsF::Empty);
        // Now that we've dealt with the row extends, we just need to
        // work out the tail.
        let tail = match &rrows.0 {
            RecordRowsF::Empty => internals::empty_tail(),
            RecordRowsF::TailDyn => internals::dyn_tail(),
            RecordRowsF::TailVar(id) => get_var_contract(&vars, id.ident(), id.pos)?,
            // Safety: the while above excludes that `tail` can have the form `Extend`.
            RecordRowsF::Extend { .. } => unreachable!(),
        };

        let rec = NickelValue::record_posless(RecordData::with_field_values(fcs));

        Ok(mk_app!(
            internals::record_type(),
            rec,
            tail,
            NickelValue::bool_value_posless(has_tail)
        ))
    }
}

impl From<TypeF<Box<Type>, RecordRows, EnumRows, NickelValue>> for Type {
    fn from(typ: TypeF<Box<Type>, RecordRows, EnumRows, NickelValue>) -> Self {
        Type {
            typ,
            pos: TermPos::None,
        }
    }
}

impl Type {
    /// Creates a `Type` with the specified position
    pub fn with_pos(self, pos: TermPos) -> Type {
        Type { pos, ..self }
    }

    /// Returns the same type with the position cleared (set to `None`).
    ///
    /// This is currently only used in test code, but because it's used from integration
    /// tests we cannot hide it behind `#[cfg(test)]`.
    pub fn without_pos(self) -> Type {
        self.traverse(
            &mut |t: Type| {
                Ok::<_, Infallible>(Type {
                    pos: TermPos::None,
                    ..t
                })
            },
            TraverseOrder::BottomUp,
        )
        .unwrap()
        .traverse(
            &mut |val: NickelValue| Ok::<_, Infallible>(val.with_pos_idx(PosIdx::NONE)),
            TraverseOrder::BottomUp,
        )
        .unwrap()
    }

    /// Variant of [Self::contract] that returns the contract corresponding to a type which appears
    /// in a static type annotation. [Self::contract_static] uses the fact that the checked term
    /// has been typechecked to optimize the generated contract thanks to the guarantee of static
    /// typing.
    pub fn contract_static(
        self,
        pos_table: &mut PosTable,
    ) -> Result<NickelValue, UnboundTypeVariableError> {
        let mut sy = 0;
        let mut contract_env = Environment::new();

        self.simplify(&mut contract_env, SimplifyVars::new(), Polarity::Positive)
            .subcontract(pos_table, contract_env, Polarity::Positive, &mut sy)
    }

    /// Return the contract corresponding to a type. Said contract must then be applied using the
    /// `ApplyContract` primitive operation.
    pub fn contract(
        &self,
        pos_table: &mut PosTable,
    ) -> Result<NickelValue, UnboundTypeVariableError> {
        increment!(format!("gen_contract:{}", self.pretty_print_cap(40)));

        let mut sy = 0;

        self.subcontract(pos_table, Environment::new(), Polarity::Positive, &mut sy)
    }

    /// Returns true if this type is a function type (including a polymorphic one), false
    /// otherwise.
    pub fn is_function_type(&self) -> bool {
        match &self.typ {
            TypeF::Forall { body, .. } => body.is_function_type(),
            TypeF::Arrow(..) => true,
            _ => false,
        }
    }

    /// Determine if a type is an atom, that is a either a primitive type (`Dyn`, `Number`, etc.) or
    /// a type delimited by specific markers (such as a row type). Used in formatting to decide if
    /// parentheses need to be inserted during pretty pretting.
    pub fn fmt_is_atom(&self) -> bool {
        match &self.typ {
            TypeF::Dyn
            | TypeF::Number
            | TypeF::Bool
            | TypeF::String
            | TypeF::Var(_)
            | TypeF::Record(_)
            | TypeF::Enum(_) => true,
            TypeF::Contract(ctr) => ctr.fmt_is_atom(),
            _ => false,
        }
    }

    /// Searches for a `TypeF::Contract`. If one is found, returns the term it contains.
    pub fn find_contract(&self) -> Option<NickelValue> {
        self.find_map(|ty: &Type| match &ty.typ {
            TypeF::Contract(f) => Some(f.clone()),
            _ => None,
        })
    }

    /// Static typing guarantees make some of the contract checks useless, assuming that blame
    /// safety holds. This function simplifies `self` for contract generation, assuming it is part
    /// of a static type annotation, by eliding some of these useless subcontracts.
    ///
    /// # Simplifications
    ///
    /// - `forall`s of a type variable in positive positions are removed, and the corresponding type
    ///   variable is substituted for a `Dyn` contract. In consequence, [Self::contract()] will generate
    ///   simplified contracts as well. For example, `forall a. Array a -> a` becomes `Array Dyn -> Dyn`,
    ///   and `Array Dyn` will then be specialized to `$array_dyn` which has a constant-time overhead
    ///   (while `Array a` is linear in the size of the array). The final contract will just check that
    ///   the argument is an array.
    /// - `forall`s of a row variable (either record or enum) are removed as well. The substitution of
    ///   the corresponding row variable is a bit more complex than in the type variable case and depends
    ///   on the situation. See [RecordRows::simplify] and [EnumRows::simplify] for more details.
    /// - `forall`s in negative position are left unchanged.
    /// - All positive occurrences of type constructors that aren't a function type are recursively
    ///   simplified. If they are simplified to a trivial type, such as `{; Dyn}` for a record or `Array
    ///   Dyn` for an array, they are further simplified to `Dyn`.
    ///   are turned to `Dyn` contracts. However, we must be careful here: type constructors might
    ///   have function types somewhere inside, as in `{foo : Number, bar : Array (String ->
    ///   String) }`. In this case, we can elide `foo`, but not `bar`.
    fn simplify(
        self,
        contract_env: &mut Environment<Ident, NickelValue>,
        mut simplify_vars: SimplifyVars,
        polarity: Polarity,
    ) -> Self {
        let mut pos = self.pos;

        let simplified = match self.typ {
            TypeF::Arrow(dom, codom) => TypeF::Arrow(
                Box::new(dom.simplify(contract_env, simplify_vars.clone(), polarity.flip())),
                Box::new(codom.simplify(contract_env, simplify_vars, polarity)),
            ),
            TypeF::Forall {
                var,
                var_kind: VarKind::Type,
                body,
            } if polarity == Polarity::Positive => {
                // The case of type variables is the simplest: we just replace all of them by
                // `Dyn`. We don't bother messing with the contract environment and only register
                // them in `simplify_vars`. Subsequent calls to `simplify` will check the register
                // and replace the variable by `Dyn` when appropriate.
                simplify_vars.type_vars_elide.insert(var.ident(), ());

                let result = body.simplify(contract_env, simplify_vars, polarity);
                // we keep the position of the body, not the one of the forall
                pos = result.pos;
                result.typ
            }
            TypeF::Forall {
                var,
                var_kind: VarKind::RecordRows { excluded },
                body,
            } if polarity == Polarity::Positive => {
                simplify_vars.rrows_vars_elide.insert(var.ident(), excluded);

                let result = body.simplify(contract_env, simplify_vars, polarity);
                // we keep the position of the body, not the one of the forall
                pos = result.pos;
                result.typ
            }
            // For enum rows in negative position, we don't any simplification for now because
            // parametricity isn't enforced. Polymorphic enum row contracts are mostly doing
            // nothing, and it's fine to keep them as they are. See the documentation of
            // `$forall_enum_tail` for more details. Even if we do enforce parametricity in the
            // future, we will be missing an optimization opportunity here but we won't introduce
            // any unsoundness.
            //
            // We don't elide foralls in negative position either.
            //
            // In both cases, we still simplify the body of the forall.
            TypeF::Forall {
                var,
                var_kind,
                body,
            } => TypeF::Forall {
                var,
                var_kind,
                body: Box::new(body.simplify(contract_env, simplify_vars, polarity)),
            },
            TypeF::Var(id) if simplify_vars.type_vars_elide.get(&id).is_some() => TypeF::Dyn,
            // Any ground type in positive position can be entirely elided
            TypeF::Number | TypeF::String | TypeF::Bool | TypeF::Symbol | TypeF::ForeignId
                if matches!(polarity, Polarity::Positive) =>
            {
                TypeF::Dyn
            }
            // We need to recurse into type constructors, which could contain arrow types inside.
            TypeF::Record(rrows) => {
                let rrows_simplified = rrows.simplify(contract_env, simplify_vars, polarity);

                // [^simplify-type-constructor-positive]: if we are in positive position, and the
                // record type is entirely elided (which means simplified to `{; Dyn}`) or is
                // empty, we can simplify it further to `Dyn`. This works for other type
                // constructors as well (arrays, dictionaries, etc.): if there's no negative type
                // or polymorphic tail somewhere inside, we can get rid of the whole thing.
                if matches!(
                    (&rrows_simplified, polarity),
                    (
                        RecordRows(RecordRowsF::TailDyn) | RecordRows(RecordRowsF::Empty),
                        Polarity::Positive
                    )
                ) {
                    TypeF::Dyn
                } else {
                    TypeF::Record(rrows_simplified)
                }
            }
            TypeF::Enum(erows) => {
                let erows = erows.simplify(contract_env, simplify_vars, polarity);

                // See [^simplify-type-constructor-positive]
                erows.map(TypeF::Enum).unwrap_or(TypeF::Dyn)
            }
            TypeF::Dict {
                type_fields,
                flavour,
            } => {
                let type_fields =
                    Box::new(type_fields.simplify(contract_env, simplify_vars, polarity));

                // See [^simplify-type-constructor-positive]
                if matches!(
                    (&type_fields.typ, polarity),
                    (TypeF::Dyn, Polarity::Positive)
                ) {
                    TypeF::Dyn
                } else {
                    TypeF::Dict {
                        type_fields,
                        flavour,
                    }
                }
            }
            TypeF::Array(t) => {
                let type_elts = t.simplify(contract_env, simplify_vars, polarity);

                // See [^simplify-type-constructor-positive]
                if matches!((&type_elts.typ, polarity), (TypeF::Dyn, Polarity::Positive)) {
                    TypeF::Dyn
                } else {
                    TypeF::Array(Box::new(type_elts))
                }
            }
            // All the remaining cases are ground types that don't contain subtypes, or they are
            // type variables that can't be elided. We leave them unchanged
            t => t,
        };

        Type {
            typ: simplified,
            pos,
        }
    }
}

impl Traverse<Type> for Type {
    fn traverse<F, E>(self, f: &mut F, order: TraverseOrder) -> Result<Self, E>
    where
        F: FnMut(Type) -> Result<Type, E>,
    {
        let pre_map = match order {
            TraverseOrder::TopDown => f(self)?,
            TraverseOrder::BottomUp => self,
        };

        // traverse keeps track of state in the FnMut function. try_map_state
        // keeps track of it in a separate state variable. we can pass the
        // former into the latter by treating the function itself as the state
        let typ = pre_map.typ.try_map_state(
            |ty, f| Ok(Box::new(ty.traverse(f, order)?)),
            |rrows, f| rrows.traverse(f, order),
            |erows, f| erows.traverse(f, order),
            |ctr, _| Ok(ctr),
            f,
        )?;

        let post_map = Type { typ, ..pre_map };

        match order {
            TraverseOrder::TopDown => Ok(post_map),
            TraverseOrder::BottomUp => f(post_map),
        }
    }

    fn traverse_ref<S, U>(
        &self,
        f: &mut dyn FnMut(&Type, &S) -> TraverseControl<S, U>,
        state: &S,
    ) -> Option<U> {
        let child_state = match f(self, state) {
            TraverseControl::Continue => None,
            TraverseControl::ContinueWithScope(s) => Some(s),
            TraverseControl::SkipBranch => {
                return None;
            }
            TraverseControl::Return(ret) => {
                return Some(ret);
            }
        };
        let state = child_state.as_ref().unwrap_or(state);

        match &self.typ {
            TypeF::Dyn
            | TypeF::Number
            | TypeF::Bool
            | TypeF::String
            | TypeF::ForeignId
            | TypeF::Symbol
            | TypeF::Var(_)
            | TypeF::Wildcard(_) => None,
            TypeF::Contract(rt) => rt.traverse_ref(f, state),
            TypeF::Arrow(t1, t2) => t1
                .traverse_ref(f, state)
                .or_else(|| t2.traverse_ref(f, state)),
            TypeF::Forall { body: t, .. }
            | TypeF::Dict { type_fields: t, .. }
            | TypeF::Array(t) => t.traverse_ref(f, state),
            TypeF::Record(rrows) => rrows.traverse_ref(f, state),
            TypeF::Enum(erows) => erows.traverse_ref(f, state),
        }
    }
}

impl Traverse<NickelValue> for Type {
    fn traverse<F, E>(self, f: &mut F, order: TraverseOrder) -> Result<Self, E>
    where
        F: FnMut(NickelValue) -> Result<NickelValue, E>,
    {
        self.traverse(
            &mut |ty: Type| match ty.typ {
                TypeF::Contract(t) => t
                    .traverse(f, order)
                    .map(|t| Type::from(TypeF::Contract(t)).with_pos(ty.pos)),
                _ => Ok(ty),
            },
            order,
        )
    }

    fn traverse_ref<S, U>(
        &self,
        f: &mut dyn FnMut(&NickelValue, &S) -> TraverseControl<S, U>,
        state: &S,
    ) -> Option<U> {
        self.traverse_ref(
            &mut |ty: &Type, s: &S| match &ty.typ {
                TypeF::Contract(t) => {
                    if let Some(ret) = t.traverse_ref(f, s) {
                        TraverseControl::Return(ret)
                    } else {
                        TraverseControl::SkipBranch
                    }
                }
                _ => TraverseControl::Continue,
            },
            state,
        )
    }
}

/// Some context storing various type variables that can be simplified during static contract
/// simplification (see the various `simplify()` methods).
#[derive(Clone, Debug, Default)]
struct SimplifyVars {
    /// Environment used as a persistent HashSet to record type variables that can be elided
    /// (replaced by `Dyn`) because they were introduced by a forall in positive position.
    type_vars_elide: Environment<Ident, ()>,
    /// Record record rows variables that were introduced by a forall in positive position and
    /// their corresponding `excluded` field. They will be substituted for different simplified
    /// contracts depending on where the variable appears and the surrounding record.
    rrows_vars_elide: Environment<Ident, HashSet<Ident>>,
}

impl SimplifyVars {
    fn new() -> Self {
        Self::default()
    }
}

impl_display_from_pretty!(Type);
impl_display_from_pretty!(EnumRow);
impl_display_from_pretty!(EnumRows);
impl_display_from_pretty!(RecordRow);
impl_display_from_pretty!(RecordRows);

impl PrettyPrintCap for Type {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        parser::{ErrorTolerantParserCompat, grammar::FixedTypeParser, lexer::Lexer},
        position::PosTable,
    };

    /// Parse a type represented as a string.
    fn parse_type(pos_table: &mut PosTable, s: &str) -> Type {
        use crate::files::Files;
        let id = Files::empty().add("<test>", s);

        FixedTypeParser::new()
            .parse_strict_compat(pos_table, id, Lexer::new(s))
            .unwrap()
    }

    /// Parse a type, simplify it and assert that the result corresponds to the second argument
    /// (which is parsed and printed again to eliminate formatting differences). This function also
    /// checks that the contract generation from the simplified version works without an unbound
    /// type variable error.
    #[track_caller]
    fn assert_simplifies_to(orig: &str, target: &str) {
        let mut pos_table = PosTable::new();
        let parsed = parse_type(&mut pos_table, orig);

        parsed.clone().contract_static(&mut pos_table).unwrap();

        let simplified = parsed.simplify(
            &mut Environment::new(),
            SimplifyVars::new(),
            Polarity::Positive,
        );
        let target_typ = parse_type(&mut pos_table, target);

        assert_eq!(format!("{simplified}"), format!("{target_typ}"));
    }

    #[test]
    fn simplify() {
        assert_simplifies_to("forall a. a -> (a -> a) -> a", "Dyn -> (Dyn -> Dyn) -> Dyn");
        assert_simplifies_to(
            "forall b. (forall a. a -> a) -> b",
            "(forall a. a -> a) -> Dyn",
        );

        // Big but entirely positive type
        assert_simplifies_to(
            "{foo : Array {bar : String, baz : Number}, qux: [| 'Foo, 'Bar, 'Baz Dyn |], pweep: {single : Array Bool}}",
            "Dyn",
        );
        // Mixed type with arrows inside the return value
        assert_simplifies_to(
            "Array Number -> Array {foo : Number,  bar : String -> String}",
            "Array Number -> Array {bar: String -> Dyn; Dyn}",
        );
        // Enum with arrows inside
        assert_simplifies_to(
            "{single : Array [| 'Foo, 'Bar, 'Baz (Dyn -> Dyn) |] }",
            "{single : Array [| 'Foo, 'Bar, 'Baz (Dyn -> Dyn) |]; Dyn}",
        );

        // Polymorphic rows, case without excluded fields to check
        assert_simplifies_to(
            "forall r. {meta : String, doc : String; r} -> {meta : String; r}",
            "{meta : String, doc : String; Dyn} -> Dyn",
        );
        // Polymorphic rows in negative position should prevent row elision in positive position
        assert_simplifies_to(
            "(forall r. {meta : String, count: Number; r} -> {; r}) -> {meta : String, count: Number}",
            "(forall r. {meta : Dyn, count : Dyn; r} -> {; r}) -> Dyn",
        );
    }

    #[test]
    fn simplify_dont_elide_record() {
        use regex::Regex;
        // The simplified version of the type should contain a generated variable here. We can't
        // represent a generated variable in normal Nickel syntax, so instead of parsing the
        // expected result and print it again to eliminate formatting details as in
        // `assert_simplifies_to`, we will print the simplified version and do a direct string
        // comparison on the result.
        let orig = "forall r. {x: Number, y: String; r} -> {z: Array Bool; r}";

        let mut contract_env = Environment::new();
        let mut pos_table = PosTable::new();
        let simplified = parse_type(&mut pos_table, orig)
            .simplify(&mut contract_env, SimplifyVars::new(), Polarity::Positive)
            .to_string();

        // Note: because we match on strings directly, we must get the formatting right.
        let expected = Regex::new(r"\{ x : Number, y : String; (%\d+) \} -> Dyn").unwrap();
        let caps = expected.captures(&simplified).unwrap();

        // We should have the generated variable in the contract environment bound to
        // `$forall_enum_tail_excluded_only ["z"]`. Indeed, `x`, `y` and `z` are excluded fields
        // for the row variable `r`. Because `forall r` is in positive position, we remove the
        // forall. `{x: Number, y: String; r}` is in negative position, so we still need to check
        // for excluded fields, but `x` and `y` are already taken care of in the body of the record
        // type. It only remains to check that `z` isn't present in the input record.
        let expected_var = Ident::from(caps.get(1).unwrap().as_str());

        assert_eq!(
            contract_env.get(&expected_var).unwrap().to_string(),
            "$forall_record_tail_excluded_only [ \"z\" ]"
        );
    }
}
