//! Representation of Nickel types in the AST.
use super::{Ast, AstAlloc, TermPos};
use crate::{
    identifier::Ident, impl_display_from_bytecode_pretty, traverse::*, typ as mainline_typ,
};
use iter::*;
pub use mainline_typ::{EnumRowF, EnumRowsF, RecordRowF, RecordRowsF, TypeF};

/// The recursive unrolling of a type, that is when we "peel off" the top-level layer to find the actual
/// structure represented by an instantiation of `TypeF`.
pub type TypeUnr<'ast> = TypeF<&'ast Type<'ast>, RecordRows<'ast>, EnumRows<'ast>, &'ast Ast<'ast>>;

/// The recursive unrolling of a enum rows.
pub type EnumRowsUnr<'ast> = EnumRowsF<&'ast Type<'ast>, &'ast EnumRows<'ast>>;

/// The recursive unrolling of record rows.
pub type RecordRowsUnr<'ast> = RecordRowsF<&'ast Type<'ast>, &'ast RecordRows<'ast>>;

/// Concrete, recursive definition for an enum row.
pub type EnumRow<'ast> = EnumRowF<&'ast Type<'ast>>;
/// Concrete, recursive definition for enum rows.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct EnumRows<'ast>(pub EnumRowsUnr<'ast>);
/// Concrete, recursive definition for a record row.
pub type RecordRow<'ast> = RecordRowF<&'ast Type<'ast>>;
#[derive(Clone, PartialEq, Eq, Debug)]
/// Concrete, recursive definition for record rows.
pub struct RecordRows<'ast>(pub RecordRowsUnr<'ast>);

/// Concrete, recursive type for a Nickel type.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Type<'ast> {
    pub typ: TypeUnr<'ast>,
    pub pos: TermPos,
}

impl<'ast> From<TypeUnr<'ast>> for Type<'ast> {
    fn from(typ: TypeUnr<'ast>) -> Self {
        Type {
            typ,
            pos: TermPos::None,
        }
    }
}

impl<'ast> Type<'ast> {
    /// Sets a new position for this type.
    pub fn with_pos(self, pos: TermPos) -> Type<'ast> {
        Type { pos, ..self }
    }

    /// Searches for a [crate::typ::TypeF]. If one is found, returns the term it contains.
    pub fn find_contract(&'ast self) -> Option<&'ast Ast<'ast>> {
        self.find_map(|ty: &'ast Type| match &ty.typ {
            TypeF::Contract(f) => Some(*f),
            _ => None,
        })
    }
}

impl<'ast> TypeUnr<'ast> {
    pub fn spanned(self, pos: TermPos) -> Type<'ast> {
        Type { typ: self, pos }
    }
}

impl<'ast> TraverseAlloc<'ast, Type<'ast>> for Type<'ast> {
    fn traverse<F, E>(
        self,
        alloc: &'ast AstAlloc,
        f: &mut F,
        order: TraverseOrder,
    ) -> Result<Self, E>
    where
        F: FnMut(Type<'ast>) -> Result<Type<'ast>, E>,
    {
        let pre_map = match order {
            TraverseOrder::TopDown => f(self)?,
            TraverseOrder::BottomUp => self,
        };

        // traverse keeps track of state in the FnMut function. try_map_state
        // keeps track of it in a separate state variable. we can pass the
        // former into the latter by treating the function itself as the state
        let typ = pre_map.typ.try_map_state(
            |ty, f| Ok(alloc.alloc(ty.clone().traverse(alloc, f, order)?)),
            |rrows, f| rrows.traverse(alloc, f, order),
            |erows, _| Ok(erows),
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
        &'ast self,
        f: &mut dyn FnMut(&'ast Type<'ast>, &S) -> TraverseControl<S, U>,
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
            | TypeF::Enum(_)
            | TypeF::Wildcard(_) => None,
            TypeF::Contract(ast) => ast.traverse_ref(f, state),
            TypeF::Arrow(t1, t2) => t1
                .traverse_ref(f, state)
                .or_else(|| t2.traverse_ref(f, state)),
            TypeF::Forall { body: t, .. }
            | TypeF::Dict { type_fields: t, .. }
            | TypeF::Array(t) => t.traverse_ref(f, state),
            TypeF::Record(rrows) => rrows.traverse_ref(f, state),
        }
    }
}

impl<'ast> TraverseAlloc<'ast, Ast<'ast>> for Type<'ast> {
    fn traverse<F, E>(
        self,
        alloc: &'ast AstAlloc,
        f: &mut F,
        order: TraverseOrder,
    ) -> Result<Self, E>
    where
        F: FnMut(Ast<'ast>) -> Result<Ast<'ast>, E>,
    {
        self.traverse(
            alloc,
            &mut |ty: Type| match ty.typ {
                TypeF::Contract(t) => t
                    .clone()
                    .traverse(alloc, f, order)
                    .map(|t| Type::from(TypeF::Contract(alloc.alloc(t))).with_pos(ty.pos)),
                _ => Ok(ty),
            },
            order,
        )
    }

    fn traverse_ref<S, U>(
        &'ast self,
        f: &mut dyn FnMut(&'ast Ast<'ast>, &S) -> TraverseControl<S, U>,
        state: &S,
    ) -> Option<U> {
        self.traverse_ref(
            &mut |ty: &'ast Type, s: &S| match &ty.typ {
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

impl<'ast> TraverseAlloc<'ast, Type<'ast>> for RecordRows<'ast> {
    fn traverse<F, E>(
        self,
        alloc: &'ast AstAlloc,
        f: &mut F,
        order: TraverseOrder,
    ) -> Result<RecordRows<'ast>, E>
    where
        F: FnMut(Type<'ast>) -> Result<Type<'ast>, E>,
    {
        // traverse keeps track of state in the FnMut function. try_map_state
        // keeps track of it in a separate state variable. we can pass the
        // former into the latter by treating the function itself as the state
        let rows = self.0.try_map_state(
            |ty, f| Ok(alloc.alloc(ty.clone().traverse(alloc, f, order)?)),
            |rrows, f| Ok(alloc.alloc(rrows.clone().traverse(alloc, f, order)?)),
            f,
        )?;

        Ok(RecordRows(rows))
    }

    fn traverse_ref<S, U>(
        &'ast self,
        f: &mut dyn FnMut(&'ast Type<'ast>, &S) -> TraverseControl<S, U>,
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

impl<'ast> RecordRows<'ast> {
    /// Find a nested binding in a record row type. The nested field is given as a list of
    /// successive fields, that is, as a path. Return `None` if there is no such binding.
    ///
    /// # Example
    ///
    /// - self: `{a : {b : Number }}`
    /// - path: `["a", "b"]`
    /// - result: `Some(Number)`
    pub fn find_path<'a>(&'a self, path: &[Ident]) -> Option<&'a RecordRow<'ast>> {
        let mut curr_rrows = self;

        for (idx, id) in path.iter().enumerate() {
            let next_rrows = curr_rrows.iter().find_map(|item| match item {
                RecordRowsItem::Row(row) if row.id.ident() == *id => Some(row),
                _ => None,
            });

            if idx == path.len() - 1 {
                return next_rrows;
            }

            match next_rrows.map(|row| &row.typ.typ) {
                Some(TypeF::Record(rrows)) => curr_rrows = rrows,
                _ => return None,
            }
        }

        None
    }

    /// Find the row with the given identifier in the record type. Return `None` if there is no such
    /// row.
    ///
    /// Equivalent to `find_path(&[id])`.
    pub fn find_row(&self, id: Ident) -> Option<&RecordRow<'ast>> {
        self.find_path(&[id])
    }

    pub fn iter(&self) -> RecordRowsIter<Type<'ast>, RecordRows<'ast>> {
        RecordRowsIter {
            rrows: Some(self),
            ty: std::marker::PhantomData,
        }
    }
}

impl<'ast> EnumRows<'ast> {
    /// Find the row with the given identifier in the enum type. Return `None` if there is no such
    /// row.
    pub fn find_row<'a>(&'a self, id: Ident) -> Option<&'a EnumRow<'ast>> {
        self.iter().find_map(|row_item| match row_item {
            EnumRowsItem::Row(row) if row.id.ident() == id => Some(row),
            _ => None,
        })
    }

    pub fn iter(&self) -> EnumRowsIter<Type<'ast>, EnumRows<'ast>> {
        EnumRowsIter {
            erows: Some(self),
            ty: std::marker::PhantomData,
        }
    }
}

impl_display_from_bytecode_pretty!(Type<'_>);
impl_display_from_bytecode_pretty!(EnumRow<'_>);
impl_display_from_bytecode_pretty!(EnumRows<'_>);
impl_display_from_bytecode_pretty!(RecordRow<'_>);
impl_display_from_bytecode_pretty!(RecordRows<'_>);

pub mod iter {
    use super::*;
    use crate::identifier::LocIdent;

    /// An iterator over the rows of a record type.
    pub struct RecordRowsIter<'a, Ty, RRows> {
        pub(crate) rrows: Option<&'a RRows>,
        pub(crate) ty: std::marker::PhantomData<Ty>,
    }

    /// The item produced by an iterator over record rows.
    pub enum RecordRowsItem<'a, Ty> {
        TailDyn,
        TailVar(&'a LocIdent),
        Row(&'a RecordRowF<Ty>),
    }

    impl<'a, 'ast> Iterator for RecordRowsIter<'a, Type<'ast>, RecordRows<'ast>> {
        type Item = RecordRowsItem<'a, &'ast Type<'ast>>;

        fn next(&mut self) -> Option<Self::Item> {
            self.rrows.and_then(|next| match next.0 {
                RecordRowsF::Empty => {
                    self.rrows = None;
                    None
                }
                RecordRowsF::TailDyn => {
                    self.rrows = None;
                    Some(RecordRowsItem::TailDyn)
                }
                RecordRowsF::TailVar(ref id) => {
                    self.rrows = None;
                    Some(RecordRowsItem::TailVar(id))
                }
                RecordRowsF::Extend { ref row, tail } => {
                    self.rrows = Some(tail);
                    Some(RecordRowsItem::Row(row))
                }
            })
        }
    }

    pub struct EnumRowsIter<'a, Ty, ERows> {
        pub(crate) erows: Option<&'a ERows>,
        pub(crate) ty: std::marker::PhantomData<Ty>,
    }

    pub enum EnumRowsItem<'a, Ty> {
        TailVar(&'a LocIdent),
        Row(&'a EnumRowF<Ty>),
    }

    impl<'a, 'ast> Iterator for EnumRowsIter<'a, Type<'ast>, EnumRows<'ast>> {
        type Item = EnumRowsItem<'a, &'ast Type<'ast>>;

        fn next(&mut self) -> Option<Self::Item> {
            self.erows.and_then(|next| match next.0 {
                EnumRowsF::Empty => {
                    self.erows = None;
                    None
                }
                EnumRowsF::TailVar(ref id) => {
                    self.erows = None;
                    Some(EnumRowsItem::TailVar(id))
                }
                EnumRowsF::Extend { ref row, tail } => {
                    self.erows = Some(tail);
                    Some(EnumRowsItem::Row(row))
                }
            })
        }
    }
}
