//! Representation of Nickel types in the AST.

use super::{Ast, TermPos};

use crate::typ as mainline_typ;
// Concrete, recursive definition of Nickel types from the generic `XxxF` definitions. This is
// "tying" the knot. We have to put indirections (references, `Box`, etc.) in the appropriate
// positions (otherwise, Rust will complain that the type has an infinite size), but also avoid
// putting in more than necessary, as indirections have a cost.
//
// For example, `RecordRows` contains a `RecordRow`. The latter doesn't need to be boxed, because a
// `RecordRow` itself potentially contains occurrences of `Type` and `RecordRows`, which need to
// be boxed. Hence, we don't need to additionally box `RecordRow`.

pub use mainline_typ::{EnumRowF, EnumRowsF, RecordRowF, RecordRowsF, TypeF};

/// The recursive unrolling of a type, that is when we "peel off" the top-level layer to find the actual
/// structure represented by an instantiation of `TypeF`.
pub type TypeUnr<'ast> = TypeF<&'ast Type<'ast>, RecordRows<'ast>, EnumRows<'ast>, Ast<'ast>>;

/// The recursive unrolling of a enum rows.
pub type EnumRowsUnr<'ast> = EnumRowsF<&'ast Type<'ast>, &'ast EnumRows<'ast>>;

/// The recursive unrolling of record rows.
pub type RecordRowsUnr<'ast> = RecordRowsF<&'ast Type<'ast>, &'ast RecordRows<'ast>>;

/// Concrete, recursive definition for an enum row.
pub type EnumRow<'ast> = EnumRowF<&'ast Type<'ast>>;
/// Concrete, recursive definition for enum rows.
#[derive(Clone, PartialEq, Debug)]
pub struct EnumRows<'ast>(pub EnumRowsUnr<'ast>);
/// Concrete, recursive definition for a record row.
pub type RecordRow<'ast> = RecordRowF<&'ast Type<'ast>>;
#[derive(Clone, PartialEq, Debug)]
/// Concrete, recursive definition for record rows.
pub struct RecordRows<'ast>(pub RecordRowsUnr<'ast>);

/// Concrete, recursive type for a Nickel type.
#[derive(Clone, PartialEq, Debug)]
pub struct Type<'ast> {
    pub typ: TypeUnr<'ast>,
    pub pos: TermPos,
}
