//! Helpers for building `TypeWrapper`s.
use super::{AbsType, TypeWrapper};

/// Multi-ary arrow constructor for types implementing `Into<TypeWrapper>`.
#[macro_export]
macro_rules! mk_tyw_arrow {
    ($left:expr, $right:expr) => {
        $crate::typecheck::TypeWrapper::Concrete(
            $crate::types::AbsType::Arrow(
                Box::new($crate::typecheck::TypeWrapper::from($left)),
                Box::new($crate::typecheck::TypeWrapper::from($right))
            )
        )
    };
    ( $fst:expr, $snd:expr , $( $types:expr ),+ ) => {
        mk_tyw_arrow!($fst, mk_tyw_arrow!($snd, $( $types ),+))
    };
}

/// Multi-ary enum row constructor for types implementing `Into<TypeWrapper>`.
/// `mk_tyw_enum_row!(id1, .., idn; tail)` correspond to `<id1, .., idn | tail>.
#[macro_export]
macro_rules! mk_tyw_enum_row {
    () => {
        $crate::typecheck::TypeWrapper::from(AbsType::RowEmpty())
    };
    (; $tail:expr) => {
        $crate::typecheck::TypeWrapper::from($tail)
    };
    ( $id:expr $(, $ids:expr )* $(; $tail:expr)?) => {
        $crate::typecheck::TypeWrapper::Concrete(
            $crate::types::AbsType::RowExtend(
                Ident::from($id),
                None,
                Box::new(mk_tyw_enum_row!($( $ids ),* $(; $tail)?))
            )
        )
    };
}

/// Multi-ary record row constructor for types implementing `Into<TypeWrapper>`.
/// `mk_tyw_row!((id1, ty1), .., (idn, tyn); tail)` correspond to `{id1: ty1, .., idn: tyn |
/// tail}. The tail can be omitted, in which case the empty row is uses as a tail instead.
#[macro_export]
macro_rules! mk_tyw_row {
    () => {
        $crate::typecheck::TypeWrapper::from(AbsType::RowEmpty())
    };
    (; $tail:expr) => {
        $crate::typecheck::TypeWrapper::from($tail)
    };
    (($id:expr, $ty:expr) $(,($ids:expr, $tys:expr))* $(; $tail:expr)?) => {
        $crate::typecheck::TypeWrapper::Concrete(
            $crate::types::AbsType::RowExtend(
                Ident::from($id),
                Some(Box::new($ty.into())),
                Box::new(mk_tyw_row!($(($ids, $tys)),* $(; $tail)?))
            )
        )
    };
}

/// Wrapper around `mk_tyw_enum_row!` to build an enum type from an enum row.
#[macro_export]
macro_rules! mk_tyw_enum {
    ($( $ids:expr ),* $(; $tail:expr)?) => {
        $crate::typecheck::TypeWrapper::Concrete(
            $crate::types::AbsType::Enum(
                Box::new(mk_tyw_enum_row!($( $ids ),* $(; $tail)?))
            )
        )
    };
}

/// Wrapper around `mk_tyw_record!` to build a record type from a record row.
#[macro_export]
macro_rules! mk_tyw_record {
    ($(($ids:expr, $tys:expr)),* $(; $tail:expr)?) => {
        $crate::typecheck::TypeWrapper::Concrete(
            $crate::types::AbsType::StaticRecord(
                Box::new(mk_tyw_row!($(($ids, $tys)),* $(; $tail)?))
            )
        )
    };
}

/// Generate an helper function to build a 0-ary type.
macro_rules! generate_builder {
    ($fun:ident, $var:ident) => {
        pub fn $fun() -> TypeWrapper {
            TypeWrapper::Concrete(AbsType::$var())
        }
    };
}

pub fn dyn_record<T>(ty: T) -> TypeWrapper
where
    T: Into<TypeWrapper>,
{
    TypeWrapper::Concrete(AbsType::Dict(Box::new(ty.into())))
}

pub fn array<T>(ty: T) -> TypeWrapper
where
    T: Into<TypeWrapper>,
{
    TypeWrapper::Concrete(AbsType::Array(Box::new(ty.into())))
}

// dyn is a reserved keyword
generate_builder!(dynamic, Dyn);
generate_builder!(str, Str);
generate_builder!(num, Num);
generate_builder!(bool, Bool);
generate_builder!(sym, Sym);
generate_builder!(row_empty, RowEmpty);
