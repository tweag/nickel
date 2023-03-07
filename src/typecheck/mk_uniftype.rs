//! Helpers for building `TypeWrapper`s.
use super::{TypeF, UnifType};

/// Multi-ary arrow constructor for types implementing `Into<TypeWrapper>`.
#[macro_export]
macro_rules! mk_uty_arrow {
    ($left:expr, $right:expr) => {
        $crate::typecheck::UnifType::Concrete(
            $crate::types::TypeF::Arrow(
                Box::new($crate::typecheck::UnifType::from($left)),
                Box::new($crate::typecheck::UnifType::from($right))
            )
        )
    };
    ( $fst:expr, $snd:expr , $( $types:expr ),+ ) => {
        mk_uty_arrow!($fst, mk_uty_arrow!($snd, $( $types ),+))
    };
}

/// Multi-ary enum row constructor for types implementing `Into<TypeWrapper>`.
/// `mk_uty_enum_row!(id1, .., idn; tail)` correspond to `<id1, .., idn | tail>.
#[macro_export]
macro_rules! mk_uty_enum_row {
    () => {
        $crate::typecheck::UnifEnumRows::Concrete(EnumRowsF::Empty)
    };
    (; $tail:expr) => {
        $crate::typecheck::UnifEnumRows::from($tail)
    };
    ( $id:expr $(, $ids:expr )* $(; $tail:expr)?) => {
        $crate::typecheck::UnifEnumRows::Concrete(
            $crate::types::EnumRowsF::Extend {
                row: Ident::from($id),
                tail: Box::new(mk_uty_enum_row!($( $ids ),* $(; $tail)?))
            }
        )
    };
}

/// Multi-ary record row constructor for types implementing `Into<TypeWrapper>`.
/// `mk_uty_row!((id1, ty1), .., (idn, tyn); tail)` correspond to `{id1: ty1, .., idn: tyn |
/// tail}. The tail can be omitted, in which case the empty row is uses as a tail instead.
#[macro_export]
macro_rules! mk_uty_row {
    () => {
        $crate::typecheck::UnifRecordRows::Concrete(RecordRowsF::Empty)
    };
    (; $tail:expr) => {
        $crate::typecheck::UnifRecordRows::from($tail)
    };
    (($id:expr, $ty:expr) $(,($ids:expr, $tys:expr))* $(; $tail:expr)?) => {
        $crate::typecheck::UnifRecordRows::Concrete(
            $crate::types::RecordRowsF::Extend {
                row: $crate::types::RecordRowF {
                    id: Ident::from($id),
                    types: Box::new($ty.into()),
                },
                tail: Box::new(mk_uty_row!($(($ids, $tys)),* $(; $tail)?)),
            }
        )
    };
}

/// Wrapper around `mk_uty_enum_row!` to build an enum type from an enum row.
#[macro_export]
macro_rules! mk_uty_enum {
    ($( $ids:expr ),* $(; $tail:expr)?) => {
        $crate::typecheck::UnifType::Concrete(
            $crate::types::TypeF::Enum(
                mk_uty_enum_row!($( $ids ),* $(; $tail)?)
            )
        )
    };
}

/// Wrapper around `mk_uty_record!` to build a record type from a record row.
#[macro_export]
macro_rules! mk_uty_record {
    ($(($ids:expr, $tys:expr)),* $(; $tail:expr)?) => {
        $crate::typecheck::UnifType::Concrete(
            $crate::types::TypeF::Record(
                mk_uty_row!($(($ids, $tys)),* $(; $tail)?)
            )
        )
    };
}

/// Generate an helper function to build a 0-ary type.
macro_rules! generate_builder {
    ($fun:ident, $var:ident) => {
        pub fn $fun() -> UnifType {
            UnifType::Concrete(TypeF::$var)
        }
    };
}

pub fn dyn_record<T>(ty: T) -> UnifType
where
    T: Into<UnifType>,
{
    UnifType::Concrete(TypeF::Dict(Box::new(ty.into())))
}

pub fn array<T>(ty: T) -> UnifType
where
    T: Into<UnifType>,
{
    UnifType::Concrete(TypeF::Array(Box::new(ty.into())))
}

// dyn is a reserved keyword
generate_builder!(dynamic, Dyn);
generate_builder!(str, String);
generate_builder!(num, Number);
generate_builder!(bool, Bool);
generate_builder!(sym, Symbol);
