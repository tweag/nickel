//! Helpers for building `TypeWrapper`s.
use super::{UnifType, VarLevelsData};
use crate::typ::{DictTypeFlavour, TypeF};

/// Multi-ary arrow constructor for types implementing `Into<TypeWrapper>`.
#[macro_export]
macro_rules! mk_uty_arrow {
    ($left:expr, $right:expr) => {
        $crate::typecheck::UnifType::concrete(
            $crate::typ::TypeF::Arrow(
                Box::new($crate::typecheck::UnifType::from($left)),
                Box::new($crate::typecheck::UnifType::from($right))
             )
         )
    };
    ( $fst:expr, $snd:expr , $( $types:expr ),+ ) => {
        $crate::mk_uty_arrow!($fst, $crate::mk_uty_arrow!($snd, $( $types ),+))
    };
}

/// Multi-ary enum row constructor for types implementing `Into<TypeWrapper>`.
/// `mk_uty_enum_row!(id1, .., idn; tail)` correspond to `[| 'id1, .., 'idn; tail |]. With the
/// addition of algebraic data types (enum variants), individual rows can also take an additional
/// type parameter, specificed as a tuple: for example, `mk_uty_enum_row!(id1, (id2, ty2); tail)`
/// is `[| 'id1, 'id2 ty2; tail |]`.
#[macro_export]
macro_rules! mk_uty_enum_row {
    () => {
        $crate::typecheck::UnifEnumRows::Concrete {
            erows: $crate::typ::EnumRowsF::Empty,
            var_levels_data: $crate::typecheck::VarLevelsData::new_no_uvars(),
        }
    };
    (; $tail:expr) => {
        $crate::typecheck::UnifEnumRows::from($tail)
    };
    ( ($id:expr, $ty:expr) $(, $rest:tt )* $(; $tail:expr)? ) => {
        $crate::typecheck::UnifEnumRows::concrete(
            $crate::typ::EnumRowsF::Extend {
                row: $crate::typ::EnumRowF {
                    id: $crate::identifier::LocIdent::from($id),
                    typ: Some(Box::new($ty.into())),
                },
                tail: Box::new($crate::mk_uty_enum_row!($( $rest ),* $(; $tail)?))
            }
        )
    };
    ( $id:expr $(, $rest:tt )* $(; $tail:expr)? ) => {
        $crate::typecheck::UnifEnumRows::concrete(
            $crate::typ::EnumRowsF::Extend {
                row: $crate::typ::EnumRowF {
                    id: $crate::identifier::LocIdent::from($id),
                    typ: None,
                },
                tail: Box::new($crate::mk_uty_enum_row!($( $rest ),* $(; $tail)?))
            }
        )
    };
}

/// Multi-ary record row constructor for types implementing `Into<TypeWrapper>`. `mk_uty_row!((id1,
/// ty1), .., (idn, tyn); tail)` correspond to `{id1: ty1, .., idn: tyn; tail}`. The tail can be
/// omitted, in which case the empty row is uses as a tail instead.
#[macro_export]
macro_rules! mk_uty_record_row {
    () => {
        $crate::typecheck::UnifRecordRows::Concrete {
            rrows: $crate::typ::RecordRowsF::Empty,
            var_levels_data: $crate::typecheck::VarLevelsData::new_no_uvars()
        }
    };
    (; $tail:expr) => {
        $crate::typecheck::UnifRecordRows::from($tail)
    };
    (($id:expr, $ty:expr) $(,($ids:expr, $tys:expr))* $(; $tail:expr)?) => {
        $crate::typecheck::UnifRecordRows::concrete(
            $crate::typ::RecordRowsF::Extend {
                row: $crate::typ::RecordRowF {
                    id: $crate::identifier::LocIdent::from($id),
                    typ: Box::new($ty.into()),
                },
                tail: Box::new($crate::mk_uty_record_row!($(($ids, $tys)),* $(; $tail)?)),
            }
        )
    };
}

/// Wrapper around `mk_uty_enum_row!` to build an enum type from an enum row.
#[macro_export]
macro_rules! mk_uty_enum {
    ($( $args:tt )*) => {
        $crate::typecheck::UnifType::concrete(
            $crate::typ::TypeF::Enum(
                $crate::mk_uty_enum_row!($( $args )*)
            )
        )
    };
}

/// Wrapper around `mk_uty_record!` to build a record type from a record row.
#[macro_export]
macro_rules! mk_uty_record {
    ($(($ids:expr, $tys:expr)),* $(; $tail:expr)?) => {
        $crate::typecheck::UnifType::concrete(
            $crate::typ::TypeF::Record(
                $crate::mk_uty_record_row!($(($ids, $tys)),* $(; $tail)?)
            )
        )
    };
}

/// Generate an helper function to build a 0-ary type.
macro_rules! generate_builder {
    ($fun:ident, $var:ident) => {
        pub fn $fun() -> UnifType {
            UnifType::Concrete {
                typ: TypeF::$var,
                var_levels_data: VarLevelsData::new_no_uvars(),
            }
        }
    };
}

pub fn dict<T>(ty: T) -> UnifType
where
    T: Into<UnifType>,
{
    UnifType::concrete(TypeF::Dict {
        type_fields: Box::new(ty.into()),
        flavour: DictTypeFlavour::Type,
    })
}

pub fn array<T>(ty: T) -> UnifType
where
    T: Into<UnifType>,
{
    UnifType::concrete(TypeF::Array(Box::new(ty.into())))
}

pub fn arrow(domain: impl Into<UnifType>, codomain: impl Into<UnifType>) -> UnifType {
    UnifType::concrete(TypeF::Arrow(
        Box::new(domain.into()),
        Box::new(codomain.into()),
    ))
}

// dyn is a reserved keyword
generate_builder!(dynamic, Dyn);
generate_builder!(str, String);
generate_builder!(num, Number);
generate_builder!(bool, Bool);
generate_builder!(sym, Symbol);
generate_builder!(foreign_id, ForeignId);
