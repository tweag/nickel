//! Helpers for building `TypeWrapper`s.
use super::{UnifType, VarLevelsData};
use crate::typ::{DictTypeFlavour, TypeF};

/// Multi-ary arrow constructor for types implementing `Into<TypeWrapper>`.
#[macro_export]
macro_rules! mk_buty_arrow {
    ($left:expr, $right:expr) => {
        $crate::bytecode::typecheck::UnifType::concrete(
            $crate::typ::TypeF::Arrow(
                Box::new($crate::bytecode::typecheck::UnifType::from($left)),
                Box::new($crate::bytecode::typecheck::UnifType::from($right))
             )
         )
    };
    ( $fst:expr, $snd:expr , $( $types:expr ),+ ) => {
        $crate::mk_buty_arrow!($fst, $crate::mk_buty_arrow!($snd, $( $types ),+))
    };
}

/// Multi-ary enum row constructor for types implementing `Into<TypeWrapper>`.
/// `mk_buty_enum_row!(id1, .., idn; tail)` correspond to `[| 'id1, .., 'idn; tail |]. With the
/// addition of algebraic data types (enum variants), individual rows can also take an additional
/// type parameter, specified as a tuple: for example, `mk_buty_enum_row!(id1, (id2, ty2); tail)`
/// is `[| 'id1, 'id2 ty2; tail |]`.
#[macro_export]
macro_rules! mk_buty_enum_row {
    () => {
        $crate::bytecode::typecheck::UnifEnumRows::Concrete {
            erows: $crate::typ::EnumRowsF::Empty,
            var_levels_data: $crate::bytecode::typecheck::VarLevelsData::new_no_uvars(),
        }
    };
    (; $tail:expr) => {
        $crate::bytecode::typecheck::UnifEnumRows::from($tail)
    };
    ( ($id:expr, $ty:expr) $(, $rest:tt )* $(; $tail:expr)? ) => {
        $crate::bytecode::typecheck::UnifEnumRows::concrete(
            $crate::typ::EnumRowsF::Extend {
                row: $crate::typ::EnumRowF {
                    id: $crate::identifier::LocIdent::from($id),
                    typ: Some(Box::new($ty.into())),
                },
                tail: Box::new($crate::mk_buty_enum_row!($( $rest ),* $(; $tail)?))
            }
        )
    };
    ( $id:expr $(, $rest:tt )* $(; $tail:expr)? ) => {
        $crate::bytecode::typecheck::UnifEnumRows::concrete(
            $crate::typ::EnumRowsF::Extend {
                row: $crate::typ::EnumRowF {
                    id: $crate::identifier::LocIdent::from($id),
                    typ: None,
                },
                tail: Box::new($crate::mk_buty_enum_row!($( $rest ),* $(; $tail)?))
            }
        )
    };
}

/// Multi-ary record row constructor for types implementing `Into<TypeWrapper>`. `mk_buty_row!((id1,
/// ty1), .., (idn, tyn); tail)` correspond to `{id1: ty1, .., idn: tyn; tail}`. The tail can be
/// omitted, in which case the empty row is uses as a tail instead.
#[macro_export]
macro_rules! mk_buty_record_row {
    () => {
        $crate::bytecode::typecheck::UnifRecordRows::Concrete {
            rrows: $crate::typ::RecordRowsF::Empty,
            var_levels_data: $crate::bytecode::typecheck::VarLevelsData::new_no_uvars()
        }
    };
    (; $tail:expr) => {
        $crate::bytecode::typecheck::UnifRecordRows::from($tail)
    };
    (($id:expr, $ty:expr) $(,($ids:expr, $tys:expr))* $(; $tail:expr)?) => {
        $crate::bytecode::typecheck::UnifRecordRows::concrete(
            $crate::typ::RecordRowsF::Extend {
                row: $crate::typ::RecordRowF {
                    id: $crate::identifier::LocIdent::from($id),
                    typ: Box::new($ty.into()),
                },
                tail: Box::new($crate::mk_buty_record_row!($(($ids, $tys)),* $(; $tail)?)),
            }
        )
    };
}

/// Wrapper around `mk_buty_enum_row!` to build an enum type from an enum row.
#[macro_export]
macro_rules! mk_buty_enum {
    ($( $args:tt )*) => {
        $crate::bytecode::typecheck::UnifType::concrete(
            $crate::typ::TypeF::Enum(
                $crate::mk_buty_enum_row!($( $args )*)
            )
        )
    };
}

/// Wrapper around `mk_buty_record!` to build a record type from a record row.
#[macro_export]
macro_rules! mk_buty_record {
    ($(($ids:expr, $tys:expr)),* $(; $tail:expr)?) => {
        $crate::bytecode::typecheck::UnifType::concrete(
            $crate::typ::TypeF::Record(
                $crate::mk_buty_record_row!($(($ids, $tys)),* $(; $tail)?)
            )
        )
    };
}

/// Generate an helper function to build a 0-ary type.
macro_rules! generate_builder {
    ($fun:ident, $var:ident) => {
        pub fn $fun<'ast>() -> UnifType<'ast> {
            UnifType::Concrete {
                typ: TypeF::$var,
                var_levels_data: VarLevelsData::new_no_uvars(),
            }
        }
    };
}

pub fn dict<'ast, T>(ty: T) -> UnifType<'ast>
where
    T: Into<UnifType<'ast>>,
{
    UnifType::concrete(TypeF::Dict {
        type_fields: Box::new(ty.into()),
        flavour: DictTypeFlavour::Type,
    })
}

pub fn array<'ast, T>(ty: T) -> UnifType<'ast>
where
    T: Into<UnifType<'ast>>,
{
    UnifType::concrete(TypeF::Array(Box::new(ty.into())))
}

pub fn arrow<'ast>(
    domain: impl Into<UnifType<'ast>>,
    codomain: impl Into<UnifType<'ast>>,
) -> UnifType<'ast> {
    UnifType::concrete(TypeF::Arrow(
        Box::new(domain.into()),
        Box::new(codomain.into()),
    ))
}

pub fn nary_arrow<'ast, I, U>(args: I, codomain: U) -> UnifType<'ast>
where
    U: Into<UnifType<'ast>>,
    I: IntoIterator<Item: Into<UnifType<'ast>>, IntoIter: std::iter::DoubleEndedIterator>,
{
    args.into_iter()
        .rev()
        .fold(codomain.into(), |acc, ty| mk_buty_arrow!(ty.into(), acc))
}

// dyn is a reserved keyword
generate_builder!(dynamic, Dyn);
generate_builder!(str, String);
generate_builder!(num, Number);
generate_builder!(bool, Bool);
generate_builder!(sym, Symbol);
generate_builder!(foreign_id, ForeignId);
