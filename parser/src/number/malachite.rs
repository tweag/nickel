use std::fmt;

use malachite::base::{
    num::{
        arithmetic::traits::Pow,
        conversion::traits::{FromSciString, FromStringBase, IsInteger, RoundingFrom, ToSci},
    },
    rounding_modes::RoundingMode,
};

pub(super) use malachite::{Integer, rational::Rational};

pub(super) fn is_zero(q: &Rational) -> bool {
    *q == 0u32
}

pub(super) fn is_integer(q: &Rational) -> bool {
    q.is_integer()
}

pub(super) fn pow(q: &Rational, exp: i64) -> Rational {
    q.pow(exp)
}

pub(super) fn trunc(q: &Rational) -> Rational {
    Integer::rounding_from(q, RoundingMode::Down).0.into()
}

pub(super) fn round_ties_even(q: &Rational) -> Rational {
    Integer::rounding_from(q, RoundingMode::Nearest).0.into()
}

pub(super) fn to_f64(q: &Rational) -> f64 {
    f64::rounding_from(q, RoundingMode::Nearest).0
}

pub(super) fn to_f32(q: &Rational) -> f32 {
    f32::rounding_from(q, RoundingMode::Nearest).0
}

pub(super) fn to_i128(q: &Rational) -> Option<i128> {
    i128::try_from(q).ok()
}

pub(super) fn to_u128(q: &Rational) -> Option<u128> {
    u128::try_from(q).ok()
}

pub(super) fn to_sci(q: &Rational) -> impl fmt::Display + '_ {
    q.to_sci()
}

pub(super) fn from_sci_str(s: &str) -> Option<Rational> {
    Rational::from_sci_string(s)
}

pub(super) fn from_str_radix(s: &str, radix: u8) -> Option<Rational> {
    Integer::from_string_base(radix, s).map(Rational::from)
}

pub(super) fn from_f64(x: f64) -> Option<Rational> {
    Rational::try_from(x).ok()
}

pub(super) fn simplest_from_f64(x: f64) -> Option<Rational> {
    Rational::try_from_float_simplest(x).ok()
}

pub(super) fn simplest_from_f32(x: f32) -> Option<Rational> {
    Rational::try_from_float_simplest(x).ok()
}

pub(super) fn abs_numerator(q: &Rational) -> impl fmt::Display + '_ {
    q.numerator_ref()
}

pub(super) fn denominator(q: &Rational) -> impl fmt::Display + '_ {
    q.denominator_ref()
}
