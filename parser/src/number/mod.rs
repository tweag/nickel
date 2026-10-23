//! Arbitrary precision rational numbers.
//!
//! [Number] wraps the rational type `RBig` of [dashu](https://crates.io/crates/dashu).
use std::{
    fmt,
    ops::{Add, Div, Mul, Neg, Sub},
    str::FromStr,
    sync::LazyLock,
};

use dashu_base::{Abs, Signed, UnsignedAbs};
use dashu_float::DBig;
use dashu_int::{IBig, UBig};
use dashu_ratio::RBig;
use regex::Regex;

#[cfg(test)]
mod tests;

/// A Nickel number: an arbitrary precision rational.
///
/// See [crate::ast::Number] for the semantics of arithmetic operations and serialization.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Number(RBig);

/// The error returned when a number isn't an integer in the range of the target type.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TryFromNumberError;

/// The error returned when converting a NaN or an infinite float to a number.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TryFromFloatError;

impl fmt::Display for TryFromNumberError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("number is not an integer in the range of the target type")
    }
}

impl std::error::Error for TryFromNumberError {}

impl fmt::Display for TryFromFloatError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("NaN and infinities are not numbers")
    }
}

impl std::error::Error for TryFromFloatError {}

impl Number {
    pub fn zero() -> Self {
        Number::from(0)
    }

    pub fn is_zero(&self) -> bool {
        self.0.is_zero()
    }

    pub fn is_integer(&self) -> bool {
        self.0.is_int()
    }

    /// Raises `self` to the power `exp`. The result is exact.
    ///
    /// # Panics
    ///
    /// Panics if `self` is zero and `exp` is negative.
    pub fn pow(&self, exp: i64) -> Self {
        match isize::try_from(exp) {
            Ok(exp) => Number(self.0.pow(exp)),
            Err(_) => Number(self.pow(exp / 2).0.pow(2) * self.0.pow((exp % 2) as isize)),
        }
    }

    /// Rounds toward zero to an integer.
    pub fn trunc(&self) -> Self {
        Number(self.0.trunc().into())
    }

    /// Rounds to the nearest integer. A number halfway between two integers rounds to the even one.
    pub fn round_ties_even(&self) -> Self {
        let trunc = self.0.trunc();
        let is_tie = *self.0.denominator() == UBig::from(2u8);
        if is_tie && (&trunc % IBig::from(2)).is_zero() {
            Number(trunc.into())
        } else {
            Number(self.0.round().into())
        }
    }

    /// Rounds to the nearest `f64`, ties to even. A number beyond the range of `f64` gives an
    /// infinity, and a negative number that rounds to zero gives `-0.0`.
    pub fn to_f64(&self) -> f64 {
        self.0.to_f64().value()
    }

    /// Rounds to the nearest `f32`, with the same rules as [Self::to_f64].
    pub fn to_f32(&self) -> f32 {
        self.0.to_f32().value()
    }

    /// Displays the number in decimal, rounded to 16 significant digits (ties to even) and without
    /// trailing zeros. The exponent notation, as in `1.5e20`, is used when the rounded number is at
    /// least `1e16` or below `1e-5` in absolute value.
    pub fn to_sci(&self) -> impl fmt::Display + '_ {
        Sci(&self.0)
    }

    /// Parses a decimal number with an optional sign, fractional part and exponent, as in
    /// `-1.5e-3`. The result is exact. Returns `None` if `s` isn't of this form.
    pub fn from_sci_str(s: &str) -> Option<Self> {
        if !SCI_SYNTAX.is_match(s) {
            return None;
        }
        RBig::try_from(DBig::from_str(s).ok()?).ok().map(Number)
    }

    /// Parses an integer written in base `radix`, which must be between 2 and 36.
    pub fn from_str_radix(s: &str, radix: u8) -> Option<Self> {
        if s.contains('_') {
            return None;
        }
        IBig::from_str_radix(s, u32::from(radix))
            .ok()
            .map(|n| Number(RBig::from(n)))
    }

    /// Converts a float to the rational with the smallest denominator that rounds to this float.
    /// For example, `0.1` gives `1/10` and not the exact value of the float. Fails on NaN and
    /// infinities.
    pub fn try_from_float_simplest<F: Float>(x: F) -> Result<Self, TryFromFloatError> {
        x.to_number_simplest().ok_or(TryFromFloatError)
    }

    /// The absolute value of the numerator, in lowest terms.
    pub fn abs_numerator(&self) -> impl fmt::Display + '_ {
        self.0.numerator().unsigned_abs()
    }

    /// The denominator, in lowest terms. It is always positive.
    pub fn denominator(&self) -> impl fmt::Display + '_ {
        self.0.denominator()
    }
}

/// The primitive floats that convert to [Number] with [Number::try_from_float_simplest].
pub trait Float: Copy + Into<f64> {
    #[doc(hidden)]
    fn to_number_simplest(self) -> Option<Number>;
}

impl Float for f32 {
    fn to_number_simplest(self) -> Option<Number> {
        RBig::simplest_from_f32(self).map(Number)
    }
}

impl Float for f64 {
    fn to_number_simplest(self) -> Option<Number> {
        RBig::simplest_from_f64(self).map(Number)
    }
}

/// Converts a float to the rational that it represents exactly. Fails on NaN and infinities.
impl TryFrom<f64> for Number {
    type Error = TryFromFloatError;

    fn try_from(x: f64) -> Result<Self, Self::Error> {
        RBig::try_from(x).map(Number).map_err(|_| TryFromFloatError)
    }
}

macro_rules! impl_from_int {
    ($($int:ty),*) => {
        $(
            impl From<$int> for Number {
                fn from(n: $int) -> Self {
                    Number(RBig::from(IBig::from(n)))
                }
            }
        )*
    };
}

impl_from_int!(
    i8, i16, i32, i64, i128, isize, u8, u16, u32, u64, u128, usize
);

macro_rules! impl_try_into_int {
    ($($int:ty),*) => {
        $(
            impl TryFrom<&Number> for $int {
                type Error = TryFromNumberError;

                fn try_from(n: &Number) -> Result<Self, Self::Error> {
                    n.0.is_int()
                        .then(|| n.0.numerator().try_into().ok())
                        .flatten()
                        .ok_or(TryFromNumberError)
                }
            }
        )*
    };
}

impl_try_into_int!(
    i8, i16, i32, i64, i128, isize, u8, u16, u32, u64, u128, usize
);

const SCI_PRECISION: isize = 16;
const SCI_NEG_EXP_THRESHOLD: isize = -6;

static SCI_SYNTAX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^[+-]?([0-9]+\.?[0-9]*|\.[0-9]+)([eE][+-]?[0-9]+)?$").unwrap());

fn floor_log10(n: &UBig, d: &UBig) -> isize {
    let ten = UBig::from(10u8);
    let estimate = n.ilog(&ten) as isize - d.ilog(&ten) as isize;
    let below = if estimate >= 0 {
        *n < d * ten.pow(estimate as usize)
    } else {
        n * ten.pow(estimate.unsigned_abs()) < *d
    };
    estimate - isize::from(below)
}

struct Sci<'a>(&'a RBig);

impl fmt::Display for Sci<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0.is_zero() {
            return f.write_str("0");
        }
        if self.0.is_negative() {
            f.write_str("-")?;
        }
        let abs = self.0.clone().abs();
        let log = floor_log10(&abs.numerator().unsigned_abs(), abs.denominator());
        let scale = SCI_PRECISION - 1 - log;
        let significand = Number(abs * RBig::from(10).pow(scale))
            .round_ties_even()
            .to_string();
        let digits = significand.trim_end_matches('0');
        let exponent = (significand.len() - digits.len()) as isize - scale;
        let log = exponent + digits.len() as isize - 1;
        if log <= SCI_NEG_EXP_THRESHOLD || log >= SCI_PRECISION {
            let (first, rest) = digits.split_at(1);
            let point = if rest.is_empty() { "" } else { "." };
            write!(f, "{first}{point}{rest}e{log}")
        } else if exponent >= 0 {
            write!(f, "{digits}{}", "0".repeat(exponent as usize))
        } else if log >= 0 {
            let (int, fract) = digits.split_at(log as usize + 1);
            write!(f, "{int}.{fract}")
        } else {
            write!(f, "0.{}{digits}", "0".repeat((-log - 1) as usize))
        }
    }
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl fmt::Debug for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl Neg for Number {
    type Output = Number;

    fn neg(self) -> Number {
        Number(-self.0)
    }
}

impl Neg for &Number {
    type Output = Number;

    fn neg(self) -> Number {
        Number(-&self.0)
    }
}

macro_rules! impl_binop {
    ($($trait:ident, $method:ident);*) => {
        $(
            impl $trait<Number> for Number {
                type Output = Number;

                fn $method(self, rhs: Number) -> Number {
                    Number(self.0.$method(rhs.0))
                }
            }

            impl $trait<&Number> for Number {
                type Output = Number;

                fn $method(self, rhs: &Number) -> Number {
                    Number(self.0.$method(&rhs.0))
                }
            }

            impl $trait<Number> for &Number {
                type Output = Number;

                fn $method(self, rhs: Number) -> Number {
                    Number((&self.0).$method(rhs.0))
                }
            }

            impl $trait<&Number> for &Number {
                type Output = Number;

                fn $method(self, rhs: &Number) -> Number {
                    Number((&self.0).$method(&rhs.0))
                }
            }
        )*
    };
}

impl_binop!(Add, add; Sub, sub; Mul, mul; Div, div);
