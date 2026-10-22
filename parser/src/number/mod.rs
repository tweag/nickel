use std::{
    fmt,
    ops::{Add, Div, Mul, Neg, Sub},
};

mod malachite;
use self::malachite as backend;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Number(backend::Rational);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TryFromNumberError;

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
        backend::is_zero(&self.0)
    }

    pub fn is_integer(&self) -> bool {
        backend::is_integer(&self.0)
    }

    pub fn pow(&self, exp: i64) -> Self {
        Number(backend::pow(&self.0, exp))
    }

    pub fn trunc(&self) -> Self {
        Number(backend::trunc(&self.0))
    }

    pub fn round_ties_even(&self) -> Self {
        Number(backend::round_ties_even(&self.0))
    }

    pub fn to_f64(&self) -> f64 {
        backend::to_f64(&self.0)
    }

    pub fn to_f32(&self) -> f32 {
        backend::to_f32(&self.0)
    }

    pub fn to_sci(&self) -> impl fmt::Display + '_ {
        backend::to_sci(&self.0)
    }

    pub fn from_sci_str(s: &str) -> Option<Self> {
        backend::from_sci_str(s).map(Number)
    }

    pub fn from_str_radix(s: &str, radix: u8) -> Option<Self> {
        backend::from_str_radix(s, radix).map(Number)
    }

    pub fn try_from_float_simplest<F: Float>(x: F) -> Result<Self, TryFromFloatError> {
        x.to_number_simplest().ok_or(TryFromFloatError)
    }

    pub fn abs_numerator(&self) -> impl fmt::Display + '_ {
        backend::abs_numerator(&self.0)
    }

    pub fn denominator(&self) -> impl fmt::Display + '_ {
        backend::denominator(&self.0)
    }
}

pub trait Float: Copy + Into<f64> {
    #[doc(hidden)]
    fn to_number_simplest(self) -> Option<Number>;
}

impl Float for f32 {
    fn to_number_simplest(self) -> Option<Number> {
        backend::simplest_from_f32(self).map(Number)
    }
}

impl Float for f64 {
    fn to_number_simplest(self) -> Option<Number> {
        backend::simplest_from_f64(self).map(Number)
    }
}

impl TryFrom<f64> for Number {
    type Error = TryFromFloatError;

    fn try_from(x: f64) -> Result<Self, Self::Error> {
        backend::from_f64(x).map(Number).ok_or(TryFromFloatError)
    }
}

macro_rules! impl_from_int {
    ($($int:ty),*) => {
        $(
            impl From<$int> for Number {
                fn from(n: $int) -> Self {
                    Number(backend::Rational::from(backend::Integer::from(n)))
                }
            }
        )*
    };
}

impl_from_int!(
    i8, i16, i32, i64, i128, isize, u8, u16, u32, u64, u128, usize
);

macro_rules! impl_try_into_int {
    ($wide:ident, $($int:ty),*) => {
        $(
            impl TryFrom<&Number> for $int {
                type Error = TryFromNumberError;

                fn try_from(n: &Number) -> Result<Self, Self::Error> {
                    backend::$wide(&n.0)
                        .and_then(|n| <$int>::try_from(n).ok())
                        .ok_or(TryFromNumberError)
                }
            }
        )*
    };
}

impl_try_into_int!(to_i128, i8, i16, i32, i64, i128, isize);
impl_try_into_int!(to_u128, u8, u16, u32, u64, u128, usize);

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
