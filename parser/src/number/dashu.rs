use std::{fmt, str::FromStr, sync::LazyLock};

use dashu_base::{Abs, Signed, UnsignedAbs};
use dashu_float::DBig;
use dashu_int::UBig;
use regex::Regex;

pub(super) use dashu_int::IBig as Integer;
pub(super) use dashu_ratio::RBig as Rational;

pub(super) fn is_zero(q: &Rational) -> bool {
    q.is_zero()
}

pub(super) fn is_integer(q: &Rational) -> bool {
    q.is_int()
}

pub(super) fn pow(q: &Rational, exp: i64) -> Rational {
    match isize::try_from(exp) {
        Ok(exp) => q.pow(exp),
        Err(_) => pow(q, exp / 2).pow(2) * q.pow((exp % 2) as isize),
    }
}

pub(super) fn trunc(q: &Rational) -> Rational {
    q.trunc().into()
}

pub(super) fn round_ties_even(q: &Rational) -> Rational {
    let trunc = q.trunc();
    let is_tie = *q.denominator() == UBig::from(2u8);
    if is_tie && (&trunc % Integer::from(2)).is_zero() {
        trunc.into()
    } else {
        q.round().into()
    }
}

pub(super) fn to_f64(q: &Rational) -> f64 {
    q.to_f64().value()
}

pub(super) fn to_f32(q: &Rational) -> f32 {
    q.to_f32().value()
}

pub(super) fn to_i128(q: &Rational) -> Option<i128> {
    q.is_int().then(|| q.numerator().try_into().ok()).flatten()
}

pub(super) fn to_u128(q: &Rational) -> Option<u128> {
    q.is_int().then(|| q.numerator().try_into().ok()).flatten()
}

const SCI_PRECISION: isize = 16;
const SCI_NEG_EXP_THRESHOLD: isize = -6;

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

struct Sci<'a>(&'a Rational);

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
        let significand = round_ties_even(&(abs * Rational::from(10).pow(scale))).to_string();
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

pub(super) fn to_sci(q: &Rational) -> impl fmt::Display + '_ {
    Sci(q)
}

static SCI_SYNTAX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^[+-]?([0-9]+\.?[0-9]*|\.[0-9]+)([eE][+-]?[0-9]+)?$").unwrap());

pub(super) fn from_sci_str(s: &str) -> Option<Rational> {
    if !SCI_SYNTAX.is_match(s) {
        return None;
    }
    Rational::try_from(DBig::from_str(s).ok()?).ok()
}

pub(super) fn from_str_radix(s: &str, radix: u8) -> Option<Rational> {
    if s.contains('_') {
        return None;
    }
    Integer::from_str_radix(s, u32::from(radix))
        .ok()
        .map(Rational::from)
}

pub(super) fn from_f64(x: f64) -> Option<Rational> {
    Rational::try_from(x).ok()
}

pub(super) fn simplest_from_f64(x: f64) -> Option<Rational> {
    Rational::simplest_from_f64(x)
}

pub(super) fn simplest_from_f32(x: f32) -> Option<Rational> {
    Rational::simplest_from_f32(x)
}

pub(super) fn abs_numerator(q: &Rational) -> impl fmt::Display + '_ {
    q.numerator().unsigned_abs()
}

pub(super) fn denominator(q: &Rational) -> impl fmt::Display + '_ {
    q.denominator()
}
