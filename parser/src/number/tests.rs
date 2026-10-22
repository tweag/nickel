use super::Number;

fn number(s: &str) -> Number {
    Number::from_sci_str(s).unwrap()
}

fn ratio(numerator: i64, denominator: i64) -> Number {
    Number::from(numerator) / Number::from(denominator)
}

#[test]
fn parse_and_display() {
    for (input, expected) in [
        ("0", "0"),
        ("-0", "0"),
        ("42", "42"),
        ("-2.5", "-5/2"),
        ("0.1", "1/10"),
        ("1e20", "100000000000000000000"),
        ("1e-7", "1/10000000"),
        ("0.000001234", "617/500000000"),
        ("1.0000000000000005", "2000000000000001/2000000000000000"),
        ("+.5", "1/2"),
        (".5", "1/2"),
        ("5.", "5"),
        ("1E5", "100000"),
        ("1e+5", "100000"),
        ("007", "7"),
        ("1.5e-3", "3/2000"),
        (
            "3.14159265358979323846",
            "157079632679489661923/50000000000000000000",
        ),
    ] {
        assert_eq!(number(input).to_string(), expected, "{input}");
    }
}

#[test]
fn parse_rejects() {
    for input in [
        "", ".", "e5", "5e", ".-5", ".+5", "1.2.3", "1_000", "0x1f", " 5", "5 ", "++5", "+-5",
        "-+5", "inf", "1e5e3", "1.5e", "abc", "_1", "1__0", "1_0.5", "1@5", "9.+01", "-", "+.",
    ] {
        assert_eq!(Number::from_sci_str(input), None, "{input:?}");
    }
}

#[test]
fn parse_radix() {
    assert_eq!(Number::from_str_radix("ff", 16), Some(Number::from(255)));
    assert_eq!(Number::from_str_radix("FF", 16), Some(Number::from(255)));
    assert_eq!(Number::from_str_radix("777", 8), Some(Number::from(511)));
    assert_eq!(Number::from_str_radix("-101", 2), Some(Number::from(-5)));
    assert_eq!(
        Number::from_str_radix("ffffffffffffffffffffffffffffffff", 16),
        Some(Number::from(u128::MAX))
    );
    assert_eq!(Number::from_str_radix("2", 2), None);
    assert_eq!(Number::from_str_radix("", 16), None);
    assert_eq!(Number::from_str_radix("1_0", 16), None);
    assert_eq!(Number::from_str_radix("_1", 16), None);
}

#[test]
fn sci() {
    for (input, expected) in [
        ("0", "0"),
        ("42", "42"),
        ("0.1", "0.1"),
        ("-2.5", "-2.5"),
        ("1e20", "1e20"),
        ("1e-7", "1e-7"),
        ("1e-6", "1e-6"),
        ("0.00001", "0.00001"),
        ("0.000001234", "1.234e-6"),
        ("12345678901234567890", "1.234567890123457e19"),
        ("1234567890123456", "1234567890123456"),
        ("12345678901234567", "1.234567890123457e16"),
        ("1.0000000000000005", "1"),
        ("1.0000000000000015", "1.000000000000002"),
        ("9.9999999999999995", "10"),
        ("123456789012345675", "1.234567890123457e17"),
        ("1e-400", "1e-400"),
        ("-1.5e300", "-1.5e300"),
        ("1E5", "100000"),
        ("1.5e-3", "0.0015"),
        ("3.14159265358979323846", "3.141592653589793"),
    ] {
        assert_eq!(number(input).to_sci().to_string(), expected, "{input}");
    }
}

#[test]
fn division_sci() {
    for (numerator, denominator, expected) in [
        (1, 3, "0.3333333333333333"),
        (2, 3, "0.6666666666666667"),
        (-1, 3, "-0.3333333333333333"),
        (1, 7, "0.1428571428571429"),
        (22, 7, "3.142857142857143"),
        (1, 1024, "0.0009765625"),
        (-1, 1_000_000_000_000, "-1e-12"),
        (5, 11, "0.4545454545454545"),
        (6, 11, "0.5454545454545455"),
        (16, 11, "1.454545454545455"),
    ] {
        assert_eq!(
            ratio(numerator, denominator).to_sci().to_string(),
            expected,
            "{numerator}/{denominator}"
        );
    }
}

#[test]
fn arithmetic() {
    assert_eq!(number("0.1") + number("0.2"), number("0.3"));
    assert_eq!(ratio(1, 3) + ratio(1, 6), ratio(1, 2));
    assert_eq!(ratio(1, 3) * Number::from(3), Number::from(1));
    assert_eq!(Number::from(7) - number("2.5"), number("4.5"));
    assert_eq!(-ratio(1, 2), number("-0.5"));
    assert_eq!(&ratio(2, 3) / &ratio(4, 9), ratio(3, 2));
    assert!(ratio(1, 3) < number("0.3333333333333334"));
    assert!(Number::zero().is_zero());
    assert!(Number::from(4).is_integer());
    assert!(!ratio(9, 2).is_integer());
}

#[test]
fn pow() {
    assert_eq!(ratio(2, 3).pow(-3), ratio(27, 8));
    assert_eq!(Number::from(10).pow(20), number("1e20"));
    assert_eq!(Number::from(-2).pow(3), Number::from(-8));
    assert_eq!(Number::zero().pow(0), Number::from(1));
    assert_eq!(ratio(1, 2).pow(64).to_string(), "1/18446744073709551616");
}

#[test]
fn rounding() {
    for (numerator, denominator, trunc, round) in [
        (1, 2, 0, 0),
        (-1, 2, 0, 0),
        (5, 2, 2, 2),
        (7, 2, 3, 4),
        (-5, 2, -2, -2),
        (-7, 2, -3, -4),
        (1, 3, 0, 0),
        (-1, 3, 0, 0),
        (2, 3, 0, 1),
        (-2, 3, 0, -1),
    ] {
        let n = ratio(numerator, denominator);
        assert_eq!(n.trunc(), Number::from(trunc), "trunc {n}");
        assert_eq!(n.round_ties_even(), Number::from(round), "round {n}");
    }
}

#[test]
fn to_float() {
    let max = Number::try_from(f64::MAX).unwrap();
    for (n, f64_value, f32_value) in [
        (ratio(1, 3), 0.3333333333333333, 0.33333334),
        (ratio(2, 3), 0.6666666666666666, 0.6666667),
        (number("1e400"), f64::INFINITY, f32::INFINITY),
        (number("-1e400"), f64::NEG_INFINITY, f32::NEG_INFINITY),
        (number("1e-400"), 0.0, 0.0),
        (number("-1e-400"), -0.0, -0.0),
        (max.clone(), f64::MAX, f32::INFINITY),
        (max + Number::from(1), f64::MAX, f32::INFINITY),
        (
            number("9007199254740993"),
            9007199254740992.0,
            9007199000000000.0,
        ),
        (
            number("-9007199254740993"),
            -9007199254740992.0,
            -9007199000000000.0,
        ),
        (number("5e-324"), 5e-324, 0.0),
        (Number::from(2).pow(-1075), 0.0, 0.0),
        (Number::from(3) * Number::from(2).pow(-1076), 5e-324, 0.0),
    ] {
        assert_eq!(n.to_f64().to_bits(), f64_value.to_bits(), "f64 {n}");
        assert_eq!(n.to_f32().to_bits(), f32_value.to_bits(), "f32 {n}");
    }
}

#[test]
fn to_int() {
    assert_eq!(i64::try_from(&Number::from(i64::MAX)), Ok(i64::MAX));
    assert_eq!(i64::try_from(&Number::from(i64::MIN)), Ok(i64::MIN));
    assert!(i64::try_from(&(Number::from(i64::MAX) + Number::from(1))).is_err());
    assert!(i64::try_from(&ratio(1, 2)).is_err());
    assert_eq!(u64::try_from(&Number::from(u64::MAX)), Ok(u64::MAX));
    assert!(u64::try_from(&Number::from(-1)).is_err());
    assert_eq!(u32::try_from(&number("4e9")), Ok(4_000_000_000));
    assert_eq!(i128::try_from(&Number::from(i128::MIN)), Ok(i128::MIN));
    assert_eq!(usize::try_from(&number("1e3")), Ok(1000));
}

#[test]
fn from_float() {
    for (x, simplest, exact) in [
        (0.1, "1/10", "3602879701896397/36028797018963968"),
        (0.5, "1/2", "1/2"),
        (1.0 / 3.0, "1/3", "6004799503160661/18014398509481984"),
        (-2.75, "-11/4", "-11/4"),
        (123456.789, "123456789/1000", "8483885939586761/68719476736"),
        (-0.0, "0", "0"),
    ] {
        assert_eq!(
            Number::try_from_float_simplest(x).unwrap().to_string(),
            simplest,
            "{x}"
        );
        assert_eq!(Number::try_from(x).unwrap().to_string(), exact, "{x}");
    }
    assert_eq!(
        Number::try_from_float_simplest(5e-324).unwrap().to_string(),
        "1/134934835538207078901663564479278204699704433176094745571267572018286893045330231261306922467624749663458051909544541274259872491590528984578008933293825673281011616184377574242977938494728810894159845118981830045708004988949795132412068903766106370893873371008239179701555609743487001688124218339097337508871805160728996523"
    );
    assert_eq!(
        Number::try_from_float_simplest(f64::MAX),
        Number::try_from(f64::MAX)
    );
    assert_eq!(Number::try_from_float_simplest(0.1f32), Ok(ratio(1, 10)));
    assert_eq!(
        Number::try_from_float_simplest(1.0f32 / 3.0),
        Ok(ratio(1, 3))
    );
    assert_eq!(
        Number::try_from_float_simplest(16777217.0f32),
        Ok(Number::from(16777216))
    );
    for x in [f64::NAN, f64::INFINITY, f64::NEG_INFINITY] {
        assert!(Number::try_from_float_simplest(x).is_err(), "{x}");
        assert!(Number::try_from(x).is_err(), "{x}");
    }
}

#[test]
fn rational_parts() {
    let n = ratio(-6, 4);
    assert_eq!(n.abs_numerator().to_string(), "3");
    assert_eq!(n.denominator().to_string(), "2");
}
