//! Serialization of an evaluated program to various data format.
use crate::{
    error::ExportError,
    identifier::LocIdent,
    term::{
        array::{Array, ArrayAttrs},
        record::RecordData,
        IndexMap, Number, RichTerm, Term, TypeAnnotation,
    },
};

use serde::{
    de::{Deserialize, Deserializer},
    ser::{Serialize, SerializeMap, SerializeSeq, Serializer},
};

use malachite::{
    num::conversion::traits::{IsInteger, RoundingFrom},
    rounding_modes::RoundingMode,
};
use once_cell::sync::Lazy;

use std::{fmt, io, rc::Rc};

/// Available export formats.
// If you add or remove variants, remember to update the CLI docs in `src/bin/nickel.rs'
#[derive(Copy, Clone, Eq, PartialEq, Debug, Default, clap::ValueEnum)]
pub enum ExportFormat {
    Raw,
    #[default]
    Json,
    Yaml,
    Toml,
}

impl fmt::Display for ExportFormat {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Raw => write!(f, "raw"),
            Self::Json => write!(f, "json"),
            Self::Yaml => write!(f, "yaml"),
            Self::Toml => write!(f, "toml"),
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct ParseFormatError(String);

impl fmt::Display for ParseFormatError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "unsupported export format {}", self.0)
    }
}

/// Implicitly convert numbers to primitive integers when possible, and serialize an exact
/// representation. Note that `u128` and `i128` aren't supported for common configuration formats in
/// serde, so we rather pick `i64` and `u64`, even if the former couple theoretically allows for a
/// wider range of rationals to be exactly represented. We don't expect values to be that large in
/// practice anyway: using arbitrary precision rationals is directed toward not introducing rounding
/// errors when performing simple arithmetic operations over decimals numbers, mostly.
///
/// If the number doesn't fit into an `i64` or `u64`, we approximate it by the nearest `f64` and
/// serialize this value. This may incur a loss of precision, but this is expected: we can't
/// represent something like e.g. `1/3` exactly in JSON anyway.
pub fn serialize_num<S>(n: &Number, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    if n.is_integer() {
        if *n < 0 {
            if let Ok(n_as_integer) = i64::try_from(n) {
                return n_as_integer.serialize(serializer);
            }
        } else if let Ok(n_as_uinteger) = u64::try_from(n) {
            return n_as_uinteger.serialize(serializer);
        }
    }

    f64::rounding_from(n, RoundingMode::Nearest)
        .0
        .serialize(serializer)
}

/// Deserialize a Nickel number. As for parsing, we convert the number from a 64bits float
pub fn deserialize_num<'de, D>(deserializer: D) -> Result<Number, D::Error>
where
    D: Deserializer<'de>,
{
    let as_f64 = f64::deserialize(deserializer)?;
    Number::try_from_float_simplest(as_f64).map_err(|_| {
        serde::de::Error::custom(format!(
            "couldn't convert {as_f64} to a Nickel number: Nickel doesn't support NaN nor infinity"
        ))
    })
}

/// Serializer for annotated values.
pub fn serialize_annotated_value<S>(
    _annot: &TypeAnnotation,
    t: &RichTerm,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    t.serialize(serializer)
}

/// Serializer for a record. Serialize fields in alphabetical order to get a deterministic output
pub fn serialize_record<S>(record: &RecordData, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    let mut entries = record
        .iter_serializable()
        .collect::<Result<Vec<_>, _>>()
        .map_err(|missing_def_err| {
            serde::ser::Error::custom(format!(
                "missing field definition for `{}`",
                missing_def_err.id
            ))
        })?;

    entries.sort_by_key(|(k, _)| *k);

    let mut map_ser = serializer.serialize_map(Some(entries.len()))?;
    for (id, t) in entries.iter() {
        map_ser.serialize_entry(&id.to_string(), &t)?
    }

    map_ser.end()
}

/// Deserialize for a record. Required to set the record attributes to default.
pub fn deserialize_record<'de, D>(deserializer: D) -> Result<RecordData, D::Error>
where
    D: Deserializer<'de>,
{
    let fields = IndexMap::<LocIdent, _>::deserialize(deserializer)?;
    Ok(RecordData::with_field_values(fields))
}

/// Serialize for an Array. Required to hide the internal attributes.
pub fn serialize_array<S>(
    terms: &Array,
    _attrs: &ArrayAttrs,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    let mut seq = serializer.serialize_seq(Some(terms.len()))?;
    for term in terms.iter() {
        seq.serialize_element(term)?;
    }

    seq.end()
}

/// Deserialize for an Array. Required to set the default attributes.
pub fn deserialize_array<'de, D>(deserializer: D) -> Result<(Array, ArrayAttrs), D::Error>
where
    D: Deserializer<'de>,
{
    let terms = Array::new(Rc::from(Vec::deserialize(deserializer)?));
    Ok((terms, Default::default()))
}

impl Serialize for RichTerm {
    /// Serialize the underlying term.
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        (*self.term).serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for RichTerm {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let t: Term = Term::deserialize(deserializer)?;
        Ok(RichTerm::from(t))
    }
}

/// Check that a term is serializable. Serializable terms are booleans, numbers, strings, enum,
/// arrays of serializable terms or records of serializable terms.
pub fn validate(format: ExportFormat, t: &RichTerm) -> Result<(), ExportError> {
    use Term::*;

    static NUMBER_MIN: Lazy<Number> = Lazy::new(|| Number::try_from(f64::MIN).unwrap());
    static NUMBER_MAX: Lazy<Number> = Lazy::new(|| Number::try_from(f64::MAX).unwrap());

    if format == ExportFormat::Raw {
        if let Term::Str(_) = t.term.as_ref() {
            Ok(())
        } else {
            Err(ExportError::NotAString(t.clone()))
        }
    } else {
        match t.term.as_ref() {
            // TOML doesn't support null values
            Null if format == ExportFormat::Json || format == ExportFormat::Yaml => Ok(()),
            Null => Err(ExportError::UnsupportedNull(format, t.clone())),
            Bool(_) | Str(_) | Enum(_) => Ok(()),
            Num(n) => {
                if *n >= *NUMBER_MIN && *n <= *NUMBER_MAX {
                    Ok(())
                } else {
                    Err(ExportError::NumberOutOfRange {
                        term: t.clone(),
                        value: n.clone(),
                    })
                }
            }
            Record(record) => {
                record.iter_serializable().try_for_each(|binding| {
                    // unwrap(): terms must be fully evaluated before being validated for
                    // serialization. Otherwise, it's an internal error.
                    let (_, rt) = binding.unwrap_or_else(|err| {
                        panic!(
                            "encountered field without definition `{}` \
                            during pre-serialization validation",
                            err.id
                        )
                    });
                    validate(format, rt)
                })?;
                Ok(())
            }
            Array(array, _) => {
                array.iter().try_for_each(|t| validate(format, t))?;
                Ok(())
            }
            _ => Err(ExportError::NonSerializable(t.clone())),
        }
    }
}

pub fn to_writer<W>(mut writer: W, format: ExportFormat, rt: &RichTerm) -> Result<(), ExportError>
where
    W: io::Write,
{
    match format {
        ExportFormat::Json => serde_json::to_writer_pretty(writer, &rt)
            .map_err(|err| ExportError::Other(err.to_string())),
        ExportFormat::Yaml => {
            serde_yaml::to_writer(writer, &rt).map_err(|err| ExportError::Other(err.to_string()))
        }
        ExportFormat::Toml => toml::to_string_pretty(rt)
            .map_err(|err| ExportError::Other(err.to_string()))
            .and_then(|s| {
                writer
                    .write_all(s.as_bytes())
                    .map_err(|err| ExportError::Other(err.to_string()))
            }),
        ExportFormat::Raw => match rt.as_ref() {
            Term::Str(s) => writer
                .write_all(s.as_bytes())
                .map_err(|err| ExportError::Other(err.to_string())),
            t => Err(ExportError::Other(format!(
                "raw export requires a `String`, got {}",
                // unwrap(): terms must be fully evaluated before serialization,
                // and fully evaluated terms have a definite type.
                t.type_of().unwrap()
            ))),
        },
    }
}

pub fn to_string(format: ExportFormat, rt: &RichTerm) -> Result<String, ExportError> {
    let mut buffer: Vec<u8> = Vec::new();
    to_writer(&mut buffer, format, rt)?;

    Ok(String::from_utf8_lossy(&buffer).into_owned())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cache::resolvers::DummyResolver;
    use crate::eval::cache::CacheImpl;
    use crate::eval::VirtualMachine;
    use crate::program::Program;
    use crate::term::{make as mk_term, BinaryOp};
    use serde_json::json;
    use std::io::Cursor;

    fn eval(s: &str) -> RichTerm {
        let src = Cursor::new(s);
        let mut prog =
            Program::<CacheImpl>::new_from_source(src, "<test>", std::io::stderr()).unwrap();
        prog.eval_full().expect("program eval should succeed")
    }

    #[track_caller]
    fn assert_json_eq<T: Serialize>(term: &str, expected: T) {
        assert_eq!(
            serde_json::to_string(&eval(term)).unwrap(),
            serde_json::to_string(&expected).unwrap()
        )
    }

    #[track_caller]
    fn assert_nickel_eq(term: RichTerm, expected: RichTerm) {
        assert_eq!(
            VirtualMachine::<_, CacheImpl>::new(DummyResolver {}, std::io::stderr())
                .eval(mk_term::op2(BinaryOp::Eq(), term, expected))
                .map(Term::from),
            Ok(Term::Bool(true))
        )
    }

    #[track_caller]
    fn assert_pass_validation(format: ExportFormat, term: &str) {
        validate(format, &eval(term)).unwrap();
    }

    #[track_caller]
    fn assert_fail_validation(format: ExportFormat, term: &str) {
        validate(format, &eval(term)).unwrap_err();
    }

    #[track_caller]
    fn assert_involutory(term: &str) {
        let evaluated = eval(term);
        let from_json: RichTerm =
            serde_json::from_str(&serde_json::to_string(&evaluated).unwrap()).unwrap();
        let from_yaml: RichTerm =
            serde_yaml::from_str(&serde_yaml::to_string(&evaluated).unwrap()).unwrap();
        let from_toml: RichTerm = toml::from_str(&toml::to_string(&evaluated).unwrap()).unwrap();

        assert_nickel_eq(from_json, evaluated.clone());
        assert_nickel_eq(from_yaml, evaluated.clone());
        assert_nickel_eq(from_toml, evaluated);
    }

    #[test]
    fn basic() {
        assert_json_eq("1 + 1", 2);

        let null: Option<()> = None;
        assert_json_eq("null", null);

        assert_json_eq("if true then false else true", false);
        assert_json_eq(r#""Hello, %{"world"}!""#, "Hello, world!");
        assert_json_eq("'foo", "foo");
    }

    #[test]
    fn arrays() {
        assert_json_eq("[]", json!([]));
        assert_json_eq("[null, (1+1), (2+2), (3+3)]", json!([null, 2, 4, 6]));
        assert_json_eq(
            r#"['a, ("b" ++ "c"), "d%{"e"}f", "g"]"#,
            json!(["a", "bc", "def", "g"]),
        );
        assert_json_eq(
            r#"std.array.fold_right (fun elt acc => [[elt]] @ acc) [] [1, 2, 3, 4]"#,
            json!([[1], [2], [3], [4]]),
        );
        assert_json_eq("[\"a\", 1, false, 'foo]", json!(["a", 1, false, "foo"]));
    }

    #[test]
    fn records() {
        assert_json_eq(
            "{a = 1, b = 2+2, c = 3, d = null}",
            json!({"a": 1, "b": 4, "c": 3, "d": null}),
        );

        assert_json_eq(
            "{a = {} & {b = {c = if true then 'richtig else 'falsch}}}",
            json!({"a": {"b": {"c": "richtig"}}}),
        );

        assert_json_eq(
            "{foo = let z = 0.5 + 0.5 in z, \
            bar = [\"str\", true || false], \
            baz = {subfoo = !false} & {subbar = 1 - 1}}",
            json!({"foo": 1, "bar": ["str", true], "baz": {"subfoo": true, "subbar": 0}}),
        );
    }

    #[test]
    fn meta_values() {
        assert_json_eq(
            "{a | default = 1, b | doc \"doc\" = 2+2, c = 3}",
            json!({"a": 1, "b": 4, "c": 3}),
        );

        assert_json_eq(
            "{a = {b | default = {}} & {b.c | default = (if true then 'faux else 'vrai)}}",
            json!({"a": {"b": {"c": "faux"}}}),
        );

        assert_json_eq(
            "{baz | default = {subfoo | default = !false} & {subbar | default = 1 - 1}}",
            json!({"baz": {"subfoo": true, "subbar": 0}}),
        );

        assert_json_eq(
            "{a = {b | default = {}} & {b.c | not_exported = false} & {b.d = true}}",
            json!({"a": {"b": {"d": true}}}),
        );
    }

    #[test]
    fn prevalidation() {
        assert_fail_validation(ExportFormat::Json, "{a = 1, b = {c = fun x => x}}");
        assert_fail_validation(
            ExportFormat::Json,
            "{foo.bar = let y = \"a\" in y, b = [[fun x => x]]}",
        );
        assert_pass_validation(ExportFormat::Json, "{foo = null}");
        assert_fail_validation(ExportFormat::Toml, "{foo = null}");
    }

    #[test]
    fn involution() {
        assert_involutory("{val = 1 + 1}");
        assert_involutory("{val = \"Some string\"}");
        assert_involutory("{val = [\"a\", 3, []]}");
        assert_involutory("{a.foo.bar = \"2\", b = false, c = [{d = \"e\"}, {d = \"f\"}]}");
    }
}
