//! Serialization of an evaluated program to various data format.
use crate::{
    error::{ExportError, ExportErrorData},
    identifier::{Ident, LocIdent},
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
    num::{
        basic::floats::PrimitiveFloat,
        conversion::traits::{IsInteger, RoundingFrom},
    },
    rounding_modes::RoundingMode,
};
use once_cell::sync::Lazy;

use std::{fmt, io};

/// Available export formats.
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

/// Available metadata export formats.
#[derive(Copy, Clone, Eq, PartialEq, Debug, Default, clap::ValueEnum)]
pub enum MetadataExportFormat {
    #[default]
    Markdown,
    Json,
    Yaml,
    Toml,
}

impl fmt::Display for MetadataExportFormat {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Markdown => write!(f, "markdown"),
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

/// Helper function to convert a serialized primitive float to a Nickel number. Return a deserialize error if
/// the floating point value is either NaN or infinity.
fn number_from_float<F: PrimitiveFloat, E: serde::de::Error>(float_value: F) -> Result<Number, E>
where
    Number: TryFrom<
        F,
        Error = malachite_q::conversion::from_primitive_float::RationalFromPrimitiveFloatError,
    >,
{
    Number::try_from_float_simplest(float_value).map_err(|_| {
        E::custom(format!(
            "couldn't convert {float_value} to a Nickel number: Nickel doesn't support NaN nor infinity"
        ))
    })
}

/// Deserialize a Nickel number. As for parsing, we convert the number from a 64bits float.
pub fn deserialize_num<'de, D>(deserializer: D) -> Result<Number, D::Error>
where
    D: Deserializer<'de>,
{
    let as_f64 = f64::deserialize(deserializer)?;
    number_from_float::<f64, D::Error>(as_f64)
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
    let terms = Array::deserialize(deserializer)?;
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

/// Element of a path to a specific value within a serialized term. See [NickelPointer].
#[derive(Debug, PartialEq, Clone)]
pub enum NickelPointerElem {
    Field(Ident),
    Index(usize),
}

impl fmt::Display for NickelPointerElem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NickelPointerElem::Field(id) => write!(f, "{}", id),
            NickelPointerElem::Index(i) => write!(f, "[{}]", i),
        }
    }
}

/// A pointer to a specific value within a serialized term. The name is inspired from [JSON
/// pointer](https://datatracker.ietf.org/doc/html/rfc6901), which is an equivalent notion.
///
/// In a serialized term, there can only be constants (numbers, strings, booleans, null) and two
/// kinds of containers: records and lists. To locate a particular value within a serialized term,
/// we can use a path of field names and indices.
///
/// # Example
///
/// In the following full evaluated program:
///
/// ```nickel
/// {
///   foo = {
///     bar = ["hello", "world"],
///     other = null,
///   }
/// }
/// ```
///
/// The path to the string `"world"` is `[Field("foo"), Field("bar"), Index(1)]`. This is
/// represented (e.g. in error messages) as `foo.bar[1]`.
#[derive(Debug, PartialEq, Clone, Default)]
pub struct NickelPointer(pub Vec<NickelPointerElem>);

impl NickelPointer {
    pub fn new() -> Self {
        Self::default()
    }
}

impl fmt::Display for NickelPointer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut it = self.0.iter();
        let Some(first) = it.next() else {
            return Ok(());
        };

        write!(f, "{first}")?;

        for elem in it {
            if let NickelPointerElem::Field(_) = elem {
                write!(f, ".{elem}")?
            } else {
                write!(f, "{elem}")?
            }
        }

        Ok(())
    }
}

/// Check that a term is serializable. Serializable terms are booleans, numbers, strings, enum,
/// arrays of serializable terms or records of serializable terms.
pub fn validate(format: ExportFormat, t: &RichTerm) -> Result<(), ExportError> {
    use Term::*;

    // The max and min value that we accept to serialize as a number. Because Nickel uses arbitrary
    // precision rationals, we could actually support a wider range of numbers, but we expect that
    // implementations consuming the resulting JSON (or similar formats) won't necessary be able to
    // handle values that don't fit in a 64 bits float.
    static NUMBER_MIN: Lazy<Number> = Lazy::new(|| Number::try_from(f64::MIN).unwrap());
    static NUMBER_MAX: Lazy<Number> = Lazy::new(|| Number::try_from(f64::MAX).unwrap());

    // Push an NickelPoinerElem to the end of the path of an ExportError
    fn with_elem(mut err: ExportError, elem: NickelPointerElem) -> ExportError {
        err.path.0.push(elem);
        err
    }

    // We need to build a field path locating a potential export error. One way would be to pass a
    // context storing the current path to recursive calls of `validate`. However, representing
    // this context isn't entirely trivial: using an owned `Vec` will incur a lot of copying, even
    // in the happy path (no error), because the current path needs to be shared among all sibling
    // fields of a record. What we want is persistent linked list (cheap to clone and to extend),
    // but Rust doesn't have a built-in type for that. It's not very hard to implement manually, or
    // to use an external crate, but it's still more code.
    //
    // Instead, what we do is to return the current field in case of an error. Then, recursive
    // calls to `do_validate` can unwrap the error and add their own field to the path, so that
    // when the chain of recursive calls finally returns, we have reconstructed the full path (in
    // some sense, we're encoding the list in the OS stack). Not only this doesn't require any
    // additional data structure, but we expect that it's performant, as in the happy path branch
    // prediction should be able to negate the error code path.
    //
    // `do_validate` is the method doing the actual validation. The only reason this code is put in
    // a separate subfunction is that since we reconstruct the path bottom-up, it needs to be
    // reversed before finally returning from validate.
    fn do_validate(format: ExportFormat, t: &RichTerm) -> Result<(), ExportError> {
        match t.as_ref() {
            // TOML doesn't support null values
            Null if format == ExportFormat::Json || format == ExportFormat::Yaml => Ok(()),
            Null => Err(ExportErrorData::UnsupportedNull(format, t.clone()).into()),
            Bool(_) | Str(_) | Enum(_) => Ok(()),
            Num(n) => {
                if *n >= *NUMBER_MIN && *n <= *NUMBER_MAX {
                    Ok(())
                } else {
                    Err(ExportErrorData::NumberOutOfRange {
                        term: t.clone(),
                        value: n.clone(),
                    }
                    .into())
                }
            }
            Record(record) => {
                record.iter_serializable().try_for_each(|binding| {
                    // unwrap(): terms must be fully evaluated before being validated for
                    // serialization. Otherwise, it's an internal error.
                    let (id, rt) = binding.unwrap_or_else(|err| {
                        panic!(
                            "encountered field without definition `{}` \
                            during pre-serialization validation",
                            err.id
                        )
                    });

                    do_validate(format, rt)
                        .map_err(|err| with_elem(err, NickelPointerElem::Field(id)))
                })?;
                Ok(())
            }
            Array(array, _) => {
                array.iter().enumerate().try_for_each(|(index, t)| {
                    do_validate(format, t)
                        .map_err(|err| with_elem(err, NickelPointerElem::Index(index)))
                })?;
                Ok(())
            }
            _ => Err(ExportErrorData::NonSerializable(t.clone()).into()),
        }
    }

    if format == ExportFormat::Raw {
        if let Term::Str(_) = t.term.as_ref() {
            Ok(())
        } else {
            Err(ExportErrorData::NotAString(t.clone()).into())
        }
    } else {
        let mut result = do_validate(format, t);

        if let Err(ExportError { path, .. }) = &mut result {
            path.0.reverse();
        }

        result
    }
}

pub fn to_writer_metadata<W, T>(
    mut writer: W,
    format: MetadataExportFormat,
    item: &T,
) -> Result<(), ExportError>
where
    W: io::Write,
    T: ?Sized + Serialize,
{
    // This is a near-verbatim copy of `to_writer`
    match format {
        MetadataExportFormat::Markdown => unimplemented!(),
        MetadataExportFormat::Json => serde_json::to_writer_pretty(writer, &item)
            .map_err(|err| ExportErrorData::Other(err.to_string())),
        MetadataExportFormat::Yaml => serde_yaml::to_writer(writer, &item)
            .map_err(|err| ExportErrorData::Other(err.to_string())),
        MetadataExportFormat::Toml => toml::to_string_pretty(item)
            .map_err(|err| ExportErrorData::Other(err.to_string()))
            .and_then(|s| {
                writer
                    .write_all(s.as_bytes())
                    .map_err(|err| ExportErrorData::Other(err.to_string()))
            }),
    }?;

    Ok(())
}

pub fn to_writer<W>(mut writer: W, format: ExportFormat, rt: &RichTerm) -> Result<(), ExportError>
where
    W: io::Write,
{
    match format {
        ExportFormat::Json => serde_json::to_writer_pretty(writer, &rt)
            .map_err(|err| ExportErrorData::Other(err.to_string())),
        ExportFormat::Yaml => serde_yaml::to_writer(writer, &rt)
            .map_err(|err| ExportErrorData::Other(err.to_string())),
        ExportFormat::Toml => toml::to_string_pretty(rt)
            .map_err(|err| ExportErrorData::Other(err.to_string()))
            .and_then(|s| {
                writer
                    .write_all(s.as_bytes())
                    .map_err(|err| ExportErrorData::Other(err.to_string()))
            }),
        ExportFormat::Raw => match rt.as_ref() {
            Term::Str(s) => writer
                .write_all(s.as_bytes())
                .map_err(|err| ExportErrorData::Other(err.to_string())),
            t => Err(ExportErrorData::Other(format!(
                "raw export requires a `String`, got {}",
                // unwrap(): terms must be fully evaluated before serialization,
                // and fully evaluated terms have a definite type.
                t.type_of().unwrap()
            ))),
        },
    }?;

    Ok(())
}

pub fn to_string(format: ExportFormat, rt: &RichTerm) -> Result<String, ExportError> {
    let mut buffer: Vec<u8> = Vec::new();
    to_writer(&mut buffer, format, rt)?;

    Ok(String::from_utf8_lossy(&buffer).into_owned())
}

/// TOML deserialization wrappers. Depending on the `spanned-deser` feature being
/// enabled, [::toml::from_str] will either simply call [toml::from_str] or a custom
/// deserializer that preserves span information.
pub mod toml_deser {
    use super::PrimitiveFloat;
    use crate::term::RichTerm;
    use codespan::FileId;

    /// Utility module for spanned deserialization which preserves position information for some
    /// parsers that support it. Although we haven't measured the effect, it's likely that
    /// including span information affects performance and binary size (by pulling in alternate
    /// parsers supporting spanned deserialization or using YAML parser for JSON deserialization),
    /// so we gate this functionality behind the `spanned-deser` feature flag. It's still
    /// enabled by default, being useful for error reporting, but can be disabled when building a
    /// lightweight version of the interpreter e.g. to be embedded in a production environment.
    #[cfg(feature = "spanned-deser")]
    pub mod spanned {
        use super::*;

        use crate::{
            identifier::LocIdent,
            position::RawSpan,
            term::{record::RecordData, string::NickelString, Number, Term},
        };

        use serde::{
            de::{Deserializer, MapAccess},
            Deserialize,
        };
        use serde_untagged::UntaggedEnumVisitor;

        use toml::Spanned;

        /// The inner data of a TOML value with span information. Similar to
        /// `toml::Spanned<toml::Value>`, but where atoms use Nickel types instead (`NickelString`,
        /// `Number`, etc)
        #[derive(Debug, Clone)]
        pub enum SpannedValueData {
            String(NickelString),
            Number(Number),
            Boolean(bool),
            Array(Vec<SpannedValue>),
            /// Since we are going to iterate through the map and collect it in a new data
            /// structure to fit Nickel's record representation anyway, we store maps as vectors of
            /// entries instead of building an actual map.
            Map(Vec<(Spanned<String>, SpannedValue)>),
        }

        /// A TOML value with span information.
        ///
        /// After some experimentation, it seems that in order to properly trigger spanned
        /// deserialization in the `toml` crate, we need to write the deserializers for both
        /// [SpannedValueData] and [SpannedValue] manually, although they are pretty mechanical and
        /// should in principle be similar to what `derive(Deserialize)` would generate. However,
        /// the latter doesn't work in practice (giving "data did not match any variant of untagged
        /// enum" errors when importing TOML).
        #[derive(Debug, Clone)]
        pub struct SpannedValue(Spanned<SpannedValueData>);

        impl<'de> Deserialize<'de> for SpannedValue {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: Deserializer<'de>,
            {
                let spanned: Spanned<SpannedValueData> = Deserialize::deserialize(deserializer)?;
                Ok(SpannedValue(spanned))
            }
        }

        impl<'de> Deserialize<'de> for SpannedValueData {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: Deserializer<'de>,
            {
                fn deser_int<I: Into<Number>, E>(n: I) -> Result<SpannedValueData, E> {
                    Ok(SpannedValueData::Number(n.into()))
                }

                fn deser_float<F : PrimitiveFloat>(f: F) -> Result<SpannedValueData, serde_untagged::de::Error>
                where Number: TryFrom<F, Error = malachite_q::conversion::from_primitive_float::RationalFromPrimitiveFloatError>{
                    let as_number =
                        crate::serialize::number_from_float::<F, serde_untagged::de::Error>(f)?;
                    Ok(SpannedValueData::Number(as_number))
                }

                let data = UntaggedEnumVisitor::new()
                    .string(|str| Ok(SpannedValueData::String(str.into())))
                    .i8(deser_int)
                    .i16(deser_int)
                    .i32(deser_int)
                    .i64(deser_int)
                    .i128(deser_int)
                    .u8(deser_int)
                    .u16(deser_int)
                    .u32(deser_int)
                    .u64(deser_int)
                    .u128(deser_int)
                    .f32(deser_float)
                    .f64(deser_float)
                    .bool(|b| Ok(SpannedValueData::Boolean(b)))
                    .seq(|seq| {
                        let v: Result<Vec<SpannedValue>, serde_untagged::de::Error> =
                            seq.deserialize();
                        Ok(SpannedValueData::Array(v?))
                    })
                    .map(|mut map| {
                        let mut result = if let Some(hint) = map.size_hint() {
                            Vec::with_capacity(hint)
                        } else {
                            Vec::new()
                        };

                        while let Some((k, v)) =
                            map.next_entry::<Spanned<String>, SpannedValue>()?
                        {
                            result.push((k, v));
                        }

                        Ok(SpannedValueData::Map(result))
                    })
                    .deserialize(deserializer)?;

                Ok(data)
            }
        }

        impl SpannedValue {
            /// Take the id of the current file and convert a spanned value to a Nickel term with
            /// position information properly set.
            pub fn into_term(self, file_id: FileId) -> RichTerm {
                let span = self.0.span();

                let term = match self.0.into_inner() {
                    SpannedValueData::String(s) => Term::Str(s),
                    SpannedValueData::Number(n) => Term::Num(n),
                    SpannedValueData::Boolean(b) => Term::Bool(b),
                    SpannedValueData::Array(v) => Term::Array(
                        v.into_iter().map(|v| v.into_term(file_id)).collect(),
                        Default::default(),
                    ),
                    SpannedValueData::Map(m) => {
                        let data = m
                            .into_iter()
                            .map(|(k, v)| {
                                let key_span = k.span();
                                let ident = LocIdent::new_with_pos(
                                    k.into_inner(),
                                    RawSpan::from_range(file_id, key_span).into(),
                                );

                                (ident, v.into_term(file_id).into())
                            })
                            .collect();

                        Term::Record(RecordData {
                            fields: data,
                            ..Default::default()
                        })
                    }
                };

                RichTerm::new(term, RawSpan::from_range(file_id, span).into())
            }
        }
    }

    /// Deserialize a Nickel term with position information from a TOML source provided as a
    /// string and the file id of this source.
    #[cfg(feature = "spanned-deser")]
    pub fn from_str(s: &str, file_id: FileId) -> Result<RichTerm, toml::de::Error> {
        let spanned: spanned::SpannedValue = toml::from_str(s)?;
        Ok(spanned.into_term(file_id))
    }

    #[cfg(not(feature = "spanned-deser"))]
    pub fn from_str(s: &str, _file_id: FileId) -> Result<RichTerm, toml::de::Error> {
        toml::from_str(s)
    }
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
                .eval(mk_term::op2(BinaryOp::Eq, term, expected))
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
