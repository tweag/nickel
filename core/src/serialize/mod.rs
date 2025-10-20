//! Serialization of an evaluated program to various data format.
use crate::{
    bytecode::value::{
        ArrayBody, EnumVariantBody, InlineValue, NickelValue, NumberBody, RecordBody, TermBody,
        ValueContentRef,
    },
    error::{ExportError, ExportErrorData, PointedExportErrorData},
    identifier::{Ident, LocIdent},
    metrics,
    term::{IndexMap, Number, Term, TypeAnnotation, record::RecordData},
};

use serde::{
    de::{Deserialize, Deserializer},
    ser::{Serialize, SerializeMap, SerializeSeq, Serializer},
};

use malachite::base::{
    num::{
        basic::floats::PrimitiveFloat,
        conversion::traits::{IsInteger, RoundingFrom},
    },
    rounding_modes::RoundingMode,
};
use once_cell::sync::Lazy;

use std::{fmt, io};

pub mod yaml;

/// Available export formats.
#[derive(Copy, Clone, Eq, PartialEq, Debug, Default)]
#[cfg_attr(feature = "clap", derive(clap::ValueEnum))]
pub enum ExportFormat {
    /// Evalute a Nickel expression to a string and write that text to the output
    /// Note: `raw` is a deprecated alias for `text`; prefer `text` instead.
    #[cfg_attr(feature = "clap", value(alias("raw")))]
    Text,
    #[default]
    Json,
    Yaml,
    Toml,
}

impl fmt::Display for ExportFormat {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Text => write!(f, "text"),
            Self::Json => write!(f, "json"),
            Self::Yaml => write!(f, "yaml"),
            Self::Toml => write!(f, "toml"),
        }
    }
}

/// Available metadata export formats.
#[derive(Copy, Clone, Eq, PartialEq, Debug, Default)]
#[cfg_attr(feature = "clap", derive(clap::ValueEnum))]
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
    t: &NickelValue,
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

impl Serialize for NickelValue {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self.content_ref() {
            ValueContentRef::Inline(InlineValue::Null) => serializer.serialize_none(),
            ValueContentRef::Inline(InlineValue::True) => serializer.serialize_bool(true),
            ValueContentRef::Inline(InlineValue::False) => serializer.serialize_bool(false),
            ValueContentRef::Inline(InlineValue::EmptyArray) => {
                let seq_ser = serializer.serialize_seq(Some(0))?;
                seq_ser.end()
            }
            ValueContentRef::Inline(InlineValue::EmptyRecord) => {
                let map_ser = serializer.serialize_map(Some(0))?;
                map_ser.end()
            }
            ValueContentRef::Number(NumberBody(n)) => serialize_num(n, serializer),
            ValueContentRef::String(s) => serializer.serialize_str(&s.0),
            ValueContentRef::EnumVariant(EnumVariantBody { tag, arg: None }) => {
                serializer.serialize_str(tag.label())
            }
            ValueContentRef::EnumVariant(EnumVariantBody { tag, arg: Some(_) }) => {
                Err(serde::ser::Error::custom(format!(
                    "cannot serialize enum variant `'{tag}` with non-empty argument"
                )))
            }
            ValueContentRef::Record(RecordBody(record)) => serialize_record(record, serializer),
            ValueContentRef::Array(ArrayBody { array, .. }) => {
                let mut seq_ser = serializer.serialize_seq(Some(array.len()))?;
                for elt in array.iter() {
                    seq_ser.serialize_element(elt)?
                }
                seq_ser.end()
            }
            _ => Err(serde::ser::Error::custom(format!(
                "cannot serialize non-fully evaluated terms of type {}",
                self.type_of().unwrap_or("unknown")
            ))),
        }
    }
}

// This macro generates boilerplate visitors for the various serde number types to be included in
// the `NickelValue` deserialize implementation.
macro_rules! def_number_visitor {
    ($($ty:ty),*) => {
        $(
            paste::paste! {
                fn [<visit_ $ty>]<E>(self, v: $ty) -> Result<Self::Value, E>
                where
                    E: serde::de::Error,
                {
                    Ok(NickelValue::number_posless(v))
                }
            }
        )*
    };
}

impl<'de> Deserialize<'de> for NickelValue {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct NickelValueVisitor;

        impl<'de> serde::de::Visitor<'de> for NickelValueVisitor {
            type Value = NickelValue;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a valid Nickel value")
            }

            fn visit_bool<E>(self, v: bool) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(NickelValue::bool_value_posless(v))
            }

            def_number_visitor!(i8, u8, i16, u16, i32, u32, i64, u64, i128, u128);

            fn visit_f32<E>(self, v: f32) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                let n = number_from_float::<f32, E>(v)?;
                Ok(NickelValue::number_posless(n))
            }

            fn visit_f64<E>(self, v: f64) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                let n = number_from_float::<f64, E>(v)?;
                Ok(NickelValue::number_posless(n))
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(NickelValue::string_posless(v))
            }

            fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(NickelValue::string_posless(v))
            }

            fn visit_none<E>(self) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(NickelValue::null())
            }

            fn visit_unit<E>(self) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(NickelValue::null())
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::SeqAccess<'de>,
            {
                let mut elts = Vec::with_capacity(seq.size_hint().unwrap_or(0));

                while let Some(elem) = seq.next_element()? {
                    elts.push(elem);
                }

                Ok(NickelValue::array_posless(
                    elts.into_iter().collect(),
                    Vec::new(),
                ))
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::MapAccess<'de>,
            {
                let mut fields = IndexMap::with_capacity(map.size_hint().unwrap_or(0));

                while let Some((key, value)) = map.next_entry::<String, NickelValue>()? {
                    fields.insert(key.into(), value);
                }

                Ok(NickelValue::record_posless(RecordData::with_field_values(
                    fields,
                )))
            }
        }

        deserializer.deserialize_any(NickelValueVisitor)
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
            NickelPointerElem::Field(id) => write!(f, "{id}"),
            NickelPointerElem::Index(i) => write!(f, "[{i}]"),
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
pub fn validate(format: ExportFormat, value: &NickelValue) -> Result<(), PointedExportErrorData> {
    // The max and min value that we accept to serialize as a number. Because Nickel uses arbitrary
    // precision rationals, we could actually support a wider range of numbers, but we expect that
    // implementations consuming the resulting JSON (or similar formats) won't necessary be able to
    // handle values that don't fit in a 64 bits float.
    static NUMBER_MIN: Lazy<Number> = Lazy::new(|| Number::try_from(f64::MIN).unwrap());
    static NUMBER_MAX: Lazy<Number> = Lazy::new(|| Number::try_from(f64::MAX).unwrap());

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
    // prediction should be able to cancel the error code path.
    //
    // `do_validate` is the method doing the actual validation. The only reason this code is put in
    // a separate subfunction is that since we reconstruct the path bottom-up, it needs to be
    // reversed before finally returning from validate.
    fn do_validate(
        format: ExportFormat,
        value: &NickelValue,
    ) -> Result<(), PointedExportErrorData> {
        match value.content_ref() {
            // TOML doesn't support null values
            ValueContentRef::Inline(InlineValue::Null)
                if format == ExportFormat::Json || format == ExportFormat::Yaml =>
            {
                Ok(())
            }
            ValueContentRef::Inline(InlineValue::Null) => {
                Err(ExportErrorData::UnsupportedNull(format, value.clone()).into())
            }
            ValueContentRef::Inline(_) => Ok(()),
            ValueContentRef::String(_) => Ok(()),
            ValueContentRef::EnumVariant(EnumVariantBody { arg: None, .. }) => Ok(()),
            ValueContentRef::EnumVariant(EnumVariantBody { arg: Some(_), .. }) => {
                Err(ExportErrorData::NonSerializable(value.clone()).into())
            }
            ValueContentRef::Number(NumberBody(n)) => {
                if *n >= *NUMBER_MIN && *n <= *NUMBER_MAX {
                    Ok(())
                } else {
                    Err(ExportErrorData::NumberOutOfRange {
                        term: value.clone(),
                        value: n.clone(),
                    }
                    .into())
                }
            }
            ValueContentRef::Record(RecordBody(record)) => {
                record.iter_serializable().try_for_each(|binding| {
                    // unwrap(): terms must be fully evaluated before being validated for
                    // serialization. Otherwise, it's an internal error.
                    let (id, value) = binding.unwrap_or_else(|err| {
                        panic!(
                            "encountered field without definition `{}` \
                            during pre-serialization validation",
                            err.id
                        )
                    });

                    do_validate(format, value)
                        .map_err(|err| err.with_elem(NickelPointerElem::Field(id)))
                })?;
                Ok(())
            }
            ValueContentRef::Array(array_body) => {
                array_body
                    .array
                    .iter()
                    .enumerate()
                    .try_for_each(|(index, val)| {
                        do_validate(format, val)
                            .map_err(|err| err.with_elem(NickelPointerElem::Index(index)))
                    })?;
                Ok(())
            }
            // Not sure if we should allow this. But supporting wrapped values might alleviate the
            // pre-processing substitution work to be done upfront.
            ValueContentRef::Term(TermBody(term)) => {
                if let Term::Value(nickel_val) = term {
                    do_validate(format, nickel_val)
                } else {
                    Err(ExportErrorData::NonSerializable(value.clone()).into())
                }
            }
            _ => Err(ExportErrorData::NonSerializable(value.clone()).into()),
        }
    }

    if format == ExportFormat::Text {
        if value.as_string().is_some() {
            Ok(())
        } else {
            Err(ExportErrorData::NotAString(value.clone()).into())
        }
    } else {
        let mut result = do_validate(format, value);

        if let Err(PointedExportErrorData { path, .. }) = &mut result {
            path.0.reverse();
        }

        result
    }
}

pub fn to_writer_metadata<W, T>(
    mut writer: W,
    format: MetadataExportFormat,
    item: &T,
) -> Result<(), PointedExportErrorData>
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

pub fn to_writer<W>(
    mut writer: W,
    format: ExportFormat,
    value: &NickelValue,
) -> Result<(), PointedExportErrorData>
where
    W: io::Write,
{
    #[cfg(feature = "metrics")]
    let start_time = std::time::Instant::now();

    match format {
        ExportFormat::Json => serde_json::to_writer_pretty(writer, &value)
            .map_err(|err| ExportErrorData::Other(err.to_string())),
        ExportFormat::Yaml => serde_yaml::to_writer(writer, &value)
            .map_err(|err| ExportErrorData::Other(err.to_string())),
        ExportFormat::Toml => toml::to_string_pretty(value)
            .map_err(|err| ExportErrorData::Other(err.to_string()))
            .and_then(|s| {
                writer
                    .write_all(s.as_bytes())
                    .map_err(|err| ExportErrorData::Other(err.to_string()))
            }),
        ExportFormat::Text => match value.as_string().map(|s| &s.0) {
            Some(s) => writer
                .write_all(s.as_bytes())
                .map_err(|err| ExportErrorData::Other(err.to_string())),
            _ => Err(ExportErrorData::Other(format!(
                "raw export requires a `String`, got {}",
                // unwrap(): terms must be fully evaluated before serialization,
                // and fully evaluated terms have a definite type.
                value.type_of().unwrap()
            ))),
        },
    }?;

    metrics::increment!("runtime:serialize", start_time.elapsed().as_millis() as u64);

    Ok(())
}

pub fn to_string(format: ExportFormat, rt: &NickelValue) -> Result<String, PointedExportErrorData> {
    let mut buffer: Vec<u8> = Vec::new();
    to_writer(&mut buffer, format, rt)?;

    Ok(String::from_utf8_lossy(&buffer).into_owned())
}

/// We don't use serde to deserialize toml, because
/// - this bug: <https://github.com/toml-rs/toml/issues/798>
/// - the machinery for getting spans for toml+serde is more code than
///   what we have below
///
/// Instead, we parse the toml using `toml-edit` and then convert from their
/// representation to a `NickelValue`. Using `toml-edit` here doesn't introduce
/// any new dependencies, because it's used by `toml` internally anyway.
pub mod toml_deser {
    use crate::{
        bytecode::value::NickelValue,
        files::FileId,
        identifier::LocIdent,
        position::{PosTable, RawSpan, TermPos},
        term::record::{RecordAttrs, RecordData},
    };
    use codespan::ByteIndex;
    use malachite::{base::num::conversion::traits::ExactFrom as _, rational::Rational};
    use std::ops::Range;
    use toml_edit::Value;

    fn range_pos(range: Option<Range<usize>>, src_id: FileId) -> TermPos {
        range.map_or(TermPos::None, |span| {
            RawSpan {
                src_id,
                start: ByteIndex(span.start as u32),
                end: ByteIndex(span.end as u32),
            }
            .into()
        })
    }

    // Add `to_value` method to `toml_edit` types.
    trait ToNickelValue {
        fn to_value(&self, pos_table: &mut PosTable, src_id: FileId) -> NickelValue;

        fn to_value_with_pos(
            &self,
            pos_table: &mut PosTable,
            range: Option<Range<usize>>,
            src_id: FileId,
        ) -> NickelValue {
            let pos = range_pos(range, src_id);
            self.to_value(pos_table, src_id).with_pos(pos_table, pos)
        }
    }

    impl ToNickelValue for toml_edit::Table {
        fn to_value(&self, pos_table: &mut PosTable, src_id: FileId) -> NickelValue {
            NickelValue::record_posless(RecordData::new(
                self.iter()
                    .map(|(key, val)| {
                        (
                            LocIdent::new(key),
                            val.to_value_with_pos(pos_table, val.span(), src_id).into(),
                        )
                    })
                    .collect(),
                RecordAttrs::default(),
                None,
            ))
        }
    }

    impl ToNickelValue for toml_edit::Value {
        fn to_value(&self, pos_table: &mut PosTable, src_id: FileId) -> NickelValue {
            match self {
                Value::String(s) => NickelValue::string_posless(s.value()),
                Value::Integer(i) => NickelValue::number_posless(*i.value()),
                Value::Float(f) => NickelValue::number_posless(Rational::exact_from(*f.value())),
                Value::Boolean(b) => NickelValue::bool_value_posless(*b.value()),
                Value::Array(vs) => NickelValue::array_posless(
                    vs.iter()
                        .map(|val| val.to_value_with_pos(pos_table, val.span(), src_id))
                        .collect(),
                    Vec::new(),
                ),
                Value::InlineTable(t) => NickelValue::record_posless(RecordData::new(
                    t.iter()
                        .map(|(key, val)| {
                            (
                                LocIdent::new(key),
                                val.to_value_with_pos(pos_table, val.span(), src_id).into(),
                            )
                        })
                        .collect(),
                    RecordAttrs::default(),
                    None,
                )),
                // We don't have a proper type to represent datetimes currently, so we just parse
                // this as a string.
                Value::Datetime(dt) => NickelValue::string_posless(dt.to_string()),
            }
        }
    }

    impl ToNickelValue for toml_edit::Item {
        fn to_value(&self, pos_table: &mut PosTable, src_id: FileId) -> NickelValue {
            match self {
                toml_edit::Item::None => NickelValue::null(),
                toml_edit::Item::Table(t) => t.to_value(pos_table, src_id),
                toml_edit::Item::ArrayOfTables(ts) => NickelValue::array_posless(
                    ts.iter()
                        .map(|val| val.to_value_with_pos(pos_table, val.span(), src_id))
                        .collect(),
                    Vec::new(),
                ),
                toml_edit::Item::Value(v) => v.to_value(pos_table, src_id),
            }
        }
    }

    /// Deserialize a Nickel term with position information from a TOML source provided as a
    /// string and the file id of this source.
    pub fn from_str(
        pos_table: &mut PosTable,
        s: &str,
        file_id: FileId,
    ) -> Result<NickelValue, toml_edit::TomlError> {
        let doc: toml_edit::Document<_> = s.parse()?;
        Ok(doc
            .as_item()
            .to_value_with_pos(pos_table, doc.span(), file_id))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        cache::resolvers::DummyResolver,
        error::NullReporter,
        eval::{VirtualMachine, VmContext, cache::CacheImpl},
        program::Program,
        term::{BinaryOp, make as mk_term},
    };
    use serde_json::json;
    use std::io::Cursor;

    fn eval(s: &str) -> NickelValue {
        let src = Cursor::new(s);
        let mut prog = Program::<CacheImpl>::new_from_source(
            src,
            "<test>",
            std::io::stderr(),
            NullReporter {},
        )
        .unwrap();
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
    fn assert_nickel_eq(value: NickelValue, expected: NickelValue) {
        let mut vm_ctxt = VmContext::new(DummyResolver {}, std::io::stderr(), NullReporter {});

        assert!(
            VirtualMachine::<_, CacheImpl>::new_empty_env(&mut vm_ctxt)
                .eval(mk_term::op2(BinaryOp::Eq, value, expected))
                .unwrap()
                .phys_eq(&NickelValue::bool_true())
        );
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
        let from_json: NickelValue =
            serde_json::from_str(&serde_json::to_string(&evaluated).unwrap()).unwrap();
        let from_yaml: NickelValue =
            serde_yaml::from_str(&serde_yaml::to_string(&evaluated).unwrap()).unwrap();
        let from_toml: NickelValue = toml::from_str(&toml::to_string(&evaluated).unwrap()).unwrap();

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
