//! Deserialization of an evaluated program to plain Rust types.

use malachite::base::{num::conversion::traits::RoundingFrom, rounding_modes::RoundingMode};
use std::{ffi::OsString, io::Cursor, iter::ExactSizeIterator};

use serde::de::{
    Deserialize, DeserializeSeed, EnumAccess, IntoDeserializer, MapAccess, SeqAccess,
    VariantAccess, Visitor,
};

use crate::{
    bytecode::value::{
        Array, ArrayBody, Container, InlineValue, NickelValue, RecordBody, ValueContent,
        ValueContentRef,
    },
    error::{self, NullReporter},
    eval::cache::CacheImpl,
    identifier::LocIdent,
    program::{Input, Program},
    term::{IndexMap, record::Field},
};

macro_rules! deserialize_number {
    ($method:ident, $type:tt, $visit:ident) => {
        fn $method<V>(self, visitor: V) -> Result<V::Value, Self::Error>
        where
            V: Visitor<'de>,
        {
            match self.content_ref() {
                ValueContentRef::Number(n) => {
                    return visitor.$visit($type::rounding_from(&n.0, RoundingMode::Nearest).0);
                }
                _ => Err(RustDeserializationError::InvalidType {
                    expected: "Number".to_string(),
                    occurred: self.type_of().unwrap_or("Other").to_owned(),
                }),
            }
        }
    };
}

// TODO: revisit this lint once the large errors have been dealt with.
#[allow(clippy::large_enum_variant)]
pub enum EvalOrDeserError {
    Nickel {
        error: error::Error,
        files: Option<crate::files::Files>,
    },
    Deser(RustDeserializationError),
}

impl std::error::Error for EvalOrDeserError {}

impl std::fmt::Debug for EvalOrDeserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nickel { error, files } => {
                let filenames = files.as_ref().map(|f| f.filenames().collect::<Vec<_>>());
                f.debug_struct("Nickel")
                    .field("error", error)
                    .field("files", &filenames)
                    .finish()
            }
            Self::Deser(arg0) => f.debug_tuple("Deser").field(arg0).finish(),
        }
    }
}

impl std::fmt::Display for EvalOrDeserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EvalOrDeserError::Nickel { error, files } => {
                let mut files = files.clone().unwrap_or_default();
                write!(
                    f,
                    "{}",
                    crate::error::report::report_as_str(
                        &mut files,
                        error.clone(),
                        error::report::ColorOpt::Never
                    )
                )
            }
            EvalOrDeserError::Deser(e) => e.fmt(f),
        }
    }
}

impl EvalOrDeserError {
    fn new<E>(error: E, files: crate::files::Files) -> Self
    where
        E: Into<error::Error>,
    {
        Self::Nickel {
            error: error.into(),
            files: Some(files),
        }
    }
}

impl<E: Into<error::Error>> From<E> for EvalOrDeserError {
    fn from(err: E) -> Self {
        Self::Nickel {
            error: err.into(),
            files: None,
        }
    }
}

impl From<RustDeserializationError> for EvalOrDeserError {
    fn from(err: RustDeserializationError) -> Self {
        Self::Deser(err)
    }
}

fn from_input<'a, T, Content, Source>(input: Input<Content, Source>) -> Result<T, EvalOrDeserError>
where
    T: serde::Deserialize<'a>,
    Content: std::io::Read,
    Source: Into<OsString>,
{
    let mut program =
        Program::<CacheImpl>::new_from_input(input, std::io::stderr(), NullReporter {})
            .map_err(error::IOError::from)?;

    Ok(T::deserialize(program.eval_full_for_export().map_err(
        |err| EvalOrDeserError::new(err, program.files()),
    )?)?)
}

pub fn from_str<'a, T>(s: &'a str) -> Result<T, EvalOrDeserError>
where
    T: serde::Deserialize<'a>,
{
    from_input(Input::Source(Cursor::new(s), "string"))
}

pub fn from_slice<'a, T>(v: &'a [u8]) -> Result<T, EvalOrDeserError>
where
    T: serde::Deserialize<'a>,
{
    from_input(Input::Source(Cursor::new(v), "slice"))
}

pub fn from_path<T>(path: impl Into<OsString>) -> Result<T, EvalOrDeserError>
where
    T: serde::de::DeserializeOwned,
{
    from_input(Input::<std::fs::File, _>::Path(path))
}

pub fn from_reader<R, T>(rdr: R) -> Result<T, EvalOrDeserError>
where
    R: std::io::Read,
    T: serde::de::DeserializeOwned,
{
    from_input(Input::Source(rdr, "reader"))
}

/// An error occurred during deserialization to Rust.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum RustDeserializationError {
    InvalidType { expected: String, occurred: String },
    MissingValue,
    EmptyRecordField,
    UnimplementedType { occurred: String },
    InvalidRecordLength(usize),
    InvalidArrayLength(usize),
    Other(String),
}

impl<'de> serde::Deserializer<'de> for NickelValue {
    type Error = RustDeserializationError;

    /// Catch-all deserialization
    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.content() {
            ValueContent::Null(_) => visitor.visit_unit(),
            ValueContent::Bool(lens) => visitor.visit_bool(lens.take()),
            ValueContent::Number(lens) => {
                visitor.visit_f64(f64::rounding_from(lens.take().0, RoundingMode::Nearest).0)
            }
            ValueContent::String(lens) => visitor.visit_string(lens.take().0.into_inner()),
            ValueContent::EnumVariant(lens) => {
                let body = lens.take();

                visitor.visit_enum(EnumDeserializer {
                    tag: body.tag.into_label(),
                    value: body.arg,
                })
            }
            ValueContent::Record(lens) => visit_record_container(lens.take(), visitor),
            ValueContent::Array(lens) => visit_array_container(lens.take(), visitor),
            lens => Err(RustDeserializationError::UnimplementedType {
                occurred: lens.restore().type_of().unwrap_or("Other").to_owned(),
            }),
        }
    }

    deserialize_number!(deserialize_i8, i8, visit_i8);
    deserialize_number!(deserialize_i16, i16, visit_i16);
    deserialize_number!(deserialize_i32, i32, visit_i32);
    deserialize_number!(deserialize_i64, i64, visit_i64);
    deserialize_number!(deserialize_i128, i128, visit_i128);
    deserialize_number!(deserialize_u8, u8, visit_u8);
    deserialize_number!(deserialize_u16, u16, visit_u16);
    deserialize_number!(deserialize_u32, u32, visit_u32);
    deserialize_number!(deserialize_u64, u64, visit_u64);
    deserialize_number!(deserialize_u128, u128, visit_u128);
    deserialize_number!(deserialize_f32, f32, visit_f32);
    deserialize_number!(deserialize_f64, f64, visit_f64);

    /// Deserialize nullable field.
    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if self.is_null() {
            visitor.visit_none()
        } else {
            visitor.visit_some(self)
        }
    }

    /// deserialize an enum variant or a record with a single item.
    fn deserialize_enum<V>(
        self,
        _name: &str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let (tag, arg) = match self.content() {
            ValueContent::EnumVariant(lens) => {
                let body = lens.take();

                (body.tag.into_label(), body.arg)
            }
            ValueContent::Record(lens) => {
                let record = lens.take().unwrap_or_alloc().0;

                let mut iter = record.fields.into_iter();
                let (tag, arg) = match iter.next() {
                    Some((id, Field { value, .. })) => (id, value),
                    None => {
                        return Err(RustDeserializationError::InvalidType {
                            expected: "Record with single key".to_string(),
                            occurred: "Record without keys".to_string(),
                        });
                    }
                };

                if iter.next().is_some() {
                    return Err(RustDeserializationError::InvalidType {
                        expected: "Record with single key".to_string(),
                        occurred: "Record with multiple keys".to_string(),
                    });
                }

                (tag.into_label(), arg)
            }
            lens => {
                let value = lens.restore();

                return Err(RustDeserializationError::InvalidType {
                    expected: "Enum or Record".to_string(),
                    occurred: value.type_of().unwrap_or("Other").to_owned(),
                });
            }
        };

        visitor.visit_enum(EnumDeserializer { tag, value: arg })
    }

    /// Deserialize pass-through tuples/structs.
    fn deserialize_newtype_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_newtype_struct(self)
    }

    /// Deserialize a boolean value.
    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.as_inline() {
            Some(InlineValue::True) => visitor.visit_bool(true),
            Some(InlineValue::False) => visitor.visit_bool(false),
            _ => Err(RustDeserializationError::InvalidType {
                expected: "Bool".to_string(),
                occurred: self.type_of().unwrap_or("Other").to_owned(),
            }),
        }
    }

    /// Deserialize a string as a char.
    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_string(visitor)
    }

    /// Deserialize a string (borrowed).
    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.as_string() {
            Some(s) => visitor.visit_str(&s.0),
            _ => Err(RustDeserializationError::InvalidType {
                expected: "Str".to_string(),
                occurred: self.type_of().unwrap_or("Other").to_owned(),
            }),
        }
    }

    /// Deserialize a string (owned).
    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.content() {
            ValueContent::String(lens) => visitor.visit_string(lens.take().0.into_inner()),
            lens => Err(RustDeserializationError::InvalidType {
                expected: "Str".to_string(),
                occurred: lens.restore().type_of().unwrap_or("Other").to_owned(),
            }),
        }
    }

    /// Deserialize a string value or an array as bytes.
    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_byte_buf(visitor)
    }

    /// Deserialize a string value or an array as bytes.
    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.content() {
            ValueContent::String(lens) => visitor.visit_string(lens.take().0.into_inner()),
            ValueContent::Array(lens) => visit_array(lens.take().unwrap_or_alloc().array, visitor),
            lens => {
                let value = lens.restore();

                Err(RustDeserializationError::InvalidType {
                    expected: "Str or Array".to_string(),
                    occurred: value.type_of().unwrap_or("Other").to_owned(),
                })
            }
        }
    }

    /// Deserialize `null` as `()`.
    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.as_inline() {
            Some(InlineValue::Null) => visitor.visit_unit(),
            _ => Err(RustDeserializationError::InvalidType {
                expected: "Null".to_string(),
                occurred: self.type_of().unwrap_or("Other").to_owned(),
            }),
        }
    }

    /// Deserialize `null` as a unit struct.
    fn deserialize_unit_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_unit(visitor)
    }

    /// Deserialize an array as `Vec<T>`.
    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.content() {
            ValueContent::Array(lens) => visit_array_container(lens.take(), visitor),
            lens => Err(RustDeserializationError::InvalidType {
                expected: "Array".to_string(),
                occurred: lens.restore().type_of().unwrap_or("Other").to_owned(),
            }),
        }
    }

    /// Deserialize an array as a tuple.
    fn deserialize_tuple<V>(self, _len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    /// Deserialize an array as a tuple struct.
    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    /// Deserialize a record as a map.
    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.content() {
            ValueContent::Record(lens) => visit_record_container(lens.take(), visitor),
            lens => Err(RustDeserializationError::InvalidType {
                expected: "Record".to_string(),
                occurred: lens.restore().type_of().unwrap_or("Other").to_owned(),
            }),
        }
    }

    /// Deserialize a record as a struct.
    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.content() {
            ValueContent::Array(lens) => visit_array_container(lens.take(), visitor),
            ValueContent::Record(lens) => visit_record_container(lens.take(), visitor),
            lens => {
                let value = lens.restore();

                Err(RustDeserializationError::InvalidType {
                    expected: "Record".to_string(),
                    occurred: value.type_of().unwrap_or("Other").to_owned(),
                })
            }
        }
    }

    /// Deserialize an indentifier as a string.
    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_string(visitor)
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        drop(self);
        visitor.visit_unit()
    }
}

struct ArrayDeserializer {
    iter: <Array as IntoIterator>::IntoIter,
}

impl ArrayDeserializer {
    fn new(array: Array) -> Self {
        ArrayDeserializer {
            iter: array.into_iter(),
        }
    }
}

impl<'de> SeqAccess<'de> for ArrayDeserializer {
    type Error = RustDeserializationError;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        match self.iter.next() {
            Some(value) => seed.deserialize(value).map(Some),
            None => Ok(None),
        }
    }

    fn size_hint(&self) -> Option<usize> {
        match self.iter.size_hint() {
            (lower, Some(upper)) if lower == upper => Some(upper),
            _ => None,
        }
    }
}

fn visit_array_container<'de, V>(
    container: Container<ArrayBody>,
    visitor: V,
) -> Result<V::Value, RustDeserializationError>
where
    V: Visitor<'de>,
{
    visit_array(container.unwrap_or_alloc().array, visitor)
}

fn visit_array<'de, V>(array: Array, visitor: V) -> Result<V::Value, RustDeserializationError>
where
    V: Visitor<'de>,
{
    let len = array.len();
    let mut deserializer = ArrayDeserializer::new(array);
    let seq = visitor.visit_seq(&mut deserializer)?;
    if deserializer.iter.next().is_none() {
        Ok(seq)
    } else {
        Err(RustDeserializationError::InvalidArrayLength(len))
    }
}

struct RecordDeserializer {
    iter: <IndexMap<LocIdent, Field> as IntoIterator>::IntoIter,
    field: Option<Field>,
}

impl RecordDeserializer {
    fn new(map: IndexMap<LocIdent, Field>) -> Self {
        RecordDeserializer {
            iter: map.into_iter(),
            field: None,
        }
    }
}

impl<'de> MapAccess<'de> for RecordDeserializer {
    type Error = RustDeserializationError;

    fn next_key_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        match self.iter.next() {
            Some((key, value)) => {
                self.field = Some(value);
                seed.deserialize(key.label().into_deserializer()).map(Some)
            }
            None => Ok(None),
        }
    }

    fn next_value_seed<T>(&mut self, seed: T) -> Result<T::Value, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        match self.field.take() {
            Some(Field {
                value: Some(value), ..
            }) => seed.deserialize(value),
            // TODO: what to do about fields without definition is not totally clear (should an
            // empty optional be considered as `None` or just disappear from the serialization?
            // Probably the former). For now, we implement the same behavior as before the
            // implementation of RFC005, which is to always fail on a field without definition.
            //
            // This should be relaxed in the future.
            Some(Field { value: None, .. }) => Err(RustDeserializationError::EmptyRecordField),
            _ => Err(RustDeserializationError::MissingValue),
        }
    }

    fn size_hint(&self) -> Option<usize> {
        match self.iter.size_hint() {
            (lower, Some(upper)) if lower == upper => Some(upper),
            _ => None,
        }
    }
}

fn visit_record_container<'de, V>(
    container: Container<RecordBody>,
    visitor: V,
) -> Result<V::Value, RustDeserializationError>
where
    V: Visitor<'de>,
{
    visit_record(container.unwrap_or_alloc().0.fields, visitor)
}

fn visit_record<'de, V>(
    record: IndexMap<LocIdent, Field>,
    visitor: V,
) -> Result<V::Value, RustDeserializationError>
where
    V: Visitor<'de>,
{
    let len = record.len();
    let mut deserializer = RecordDeserializer::new(record);
    let map = visitor.visit_map(&mut deserializer)?;
    let remaining = deserializer.iter.len();
    if remaining == 0 {
        Ok(map)
    } else {
        Err(RustDeserializationError::InvalidRecordLength(len))
    }
}

struct VariantDeserializer {
    value: Option<NickelValue>,
}

impl<'de> VariantAccess<'de> for VariantDeserializer {
    type Error = RustDeserializationError;

    fn unit_variant(self) -> Result<(), Self::Error> {
        match self.value {
            Some(value) => Deserialize::deserialize(value),
            None => Ok(()),
        }
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        match self.value {
            Some(value) => seed.deserialize(value),
            None => Err(RustDeserializationError::MissingValue),
        }
    }

    fn tuple_variant<V>(self, _len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.value.map(|v| v.content()) {
            Some(ValueContent::Array(lens)) => {
                let v = lens.take().unwrap_or_alloc().array;

                if v.is_empty() {
                    visitor.visit_unit()
                } else {
                    visit_array(v, visitor)
                }
            }
            Some(lens) => Err(RustDeserializationError::InvalidType {
                expected: "Array variant".to_string(),
                occurred: lens.restore().type_of().unwrap_or("Other").to_owned(),
            }),
            None => Err(RustDeserializationError::MissingValue),
        }
    }

    fn struct_variant<V>(
        self,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.value.map(|v| v.content()) {
            Some(ValueContent::Record(lens)) => visit_record_container(lens.take(), visitor),
            Some(lens) => Err(RustDeserializationError::InvalidType {
                expected: "Array variant".to_string(),
                occurred: lens.restore().type_of().unwrap_or("Other").to_owned(),
            }),
            None => Err(RustDeserializationError::MissingValue),
        }
    }
}

struct EnumDeserializer {
    tag: String,
    value: Option<NickelValue>,
}

impl<'de> EnumAccess<'de> for EnumDeserializer {
    type Error = RustDeserializationError;
    type Variant = VariantDeserializer;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, VariantDeserializer), Self::Error>
    where
        V: DeserializeSeed<'de>,
    {
        let variant = self.tag.into_deserializer();
        let visitor = VariantDeserializer { value: self.value };
        seed.deserialize(variant).map(|v| (v, visitor))
    }
}

impl std::fmt::Display for RustDeserializationError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            RustDeserializationError::InvalidType { expected, occurred } => {
                write!(f, "invalid type: {occurred}, expected: {expected}")
            }
            RustDeserializationError::MissingValue => write!(f, "missing value"),
            RustDeserializationError::EmptyRecordField => write!(f, "empty Metavalue"),
            RustDeserializationError::InvalidRecordLength(len) => {
                write!(f, "invalid record length, expected {len}")
            }
            RustDeserializationError::InvalidArrayLength(len) => {
                write!(f, "invalid array length, expected {len}")
            }
            RustDeserializationError::UnimplementedType { occurred } => {
                write!(f, "unimplemented conversion from type: {occurred}")
            }
            RustDeserializationError::Other(err) => write!(f, "{err}"),
        }
    }
}

impl std::error::Error for RustDeserializationError {}

impl serde::de::Error for RustDeserializationError {
    fn custom<T>(msg: T) -> Self
    where
        T: std::fmt::Display,
    {
        RustDeserializationError::Other(msg.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::{from_path, from_reader, from_slice, from_str};
    use std::io::Cursor;

    use nickel_lang_utils::{
        nickel_lang_core::{
            bytecode::value::NickelValue, deserialize::RustDeserializationError,
            error::NullReporter,
        },
        test_program::TestProgram,
    };
    use serde::Deserialize;

    fn eval(source: &str) -> NickelValue {
        TestProgram::new_from_source(
            Cursor::new(source),
            "source",
            std::io::stderr(),
            NullReporter {},
        )
        .expect("program shouldn't fail")
        .eval_full()
        .expect("evaluation shouldn't fail")
    }

    #[test]
    fn rust_deserialize_struct_with_fields() {
        #[derive(Debug, PartialEq, Deserialize)]
        #[serde(rename_all = "lowercase")]
        enum E {
            Foo,
            Bar,
        }

        #[derive(Debug, PartialEq, Deserialize)]
        #[serde(rename_all = "lowercase")]
        enum H {
            Foo(u16),
            Bar(String),
        }

        #[derive(Debug, PartialEq, Deserialize)]
        struct A {
            a: f64,
            b: String,
            c: (),
            d: bool,
            e: E,
            f: Option<bool>,
            g: i16,
            h: H,
        }

        assert_eq!(
            A::deserialize(eval(
                r#"{ a = 10, b = "test string", c = null, d = true, e = 'foo, f = null,
                g = -10, h = { bar = "some other string" } }"#
            ))
            .expect("deserialization shouldn't fail"),
            A {
                a: 10.0,
                b: "test string".to_string(),
                c: (),
                d: true,
                e: E::Foo,
                f: None,
                g: -10,
                h: H::Bar("some other string".to_string())
            }
        )
    }

    #[test]
    fn rust_deserialize_array_of_numbers() {
        assert_eq!(
            Vec::<f64>::deserialize(eval(r#"[1, 2, 3, 4]"#))
                .expect("deserialization shouldn't fail"),
            vec![1.0, 2.0, 3.0, 4.0]
        )
    }

    #[test]
    fn rust_deserialize_fail_non_data() {
        #[derive(Debug, PartialEq, Deserialize)]
        struct A;

        assert_eq!(
            A::deserialize(eval(r#"fun a b => a + b"#)),
            Err(RustDeserializationError::InvalidType {
                expected: "Null".to_string(),
                occurred: "Function".to_string()
            })
        )
    }

    #[test]
    fn rust_deserialize_ignore_annotation() {
        #[derive(Debug, PartialEq, Deserialize)]
        struct A {
            a: f64,
        }

        assert_eq!(
            A::deserialize(eval(r#"{ a = (10 | Number) }"#))
                .expect("deserialization shouldn't fail"),
            A { a: 10.0 }
        )
    }

    #[test]
    fn rust_deserialize_wrappers() {
        #[derive(Debug, Clone, PartialEq, Deserialize)]
        struct A {
            a: f64,
            b: Vec<String>,
        }

        let a = A {
            a: 10.0,
            b: vec!["a".into(), "b".into()],
        };

        assert_eq!(
            from_str::<A>(r#"{ a = (10 | Number), b = ["a", "b"] }"#).unwrap(),
            a.clone()
        );

        assert_eq!(
            from_slice::<A>(br#"{ a = (10 | Number), b = ["a", "b"] }"#).unwrap(),
            a.clone()
        );

        assert_eq!(
            from_reader::<_, A>(Cursor::new(r#"{ a = (10 | Number), b = ["a", "b"] }"#)).unwrap(),
            a.clone()
        );

        assert_eq!(
            from_reader::<_, A>(Cursor::new(br#"{ a = (10 | Number), b = ["a", "b"] }"#)).unwrap(),
            a.clone()
        );

        assert_eq!(
            from_path::<f64>(
                nickel_lang_utils::project_root::project_root()
                    .join("examples/fibonacci/fibonacci.ncl")
            )
            .unwrap(),
            55.0
        );

        assert!(from_str::<String>("will not parse").is_err());
        assert!(from_str::<String>("1 | String").is_err());
    }
}
