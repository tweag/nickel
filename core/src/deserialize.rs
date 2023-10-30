//! Deserialization of an evaluated program to plain Rust types.

use malachite::{num::conversion::traits::RoundingFrom, rounding_modes::RoundingMode};
use std::iter::ExactSizeIterator;

use serde::de::{
    Deserialize, DeserializeSeed, EnumAccess, IntoDeserializer, MapAccess, SeqAccess,
    VariantAccess, Visitor,
};

use crate::identifier::LocIdent;
use crate::term::array::{self, Array};
use crate::term::record::Field;
use crate::term::{IndexMap, RichTerm, Term};

macro_rules! deserialize_number {
    ($method:ident, $type:tt, $visit:ident) => {
        fn $method<V>(self, visitor: V) -> Result<V::Value, Self::Error>
        where
            V: Visitor<'de>,
        {
            match unwrap_term(self)? {
                Term::Num(n) => visitor.$visit($type::rounding_from(&n, RoundingMode::Nearest).0),
                other => Err(RustDeserializationError::InvalidType {
                    expected: "Number".to_string(),
                    occurred: RichTerm::from(other).to_string(),
                }),
            }
        }
    };
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

impl<'de> serde::Deserializer<'de> for RichTerm {
    type Error = RustDeserializationError;

    /// Catch-all deserialization
    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match unwrap_term(self)? {
            Term::Null => visitor.visit_unit(),
            Term::Bool(v) => visitor.visit_bool(v),
            Term::Num(v) => visitor.visit_f64(f64::rounding_from(v, RoundingMode::Nearest).0),
            Term::Str(v) => visitor.visit_string(v.into_inner()),
            Term::Enum(v) => visitor.visit_enum(EnumDeserializer {
                variant: v.into_label(),
                rich_term: None,
            }),
            Term::Record(record) => visit_record(record.fields, visitor),
            Term::Array(v, _) => visit_array(v, visitor),
            // unreachable(): `unwrap_term` recursively unwraps `Annotated` nodes until it
            // encounters a different node, or it fails. Thus, if `unwrap_term` succeeds, the
            // result can't be `Annotated`.
            Term::Annotated(..) => unreachable!(),
            other => Err(RustDeserializationError::UnimplementedType {
                occurred: other.type_of().unwrap_or_else(|| "Other".to_string()),
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
        match unwrap_term(self)? {
            Term::Null => visitor.visit_none(),
            some => visitor.visit_some(RichTerm::from(some)),
        }
    }

    /// deserialize `RichTerm::Enum` tags or `RichTerm::Record`s with a single item.
    fn deserialize_enum<V>(
        self,
        _name: &str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let (variant, rich_term) = match unwrap_term(self)? {
            Term::Enum(ident) => (ident.into_label(), None),
            Term::Record(record) => {
                let mut iter = record.fields.into_iter();
                let (variant, value) = match iter.next() {
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
                (variant.into_label(), value)
            }
            other => {
                return Err(RustDeserializationError::InvalidType {
                    expected: "Enum or Record".to_string(),
                    occurred: other.type_of().unwrap_or_else(|| "Other".to_string()),
                });
            }
        };

        visitor.visit_enum(EnumDeserializer { variant, rich_term })
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

    /// Deserialize `RichTerm::Bool`
    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match unwrap_term(self)? {
            Term::Bool(v) => visitor.visit_bool(v),
            other => Err(RustDeserializationError::InvalidType {
                expected: "Bool".to_string(),
                occurred: other.type_of().unwrap_or_else(|| "Other".to_string()),
            }),
        }
    }

    /// Deserialize `RichTerm::Str` as char
    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_string(visitor)
    }

    /// Deserialize `RichTerm::Str` as str
    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_string(visitor)
    }

    /// Deserialize `RichTerm::Str` as String
    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match unwrap_term(self)? {
            Term::Str(v) => visitor.visit_string(v.into_inner()),
            other => Err(RustDeserializationError::InvalidType {
                expected: "Str".to_string(),
                occurred: other.type_of().unwrap_or_else(|| "Other".to_string()),
            }),
        }
    }

    /// Deserialize `RichTerm::Str` as String or `RichTerm::Array` as array,
    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_byte_buf(visitor)
    }

    /// Deserialize `RichTerm::Str` as String or `RichTerm::Array` as array,
    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match unwrap_term(self)? {
            Term::Str(v) => visitor.visit_string(v.into_inner()),
            Term::Array(v, _) => visit_array(v, visitor),
            other => Err(RustDeserializationError::InvalidType {
                expected: "Str or Array".to_string(),
                occurred: other.type_of().unwrap_or_else(|| "Other".to_string()),
            }),
        }
    }

    /// Deserialize `RichTerm::Null` as `()`.
    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match unwrap_term(self)? {
            Term::Null => visitor.visit_unit(),
            other => Err(RustDeserializationError::InvalidType {
                expected: "Null".to_string(),
                occurred: other.type_of().unwrap_or_else(|| "Other".to_string()),
            }),
        }
    }

    /// Deserialize `RichTerm::Null` as `()`.
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

    /// Deserialize `RichTerm::Array` as `Vec<T>`.
    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match unwrap_term(self)? {
            Term::Array(v, _) => visit_array(v, visitor),
            other => Err(RustDeserializationError::InvalidType {
                expected: "Array".to_string(),
                occurred: other.type_of().unwrap_or_else(|| "Other".to_string()),
            }),
        }
    }

    /// Deserialize `RichTerm::Array` as `Vec<T>`.
    fn deserialize_tuple<V>(self, _len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    /// Deserialize `RichTerm::Array` as `Vec<T>`.
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

    /// Deserialize `RichTerm::Record` as `HashMap<K, V>`.
    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match unwrap_term(self)? {
            Term::Record(record) => visit_record(record.fields, visitor),
            other => Err(RustDeserializationError::InvalidType {
                expected: "Record".to_string(),
                occurred: other.type_of().unwrap_or_else(|| "Other".to_string()),
            }),
        }
    }

    /// Deserialize `RichTerm::Record` as `struct`.
    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match unwrap_term(self)? {
            Term::Array(v, _) => visit_array(v, visitor),
            Term::Record(record) => visit_record(record.fields, visitor),
            other => Err(RustDeserializationError::InvalidType {
                expected: "Record".to_string(),
                occurred: other.type_of().unwrap_or_else(|| "Other".to_string()),
            }),
        }
    }

    /// Deserialize `Ident` as `String`.
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
    iter: array::IntoIter,
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

fn unwrap_term(mut rich_term: RichTerm) -> Result<Term, RustDeserializationError> {
    loop {
        rich_term = match Term::from(rich_term) {
            Term::Annotated(_, value) => value,
            other => break Ok(other),
        }
    }
}

fn visit_array<'de, V>(array: Array, visitor: V) -> Result<V::Value, RustDeserializationError>
where
    V: Visitor<'de>,
{
    let len = array.len();
    let mut deserializer = ArrayDeserializer::new(array);
    let seq = visitor.visit_seq(&mut deserializer)?;
    let remaining = deserializer.iter.len();
    if remaining == 0 {
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
    rich_term: Option<RichTerm>,
}

impl<'de> VariantAccess<'de> for VariantDeserializer {
    type Error = RustDeserializationError;

    fn unit_variant(self) -> Result<(), Self::Error> {
        match self.rich_term {
            Some(value) => Deserialize::deserialize(value),
            None => Ok(()),
        }
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        match self.rich_term {
            Some(value) => seed.deserialize(value),
            None => Err(RustDeserializationError::MissingValue),
        }
    }

    fn tuple_variant<V>(self, _len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.rich_term.map(unwrap_term) {
            Some(Ok(Term::Array(v, _))) => {
                if v.is_empty() {
                    visitor.visit_unit()
                } else {
                    visit_array(v, visitor)
                }
            }
            Some(Ok(other)) => Err(RustDeserializationError::InvalidType {
                expected: "Array variant".to_string(),
                occurred: other.type_of().unwrap_or_else(|| "Other".to_string()),
            }),
            Some(Err(err)) => Err(err),
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
        match self.rich_term.map(unwrap_term) {
            Some(Ok(Term::Record(record))) => visit_record(record.fields, visitor),
            Some(Ok(other)) => Err(RustDeserializationError::InvalidType {
                expected: "Array variant".to_string(),
                occurred: other.type_of().unwrap_or_else(|| "Other".to_string()),
            }),
            Some(Err(err)) => Err(err),
            None => Err(RustDeserializationError::MissingValue),
        }
    }
}

struct EnumDeserializer {
    variant: String,
    rich_term: Option<RichTerm>,
}

impl<'de> EnumAccess<'de> for EnumDeserializer {
    type Error = RustDeserializationError;
    type Variant = VariantDeserializer;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, VariantDeserializer), Self::Error>
    where
        V: DeserializeSeed<'de>,
    {
        let variant = self.variant.into_deserializer();
        let visitor = VariantDeserializer {
            rich_term: self.rich_term,
        };
        seed.deserialize(variant).map(|v| (v, visitor))
    }
}

impl std::fmt::Display for RustDeserializationError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            RustDeserializationError::InvalidType {
                ref expected,
                ref occurred,
            } => write!(f, "invalid type: {occurred}, expected: {expected}"),
            RustDeserializationError::MissingValue => write!(f, "missing value"),
            RustDeserializationError::EmptyRecordField => write!(f, "empty Metavalue"),
            RustDeserializationError::InvalidRecordLength(len) => {
                write!(f, "invalid record length, expected {len}")
            }
            RustDeserializationError::InvalidArrayLength(len) => {
                write!(f, "invalid array length, expected {len}")
            }
            RustDeserializationError::UnimplementedType { ref occurred } => {
                write!(f, "unimplemented conversion from type: {occurred}")
            }
            RustDeserializationError::Other(ref err) => write!(f, "{err}"),
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
    use std::io::Cursor;

    use nickel_lang_utils::{
        nickel_lang_core::{deserialize::RustDeserializationError, term::RichTerm},
        test_program::TestProgram,
    };
    use serde::Deserialize;

    fn eval(source: &str) -> RichTerm {
        TestProgram::new_from_source(Cursor::new(source), "source", std::io::stderr())
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
}
