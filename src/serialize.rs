//! Serialization of an evaluated program to various data format.
use crate::error::SerializationError;
use crate::identifier::Ident;
use crate::term::{MetaValue, RichTerm, Term};
use serde::de::{Deserialize, Deserializer};
use serde::ser::{Error, Serialize, SerializeMap, Serializer};
use std::collections::HashMap;
use std::fmt;
use std::str::FromStr;

/// Available export formats.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum ExportFormat {
    Raw,
    Json,
    Yaml,
    Toml,
}

impl std::default::Default for ExportFormat {
    fn default() -> Self {
        ExportFormat::Json
    }
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

impl FromStr for ExportFormat {
    type Err = ParseFormatError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_ref() {
            "raw" => Ok(ExportFormat::Raw),
            "json" => Ok(ExportFormat::Json),
            "yaml" => Ok(ExportFormat::Yaml),
            "toml" => Ok(ExportFormat::Toml),
            _ => Err(ParseFormatError(String::from(s))),
        }
    }
}

/// Serializer for metavalues.
pub fn serialize_meta_value<S>(meta: &MetaValue, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    if let Some(ref t) = meta.value {
        t.serialize(serializer)
    } else {
        // This error should not happen if the input term is validated before serialization
        Err(Error::custom("empty metavalue"))
    }
}

/// Serializer for a record. Serialize fields in alphabetical order to get a deterministic output
/// (by default, `HashMap`'s randomness implies a randomized order of fields in the output).
pub fn serialize_record<S>(map: &HashMap<Ident, RichTerm>, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    let mut entries: Vec<(_, _)> = map.iter().collect();
    entries.sort_by(|(k1, _), (k2, _)| k1.cmp(k2));

    let mut map_ser = serializer.serialize_map(Some(entries.len()))?;
    for (id, t) in entries.iter() {
        map_ser.serialize_entry(&id.to_string(), &t)?
    }

    map_ser.end()
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
        Ok(RichTerm::new(t, None))
    }
}

/// Check that a term is serializable. Serializable terms are booleans, numbers, strings, enum,
/// lists of serializable terms or records of serializable terms.
pub fn validate(t: &RichTerm, format: ExportFormat) -> Result<(), SerializationError> {
    use crate::term;
    use Term::*;

    if format == ExportFormat::Raw {
        if let Term::Str(_) = t.term.as_ref() {
            Ok(())
        } else {
            Err(SerializationError::NotAString(t.clone()))
        }
    } else {
        match t.term.as_ref() {
            // TOML doesn't support null values
            Null if format == ExportFormat::Json || format == ExportFormat::Yaml => Ok(()),
            Null => Err(SerializationError::UnsupportedNull(format, t.clone())),
            Bool(_) | Num(_) | Str(_) | Enum(_) => Ok(()),
            Record(map) | RecRecord(map) => {
                map.iter().try_for_each(|(_, t)| validate(t, format))?;
                Ok(())
            }
            List(vec) => {
                vec.iter().try_for_each(|t| validate(t, format))?;
                Ok(())
            }
            //TODO: have a specific error for such missing value.
            MetaValue(term::MetaValue {
                value: Some(ref t), ..
            }) => validate(t, format),
            _ => Err(SerializationError::NonSerializable(t.clone())),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error::{Error, EvalError};
    use crate::program::Program;
    use crate::term::{make as mk_term, BinaryOp};
    use serde_json::json;
    use std::io::Cursor;

    fn mk_program(s: &str) -> Result<Program, Error> {
        let src = Cursor::new(s);

        Program::new_from_source(src, "<test>").map_err(|io_err| {
            Error::EvalError(EvalError::Other(format!("IO error: {}", io_err), None))
        })
    }

    macro_rules! assert_json_eq {
        ( $term:expr, $result:expr ) => {
            assert_eq!(
                serde_json::to_string(&mk_program($term).and_then(|mut p| p.eval_full()).unwrap())
                    .unwrap(),
                serde_json::to_string(&$result).unwrap()
            )
        };
    }

    macro_rules! assert_pass_validation {
        ( $term:expr, $format:expr, true) => {
            validate(
                &mk_program($term)
                    .and_then(|mut p| p.eval_full())
                    .unwrap()
                    .into(),
                $format,
            )
            .unwrap();
        };
        ( $term:expr, $format:expr, false) => {
            validate(
                &mk_program($term)
                    .and_then(|mut p| p.eval_full())
                    .unwrap()
                    .into(),
                $format,
            )
            .unwrap_err();
        };
    }

    macro_rules! assert_involutory {
        ( $term:expr ) => {
            let evaluated = mk_program($term).and_then(|mut p| p.eval_full()).unwrap();
            let from_json: RichTerm =
                serde_json::from_str(&serde_json::to_string(&evaluated).unwrap()).unwrap();
            let from_yaml: RichTerm =
                serde_yaml::from_str(&serde_yaml::to_string(&evaluated).unwrap()).unwrap();
            let from_toml: RichTerm =
                toml::from_str(&format!("{}", &toml::Value::try_from(&evaluated).unwrap()))
                    .unwrap();

            assert_eq!(
                $crate::eval::eval(
                    mk_term::op2(BinaryOp::Eq(), from_json, evaluated.clone()),
                    &HashMap::new(),
                    &mut $crate::cache::resolvers::DummyResolver {}
                ),
                Ok(Term::Bool(true))
            );
            assert_eq!(
                $crate::eval::eval(
                    mk_term::op2(BinaryOp::Eq(), from_yaml, evaluated.clone()),
                    &HashMap::new(),
                    &mut $crate::cache::resolvers::DummyResolver {}
                ),
                Ok(Term::Bool(true))
            );
            assert_eq!(
                $crate::eval::eval(
                    mk_term::op2(BinaryOp::Eq(), from_toml, evaluated),
                    &HashMap::new(),
                    &mut $crate::cache::resolvers::DummyResolver {}
                ),
                Ok(Term::Bool(true))
            );
        };
    }

    #[test]
    fn basic() {
        assert_json_eq!("1 + 1", 2.0);

        let null: Option<()> = None;
        assert_json_eq!("null", null);

        assert_json_eq!("if true then false else true", false);
        assert_json_eq!(r##""Hello, #{"world"}!""##, "Hello, world!");
        assert_json_eq!("`foo", "foo");
    }

    #[test]
    fn lists() {
        assert_json_eq!("[]", json!([]));
        assert_json_eq!("[null, (1+1), (2+2), (3+3)]", json!([null, 2.0, 4.0, 6.0]));
        assert_json_eq!(
            r##"[`a, ("b" ++ "c"), "d#{"e"}f", "g"]"##,
            json!(["a", "bc", "def", "g"])
        );
        assert_json_eq!(
            r#"lists.fold (fun elt acc => [[elt]] @ acc) [1, 2, 3, 4] []"#,
            json!([[1.0], [2.0], [3.0], [4.0]])
        );
        assert_json_eq!("[\"a\", 1, false, `foo]", json!(["a", 1.0, false, "foo"]));
    }

    #[test]
    fn records() {
        assert_json_eq!(
            "{a = 1; b = 2+2; c = 3; d = null}",
            json!({"a": 1.0, "b": 4.0, "c": 3.0, "d": null})
        );

        assert_json_eq!(
            "{a = {} & {b = {c = if true then `richtig else `falsche}}}",
            json!({"a": {"b": {"c": "richtig"}}})
        );

        assert_json_eq!(
            "{foo = let z = 0.5 + 0.5 in z; bar = [\"str\", true || false]; baz = {subfoo = !false} & {subbar = 1 - 1}}",
            json!({"foo": 1.0, "bar": ["str", true], "baz": {"subfoo": true, "subbar": 0.0}})
        );
    }

    #[test]
    fn enriched_values() {
        assert_json_eq!(
            "{a = Default(1); b = Docstring(\"doc\", 2+2); c = 3}",
            json!({"a": 1.0, "b": 4.0, "c": 3.0})
        );

        assert_json_eq!(
            "{a = {b = Default({})} & {b = {c = Default(if true then `faux else `vrai)}}}",
            json!({"a": {"b": {"c": "faux"}}})
        );

        assert_json_eq!(
            "{baz = Default({subfoo = Default(!false)} & {subbar = Default(1 - 1)})}",
            json!({"baz": {"subfoo": true, "subbar": 0.0}})
        );
    }

    #[test]
    fn prevalidation() {
        assert_pass_validation!("{a = 1; b = { c = fun x => x }}", ExportFormat::Json, false);
        assert_pass_validation!(
            "{foo = { bar = let y = \"a\" in y}; b = [[fun x => x]] }",
            ExportFormat::Json,
            false
        );
        assert_pass_validation!("{foo = null}", ExportFormat::Json, true);
        assert_pass_validation!("{foo = null}", ExportFormat::Toml, false);
    }

    #[test]
    fn involution() {
        assert_involutory!("{val = 1 + 1}");
        assert_involutory!("{val = \"Some string\"}");
        assert_involutory!("{val = [\"a\", 3, []]}");
        assert_involutory!(
            "{a = {foo = {bar = \"2\"}}; b = false; c = [{d = \"e\"}, {d = \"f\"}]}"
        );
    }
}
