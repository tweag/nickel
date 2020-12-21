//! Serialization of an evaluated program to various data format.
use crate::error::SerializationError;
use crate::identifier::Ident;
use crate::label::Label;
use crate::term::{MetaValue, RichTerm, Term};
use crate::types::Types;
use serde::ser::{Error, Serialize, SerializeMap, Serializer};
use std::collections::HashMap;

/// Serializer for docstring. Ignore the meta-data and serialize the underlying term.
pub fn serialize_docstring<S>(_doc: &String, t: &RichTerm, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    t.serialize(serializer)
}

/// Serializer for a contract with a default value. Ignore the meta-data and serialize the
/// underlying term.
pub fn serialize_contract_default<S>(
    _ty: &Types,
    _l: &Label,
    t: &RichTerm,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    t.serialize(serializer)
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

/// Check that a term is serializable. Serializable terms are booleans, numbers, strings, enum,
/// lists of serializable terms or records of serializable terms.
pub fn validate(t: &RichTerm) -> Result<(), SerializationError> {
    use Term::*;

    match t.term.as_ref() {
        Bool(_) | Num(_) | Str(_) | Enum(_) => Ok(()),
        Record(map) | RecRecord(map) => {
            map.iter().try_for_each(|(_, t)| validate(t))?;
            Ok(())
        }
        List(vec) => {
            vec.iter().try_for_each(validate)?;
            Ok(())
        }
        DefaultValue(ref t) | ContractWithDefault(_, _, ref t) | Docstring(_, ref t) => validate(t),
        _ => Err(SerializationError::NonSerializable(t.clone())),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error::{Error, EvalError};
    use crate::program::Program;
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

    macro_rules! assert_non_serializable {
        ( $term:expr ) => {
            validate(
                &mk_program($term)
                    .and_then(|mut p| p.eval_full())
                    .unwrap()
                    .into(),
            )
            .unwrap_err();
        };
    }

    #[test]
    fn basic() {
        assert_json_eq!("1 + 1", 2.0);
        assert_json_eq!("if true then false else true", false);
        assert_json_eq!(r##""Hello, #{"world"}!""##, "Hello, world!");
        assert_json_eq!("`foo", "foo");
    }

    #[test]
    fn lists() {
        assert_json_eq!("[]", json!([]));
        assert_json_eq!("[(1+1), (2+2), (3+3)]", json!([2.0, 4.0, 6.0]));
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
            "{a = 1; b = 2+2; c = 3}",
            json!({"a": 1.0, "b": 4.0, "c": 3.0})
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
        assert_non_serializable!("{a = 1; b = { c = fun x => x }}");
        assert_non_serializable!("{foo = { bar = let y = \"a\" in y}; b = [[fun x => x]] }");
    }
}
