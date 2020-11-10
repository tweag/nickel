//! Serialization of an evaluated program to various data format.
use crate::error::NonSerializableError;
use crate::label::Label;
use crate::term::{RichTerm, Term};
use crate::types::Types;
use serde::ser::{Serialize, Serializer};

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

impl Serialize for RichTerm {
    /// Serialize the underlying term.
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        (*self.term).serialize(serializer)
    }
}

pub fn validate(t: &RichTerm) -> Result<(), NonSerializableError> {
    use Term::*;

    match t.term.as_ref() {
        Bool(_) | Num(_) | Str(_) | Enum(_) => Ok(()),
        Record(map) => {
            map.iter().try_for_each(|(_, t)| validate(t))?;
            Ok(())
        }
        List(vec) => {
            vec.iter().try_for_each(validate)?;
            Ok(())
        }
        DefaultValue(ref t) | ContractWithDefault(_, _, ref t) | Docstring(_, ref t) => validate(t),
        _ => Err(NonSerializableError(t.clone())),
    }
}

// pub trait SerializeSeed<T> {
//     fn serialize_seed<S>(&self, serializer: S, seed: &T) -> Result<S::Ok, S::Error> where S: Serializer;
// }
//
// pub struct StateSerializer<'a, T, S> {
//     state: &'a T,
//     serializer: S
// }
//
// pub trait SerializerSeed {
//     type Ok
//     type Error: Error
//
// }
// impl Serialize for Term {
//     fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
//         use Term::*;
//
//         match self {
//             Bool(b) => serializer.serialize_bool(*b),
//             Num(n) => serializer.serialize_f64(*n),
//             Str(s) => serializer.serialize_str(s),
//             Enum(id) => id.serialize(serializer),
//             Record(map) => map.serialize(serializer),
//             List(vec) => vec.serialize(serializer),
//             DefaultValue(t) | ContractWithDefault(_, _, t) | Docstring(_, t) =>
//                 t.as_ref().serialize(serializer),
//             _ => panic!(),
//         }
//     }
// }

// impl<T> Serialize for SerializeSeed<T> {
//     fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
//
//     }
// }

//
// impl Serialize for Closure {
//     fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
//         panic!("not implemented")
//     }
// }
