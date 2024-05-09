//! This module contains the part of package management that's needed by the evaluator.
//!
//! In particular, it doesn't contain any support for version resolution or
//! dependency fetching, but defines lockfiles and allows them to be used to
//! resolve package dependencies to paths.

use std::{
    array::TryFromSliceError,
    collections::HashMap,
    path::{Path, PathBuf},
    str::FromStr as _,
};

use serde::{Deserialize, Serialize};

use crate::{error::ImportError, position::TermPos};

const ID_LEN: usize = 20;

/// A git object id.
///
/// Git uses 160-bit hashes as object ids. To avoid pulling in the full git dependency, we define our
/// own id type.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ObjectId([u8; ID_LEN]);

// Git object ids are typically displayed in base16.
impl std::fmt::Display for ObjectId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&base16::encode_lower(&self.0))
    }
}

impl TryFrom<&[u8]> for ObjectId {
    type Error = TryFromSliceError;

    fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
        let arr: &[u8; ID_LEN] = bytes.try_into()?;
        Ok(ObjectId(*arr))
    }
}

#[derive(Clone, Debug)]
pub enum ParseObjectError {
    Length(usize),
    Base16(base16::DecodeError),
}

impl std::fmt::Display for ParseObjectError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseObjectError::Length(x) => {
                write!(f, "invalid object id length: got {x}, expected 40")
            }
            ParseObjectError::Base16(e) => write!(f, "invalid base16 for object id: {e}"),
        }
    }
}

impl std::error::Error for ParseObjectError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            ParseObjectError::Length(_) => None,
            ParseObjectError::Base16(e) => Some(e),
        }
    }
}

impl std::str::FromStr for ObjectId {
    type Err = ParseObjectError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // Since it's base16, the string should be twice as long as the raw data.
        if s.len() == ID_LEN * 2 {
            let mut ret = ObjectId([0; ID_LEN]);
            base16::decode_slice(s, &mut ret.0)
                .map(move |_| ret)
                .map_err(ParseObjectError::Base16)
        } else {
            Err(ParseObjectError::Length(s.len()))
        }
    }
}

impl serde::Serialize for ObjectId {
    fn serialize<S: serde::Serializer>(&self, ser: S) -> Result<S::Ok, S::Error> {
        self.to_string().serialize(ser)
    }
}

impl<'de> serde::Deserialize<'de> for ObjectId {
    fn deserialize<D: serde::Deserializer<'de>>(de: D) -> Result<Self, D::Error> {
        <&str>::deserialize(de)
            .and_then(|s| ObjectId::from_str(s).map_err(<D::Error as serde::de::Error>::custom))
    }
}

/// Packages are identified by an organization and a package name.
///
/// Both the organization and the package name are required to be valid nickel identifiers. They
/// are typically displayed separated by a slash, as in `example/package`. (Note that a slash
/// is not a valid character in an identifier.)
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[serde(try_from = "&str", into = "String")]
pub struct Name {
    pub org: String,
    pub package: String,
}

impl std::fmt::Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}/{}", self.org, self.package)
    }
}

impl std::str::FromStr for Name {
    type Err = String; // FIXME

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (org, package) = s.split_once('/').ok_or(String::new())?;
        if package.contains('/') {
            return Err(String::new());
        }

        Ok(Name {
            org: org.to_owned(),
            package: package.to_owned(),
        })
    }
}

impl TryFrom<&str> for Name {
    type Error = <Name as std::str::FromStr>::Err;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        s.parse()
    }
}

impl From<Name> for String {
    fn from(n: Name) -> Self {
        n.to_string()
    }
}

#[derive(Clone, Debug, Default)]
pub struct PackageMap {
    pub top_level: HashMap<Name, PathBuf>,
    pub packages: HashMap<(PathBuf, Name), PathBuf>,
}

impl PackageMap {
    pub fn get(
        &self,
        parent: Option<&Path>,
        name: &Name,
        pos: TermPos,
    ) -> Result<&Path, ImportError> {
        let result = match parent {
            Some(parent) => Some(
                self.packages
                    .get(&(parent.to_owned(), name.clone()))
                    .ok_or_else(|| ImportError::InternalError {
                        msg: format!("unknown parent package {parent:?}"),
                        pos,
                    })?,
            ),
            None => self.top_level.get(name),
        };
        result
            .map(PathBuf::as_path)
            .ok_or_else(|| ImportError::MissingDependency {
                parent: parent.map(Path::to_owned),
                missing: name.clone(),
                pos,
            })
    }
}
