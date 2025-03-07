//! Defines the serialization format for the package index.

use std::collections::BTreeMap;

use nickel_lang_core::identifier::Ident;
use serde::{Deserialize, Serialize};

use crate::{
    index,
    version::{SemVer, VersionReq},
    IndexDependency,
};

use super::{Package, PreciseId};

/// Defines the serialization format for `IndexDependency` in the package index.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct IndexDependencyFormat {
    #[serde(flatten)]
    pub id: IdFormat,
    pub req: VersionReq,
}

impl From<IndexDependency> for IndexDependencyFormat {
    fn from(i: IndexDependency) -> Self {
        IndexDependencyFormat {
            id: i.id.into(),
            req: i.version,
        }
    }
}

impl From<IndexDependencyFormat> for IndexDependency {
    fn from(i: IndexDependencyFormat) -> Self {
        IndexDependency {
            id: i.id.into(),
            version: i.req,
        }
    }
}

/// Defines the serialization format for `Id` in the package index.
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum IdFormat {
    #[serde(rename = "github")]
    Github { org: String, name: String },
}

impl From<index::Id> for IdFormat {
    fn from(i: index::Id) -> Self {
        match i {
            index::Id::Github { org, name } => IdFormat::Github { org, name },
        }
    }
}

impl From<IdFormat> for index::Id {
    fn from(i: IdFormat) -> Self {
        match i {
            IdFormat::Github { org, name } => index::Id::Github { org, name },
        }
    }
}

/// Defines the serialization format for a package record in the index.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct PackageFormat {
    id: PreciseId,
    version: SemVer,
    minimal_nickel_version: SemVer,
    dependencies: BTreeMap<Ident, IndexDependencyFormat>,

    authors: Vec<String>,
    description: String,
    keywords: Vec<String>,
    license: String,

    /// Version of the index schema. Currently always zero.
    v: u32,
}

impl From<Package> for PackageFormat {
    fn from(p: Package) -> Self {
        Self {
            id: p.id,
            version: p.version,
            minimal_nickel_version: p.minimal_nickel_version,
            dependencies: p
                .dependencies
                .into_iter()
                .map(|(k, v)| (k, v.into()))
                .collect(),
            authors: p.authors,
            description: p.description,
            keywords: p.keywords,
            license: p.license,
            v: 0,
        }
    }
}

impl From<PackageFormat> for Package {
    fn from(p: PackageFormat) -> Self {
        Self {
            id: p.id,
            version: p.version,
            minimal_nickel_version: p.minimal_nickel_version,
            dependencies: p
                .dependencies
                .into_iter()
                .map(|(k, v)| (k, v.into()))
                .collect(),
            authors: p.authors,
            description: p.description,
            keywords: p.keywords,
            license: p.license,
        }
    }
}
