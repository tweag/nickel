// c&p from old file.

use std::collections::HashMap;

use nickel_lang_core::identifier::Ident;
use serde::{Deserialize, Serialize};

use crate::{error::Error, resolve::Resolution, ManifestFile, Precise};

mod package_list {
    use std::collections::HashMap;

    use serde::{Deserializer, Serializer};

    use super::*;

    #[derive(Serialize, Deserialize)]
    struct Entry {
        source: Precise,
        #[serde(flatten)]
        entry: LockFileEntry,
    }

    pub fn serialize<S: Serializer>(
        h: &HashMap<Precise, LockFileEntry>,
        ser: S,
    ) -> Result<S::Ok, S::Error> {
        let entries: Vec<_> = h
            .iter()
            .map(|(source, entry)| Entry {
                source: source.clone(),
                entry: entry.clone(),
            })
            .collect();
        entries.serialize(ser)
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(
        de: D,
    ) -> Result<HashMap<Precise, LockFileEntry>, D::Error> {
        let entries = Vec::<Entry>::deserialize(de)?;
        Ok(entries.into_iter().map(|e| (e.source, e.entry)).collect())
    }
}

/// A lock file, specifying versions and names for all recursive dependencies.
///
/// This defines the on-disk format for lock files.
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct LockFile {
    /// The dependencies of the current (top-level) package.
    pub dependencies: HashMap<Ident, Precise>,
    /// All packages that we know about, and the dependencies of each one.
    ///
    /// Note that the package list is not guaranteed to be closed: path dependencies
    /// cannot have their dependencies resolved in the on-disk lockfile because they
    /// can change at any time. *Some* path dependencies (for example, path dependencies
    /// that are local to a git depencency repo) may have resolved dependencies.
    #[serde(with = "package_list")]
    pub packages: HashMap<Precise, LockFileEntry>,
}

impl LockFile {
    // TODO: move the implementation here
    pub fn new(manifest: &ManifestFile, resolution: &Resolution) -> Result<Self, Error> {
        resolution.lock_file(manifest)
    }
}

/// The dependencies of a single package.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct LockFileEntry {
    pub dependencies: HashMap<Ident, Precise>,
}
