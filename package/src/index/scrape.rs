//! Tools for updating an index from a git repository.

use gix::ObjectId;
use nickel_lang_git::Spec;
use tempfile::tempdir;

use crate::{
    error::{Error, IoResultExt},
    version::SemVer,
    ManifestFile,
};

use super::{Id, Package, PreciseId};

/// Fetch a package from the specified place, and figure out what its index
/// entry should look like.
/// TODO: allow a subdirectory?
pub fn fetch_git(id: &Id, version: SemVer, commit: &ObjectId) -> Result<Package, Error> {
    // We need to fetch the manifest file to get some metadata out. We're currently shallow-cloning
    // the whole repo, but we could use a github API (or maybe some fancier git features) to be more
    // efficient.
    let tmpdir = tempdir().without_path()?;
    let _id = nickel_lang_git::fetch(&Spec::commit(id.remote_url(), *commit), tmpdir.path())?;

    let manifest_path = tmpdir.path().join("package.ncl");
    let manifest = ManifestFile::from_path(manifest_path)?;

    let deps = manifest
        .dependencies
        .into_iter()
        .map(|(name, dep)| Ok((name, dep.as_index_dep(id.clone())?.into())))
        .collect::<Result<_, Error>>()?;

    let Id::Github { org, name } = id.clone();
    let id = PreciseId::Github {
        org,
        name,
        commit: *commit,
    };

    Ok(Package {
        id,
        vers: version,
        nickel_vers: manifest.minimal_nickel_version,
        deps,
        v: 0,
    })
}
