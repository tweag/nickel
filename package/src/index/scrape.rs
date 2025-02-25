//! Tools for updating an index from a git repository.

use gix::ObjectId;
use nickel_lang_git::Spec;
use tempfile::tempdir;

use crate::{
    error::{Error, IoResultExt},
    manifest::MANIFEST_NAME,
    ManifestFile,
};

use super::{Id, Package, PreciseId};

/// Fetch a package from the specified place, and figure out what its index
/// entry should look like.
///
/// Question: should we allow index packages to come from a _subdirectory_ of
/// a git repo, or should they always need to be at the root? Currently we only
/// allow them at the root, the motivation being that if the git repo backing
/// the index package only contains the package itself, then it will be a
/// smaller download. On the other hand, we allow git dependencies to live in
/// a subdirectory, the idea being that you can have a monorepo with multiple
/// Nickel packages.
///
/// Maybe the current situation is already flexible enough, as the publishing
/// workflow could have an automatic step to extract a Nickel package into its
/// own git repo.
pub fn fetch_git(id: &Id, commit: &ObjectId) -> Result<Package, Error> {
    // We need to fetch the manifest file to get some metadata out. We're currently shallow-cloning
    // the whole repo, but we could use a github API (or maybe some fancier git features) to be more
    // efficient.
    let tmpdir = tempdir().without_path()?;
    let _id = nickel_lang_git::fetch(&Spec::commit(id.remote_url()?, *commit), tmpdir.path())?;

    let manifest_path = tmpdir.path().join(MANIFEST_NAME);
    let manifest = ManifestFile::from_path(manifest_path)?;

    let Id::Github { org, name } = id.clone();
    let id = PreciseId::Github {
        org,
        name,
        commit: *commit,
    };

    Package::from_manifest_and_id(&manifest, &id)
}

// Check if the manifest file lives in a git directory, and check that the directory is clean.
// Then find the id of head and construct a package from it
pub fn read_from_manifest(id: &Id, manifest: &ManifestFile) -> Result<Package, Error> {
    let repo = gix::open(&manifest.parent_dir)?;
    if !matches!(repo.is_dirty(), Ok(false)) {
        eprintln!(
            "git repository at {} is dirty",
            manifest.parent_dir.display()
        );
    }

    // FIXME: unwrap
    let head_id = repo.head_tree_id().unwrap().detach();
    let id = match id {
        Id::Github { org, name } => PreciseId::Github {
            org: org.clone(),
            name: name.clone(),
            commit: head_id,
        },
    };
    Package::from_manifest_and_id(manifest, &id)
}
