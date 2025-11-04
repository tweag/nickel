//! Tools for updating an index from a git repository.

use std::path::Path;

use gix::ObjectId;
use nickel_lang_git::Spec;
use tempfile::tempdir;

use crate::{
    ManifestFile,
    error::{Error, IoResultExt},
    manifest::MANIFEST_NAME,
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

    let Id::Github { path, org, name } = id.clone();

    let mut manifest_path = tmpdir.path().to_owned();
    if !path.is_empty() {
        manifest_path.push(&path);
    }
    manifest_path.push(MANIFEST_NAME);
    let manifest = ManifestFile::from_path(manifest_path)?;

    let id = PreciseId::Github {
        org,
        name,
        commit: *commit,
        path,
    };

    Package::from_manifest_and_id(&manifest, &id)
}

/// Like `Path::strip_prefix`, but it does the suffix.
fn strip_suffix<'a>(mut path: &'a Path, mut suffix: &Path) -> Option<&'a Path> {
    while let Some(last) = suffix.file_name() {
        if path.file_name() != Some(last) {
            return None;
        }
        // unwrap: `remaining_path` and `repo_dir` both had file names, so they also have parents.
        suffix = suffix.parent().unwrap();
        path = path.parent().unwrap();
    }
    Some(path)
}

// Check if the manifest file lives in a git directory, and check that the directory is clean.
// Then find the id of head and construct a package from it
pub fn read_from_manifest(id: &Id, manifest: &ManifestFile) -> Result<Package, Error> {
    // The git repo containing the manifest is not necessarily the direct parent of
    // the manifest: starting at the git repo and following the path in `id` should
    // bring us to the manifest.
    let Id::Github { path: id_path, .. } = id;
    let Some(repo_dir) = strip_suffix(&manifest.parent_dir, id_path.as_ref()) else {
        return Err(Error::MismatchedManifestPath {
            id: id.clone(),
            manifest_dir: manifest.parent_dir.clone(),
        });
    };
    let repo = gix::open(repo_dir)?;

    if let Ok(false) = repo.is_dirty() {
        info!(
            "git repository at {} is dirty",
            manifest.parent_dir.display()
        );
    }

    let head_id = repo.head_tree_id()?.detach();
    let id = match id {
        Id::Github { org, name, path } => PreciseId::Github {
            org: org.clone(),
            name: name.clone(),
            commit: head_id,
            path: path.clone(),
        },
    };
    Package::from_manifest_and_id(manifest, &id)
}
