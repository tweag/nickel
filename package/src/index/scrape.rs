//! Tools for updating an index from a git repository.
//!
//! Given an org and a project name, we look for a github project at that location.
//! We then look for all tags of the form v<semver>. For each such tag, we record
//! a version of that package.

use std::str::FromStr as _;

use anyhow::anyhow;

use crate::{util, ManifestFile};

use super::{CachedPackageFile, Id, IndexDependency, Package, PackageLocation};

// id here is a bit misused, because it tells the github url but the name of the package can actually be different
pub fn scrape(id: &Id) -> anyhow::Result<CachedPackageFile> {
    let (_tmp_dir, repo) = util::clone_github(id)?;
    let mut buf = Vec::new();
    let mut ret = CachedPackageFile::default();

    for tag in repo.references()?.tags()? {
        // Filter out tags that don't parse as versions.
        let tag = tag.map_err(|e| anyhow!("{}", e))?;
        let tag_name = tag.name().as_bstr();
        let Ok(tag_name) = std::str::from_utf8(tag_name) else {
            continue;
        };
        let Some(tag_version) = tag_name.strip_prefix("refs/tags/v") else {
            continue;
        };
        let Ok(version) = semver::Version::from_str(tag_version) else {
            continue;
        };

        let peeled_tag = tag.into_fully_peeled_id()?;
        let tree = peeled_tag.object()?.peel_to_tree()?;
        let Some(entry) = tree.lookup_entry_by_path("package.ncl", &mut buf)? else {
            continue;
        };
        let manifest = match ManifestFile::from_contents(&entry.object()?.data) {
            Ok(m) => m,
            Err(e) => panic!("{e}"),
        };

        if manifest.version != version {
            eprintln!(
                "version mismatch: manifest {}, tag {}",
                manifest.version, version
            );
            continue;
        }

        let package_id = Id {
            org: id.org.clone(),
            name: manifest.name.label().to_owned(),
        };
        let deps = manifest
            .dependencies
            .into_iter()
            .map(|(name, dep)| match dep {
                crate::Dependency::Index { id, version } => {
                    (name, IndexDependency { id, req: version })
                }
                _ => panic!("can't index a crate with git/path deps"),
            })
            .collect();
        let gix::ObjectId::Sha1(rev) = peeled_tag.detach();
        ret.packages.insert(
            version.clone(),
            Package {
                id: package_id,
                vers: version,
                nickel_vers: manifest.nickel_version,
                loc: PackageLocation::Github {
                    id: id.clone(),
                    rev: rev.into(),
                },
                deps,
                v: 0,
            },
        );
    }

    Ok(ret)
}
