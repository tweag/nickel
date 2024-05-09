// c&p from old file.

use std::path::PathBuf;

use nickel_lang_core::package::{LockFile, LockedPackageSource, ResolvedLockFile};

use crate::{manifest::Spec, PackageSource};

pub fn resolve_lock_file(
    lock_file: &LockFile,
    root_path: PathBuf,
) -> Result<ResolvedLockFile, crate::Error> {
    let mut new = LockFile {
        // Make all path dependencies of the root absolute.
        dependencies: lock_file
            .dependencies
            .iter()
            .map(|(name, source)| (name.clone(), source.clone().with_abs_path(&root_path)))
            .collect(),
        // The old lock file knows about recursive git dependencies, and path dependencies of the root.
        // There are no path dependencies coming recursively from git dependencies because those have
        // been re-written to git dependencies; and there are no path dependencies of path dependencies
        // because those haven't been expanded yet.
        //
        // Pass through the (possibly recursive) git dependencies unchanged.
        packages: lock_file
            .packages
            .iter()
            .filter(|&(source, _)| !source.is_path())
            .map(|(source, entry)| (source.clone(), entry.clone()))
            .collect(),
    };

    // Expand (and make absolute) all path deps.
    let path_deps = lock_file.dependencies.iter().filter_map(|(name, s)| {
        if let LockedPackageSource::Path { path } = s {
            Some((name, path))
        } else {
            None
        }
    });

    let root = LockedPackageSource::Path {
        path: root_path.clone(),
    };
    for (name, path) in path_deps {
        let spec = Spec {
            name: name.clone(),
            source: PackageSource::Path { path: path.clone() },
        };
        let locked = spec.realize_rec(Some(&root))?;
        locked.flatten_into(&mut new);
    }

    Ok(ResolvedLockFile {
        path: root_path,
        inner: new,
    })
}
