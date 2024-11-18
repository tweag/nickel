//! This module contains the part of package management that's needed by the evaluator.
//!
//! In particular, it doesn't contain any support for version resolution or
//! dependency fetching, but defines package maps and allows them to be used to
//! resolve package dependencies to paths.

use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use crate::{error::ImportError, identifier::Ident, position::TermPos};

/// Maps package imports to filesystem locations.
///
/// Providing one of these to the import resolver enables importing named
/// packages, like `import foo`; this map tells the import resolver where
/// to find the `foo` package on the filesystem.
///
/// Package names are package-local, in the sense that if you import `foo`
/// and `bar`, and then `foo` also imports `bar`, then those two `bar`s
/// are not necessarily the same package.
#[derive(Clone, Debug, Default)]
pub struct PackageMap {
    /// The top-level name-to-path map. If the main file imports the name `foo`,
    /// it gets looked up here.
    pub top_level: HashMap<Ident, PathBuf>,
    /// The name-to-path map for imported packages. If the package located at
    /// `/my-path` imports `foo`, we look up `("/my-path", foo)` in this map.
    pub packages: HashMap<(PathBuf, Ident), PathBuf>,
}

impl PackageMap {
    /// Look up a package in the map.
    ///
    /// `parent` is the path of the package doing the import (if `None`, it was
    /// the top-level package doing the import).
    pub fn get(
        &self,
        parent: Option<&Path>,
        name: Ident,
        pos: TermPos,
    ) -> Result<&Path, ImportError> {
        let result = match parent {
            Some(parent) => self.packages.get(&(parent.to_owned(), name)),
            None => self.top_level.get(&name),
        };
        result
            .map(PathBuf::as_path)
            .ok_or_else(|| ImportError::MissingDependency {
                parent: parent.map(Path::to_owned),
                missing: name,
                pos,
            })
    }
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use super::*;
    use crate::{error::NullReporter, eval::cache::CacheImpl, program::Program, term::Term};
    use nickel_lang_utils::project_root::project_root;

    // Test basic package map functionality by building one manually out of
    // files in our integration test folder. More thorough integration tests
    // will be possible once the cli is wired up with lock-files.
    #[test]
    fn package_import() {
        let pkg1 = project_root().join("core/tests/integration/inputs/imports/imported/pkg1");
        let pkg2 = project_root().join("core/tests/integration/inputs/imports/imported/pkg2");

        let map = PackageMap {
            top_level: std::iter::once((Ident::new("pkg"), pkg1.clone())).collect(),
            packages: std::iter::once(((pkg1, Ident::new("dep")), pkg2)).collect(),
        };

        let mut p: Program<CacheImpl> = Program::new_from_source(
            Cursor::new("import pkg"),
            "<test>",
            std::io::sink(),
            NullReporter {},
        )
        .unwrap();
        p.set_package_map(map);

        assert_eq!(p.eval_full().unwrap().as_ref(), &Term::Num(44.into()));
    }
}
