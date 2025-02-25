// We avoid re-fetching git repositories if the lock-file matches the requirement.
// This file tests the various cases.

use nickel_lang_core::identifier::Ident;
use nickel_lang_git::Target;
use nickel_lang_package::{lock::EntryName, Dependency};
use tempfile::TempDir;

mod util;

use util::{add_git_file, init_git, test_config, ManifestBuilder};

macro_rules! assert_lock_snapshot_filtered {
    { $name:expr, $snapshot:expr } => {
        insta::with_settings!({filters => vec![
            // Lock files contain git ids, and I haven't figured out if it's possible to
            // get them consistent between runs (e.g., they include hashes of timestamps).
            // So we just filter them out of the comparison.
            (r#""id": "[a-z0-9]+""#, r#""id": <GENERATED>"#),
            // We're generating git packages in temporary directories, so ignore the location.
            (r#""url": "[^"]+""#, r#""url": <URL>"#)
        ]},
        {
            insta::assert_snapshot!($name, $snapshot);
        })
    }
}

#[test]
fn no_fetch_if_exact_match() {
    let git_dir = init_git();
    let pkg_dir = TempDir::new().unwrap();
    let manifest = ManifestBuilder::default()
        .with_dir(pkg_dir.path())
        .with_git_dep("dep", git_dir.path())
        .build();
    let (_cache_dir, config) = test_config();
    let (lock, _snap) = manifest.lock(config.clone()).unwrap();
    let lock_contents = serde_json::to_string_pretty(&lock).unwrap();
    assert_lock_snapshot_filtered!("no_fetch_if_exact_match", lock_contents);

    // Now modify the git repo. It shouldn't get re-fetched.
    add_git_file(&git_dir, "main.ncl");
    let (new_lock, _snap) = manifest.lock(config.clone()).unwrap();
    assert_eq!(new_lock, lock);

    // Delete the lock file, and try again. It should get re-fetched.
    std::fs::remove_file(manifest.default_lockfile_path().unwrap()).unwrap();
    let (new_lock, _snap) = manifest.lock(config).unwrap();
    assert_ne!(new_lock, lock);
}

// If you change the git dependency specification in the manifest, it will get
// re-fetched.
#[test]
fn fetch_if_spec_changes() {
    let git_dir = init_git();
    let pkg_dir = TempDir::new().unwrap();
    let mut manifest = ManifestBuilder::default()
        .with_dir(pkg_dir.path())
        .with_git_dep("dep", git_dir.path())
        .build();
    let (_cache_dir, config) = test_config();
    let (lock, _snap) = manifest.lock(config.clone()).unwrap();
    let lock_contents = serde_json::to_string_pretty(&lock).unwrap();
    assert_lock_snapshot_filtered!("no_fetch_if_exact_match", lock_contents);

    // Now modify the git repo. It shouldn't get re-fetched.
    add_git_file(&git_dir, "main.ncl");
    let (new_lock, _snap) = manifest.lock(config.clone()).unwrap();
    assert_eq!(new_lock, lock);

    // Modify the manifest's dependency spec. It should get re-fetched.
    let Some(Dependency::Git(old_dep)) = manifest.dependencies.get_mut(&Ident::new("dep")) else {
        unreachable!()
    };
    old_dep.target = Target::Branch("master".to_owned());
    let (new_lock, _snap) = manifest.lock(config).unwrap();
    assert_ne!(new_lock, lock);
}

// Two dependencies with different git specs get updated independently. Even if they might
// in principle point at the same revision, they could be out of sync.
#[test]
fn different_specs_different_ids() {
    let git_dir = init_git();
    let pkg_dir = TempDir::new().unwrap();
    let mut manifest = ManifestBuilder::default()
        .with_dir(pkg_dir.path())
        .with_git_dep("dep", git_dir.path())
        .with_git_dep("dep2", git_dir.path())
        .build();
    let (_cache_dir, config) = test_config();
    let (lock, _snap) = manifest.lock(config.clone()).unwrap();

    add_git_file(&git_dir, "main.ncl");

    // Modify one of the two dependency specs. It should get re-fetched but the other one shouldn't.
    let Some(Dependency::Git(old_dep)) = manifest.dependencies.get_mut(&Ident::new("dep2")) else {
        unreachable!()
    };
    old_dep.target = Target::Branch("master".to_owned());
    dbg!(&lock);
    let (new_lock, _snap) = manifest.lock(config).unwrap();

    dbg!(&new_lock);
    assert_eq!(lock.packages.len(), 1);
    assert_eq!(new_lock.packages.len(), 2);

    let dep_name = EntryName {
        name: "dep".to_owned(),
        id: 0,
    };
    let dep2_name = EntryName {
        name: "dep2".to_owned(),
        id: 0,
    };
    let dep_lock = &lock.packages[&dep_name];
    let dep_new_lock = &new_lock.packages[&dep_name];
    let dep2_new_lock = &new_lock.packages[&dep2_name];

    assert_eq!(dep_lock, dep_new_lock);
    assert_ne!(dep_lock, dep2_new_lock);
}
