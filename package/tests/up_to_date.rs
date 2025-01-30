use nickel_lang_core::identifier::Ident;
use nickel_lang_git::Target;
use nickel_lang_package::Dependency;
use tempfile::TempDir;

mod util;

use util::{add_git_file, init_git, init_pkg, test_config, ManifestBuilder};

#[test]
fn empty_up_to_date() {
    let pkg_dir = TempDir::new().unwrap();
    let manifest = ManifestBuilder::default().with_dir(pkg_dir.path()).build();
    let (_cache_dir, config) = test_config();

    let (lock, _snap) = manifest.lock(config).unwrap();

    assert!(manifest.is_lock_file_up_to_date(&lock));
}

#[test]
fn git_dep_up_to_date() {
    let git_dir = init_git();
    let pkg_dir = TempDir::new().unwrap();
    let mut manifest = ManifestBuilder::default()
        .with_dir(pkg_dir.path())
        .with_git_dep("dep", git_dir.path())
        .build();
    let (_cache_dir, config) = test_config();

    let (lock, _snap) = manifest.lock(config.clone()).unwrap();

    assert!(manifest.is_lock_file_up_to_date(&lock));

    // If we modify the git dep, it will still count as up-to-date.
    add_git_file(&git_dir, "main.ncl");
    assert!(manifest.is_lock_file_up_to_date(&lock));

    // If we modify the git spec in the manifest, it won't be up-to-date anymore.
    let Some(Dependency::Git(dep)) = manifest.dependencies.get_mut(&Ident::new("dep")) else {
        unreachable!()
    };
    dep.target = Target::Branch("master".to_owned());
    assert!(!manifest.is_lock_file_up_to_date(&lock));

    // Re-lock, and it will be up-to-date again.
    let (lock, _snap) = manifest.lock(config).unwrap();
    assert!(manifest.is_lock_file_up_to_date(&lock));
}

#[test]
fn path_dep_up_to_date() {
    let pkg_dir = TempDir::new().unwrap();
    let dep_dir = init_pkg();
    let manifest = ManifestBuilder::default()
        .with_dir(pkg_dir.path())
        .with_path_dep("dep", dep_dir.path())
        .build();
    let (_cache_dir, config) = test_config();
    let (lock, _snap) = manifest.lock(config.clone()).unwrap();

    // Path deps are always up to date.
    assert!(manifest.is_lock_file_up_to_date(&lock));
    std::fs::write(dep_dir.path().join("package.ncl"), "hi").unwrap();
    assert!(manifest.is_lock_file_up_to_date(&lock));
}
