use nickel_lang_core::identifier::Ident;
use nickel_lang_git::Target;
use nickel_lang_package::{snapshot::Snapshot, Dependency};
use tempfile::TempDir;

mod util;

use util::{add_git_file, init_git, init_pkg, test_config, ManifestBuilder};

#[test]
fn empty_up_to_date() {
    let pkg_dir = TempDir::new().unwrap();
    let manifest = ManifestBuilder::default().with_dir(pkg_dir.path()).build();
    let (_cache_dir, config) = test_config();

    let (lock, resolution) = manifest.lock(config).unwrap();

    assert!(manifest.is_lock_file_up_to_date(&resolution.snapshot, &lock));
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

    let (lock, resolution) = manifest.lock(config.clone()).unwrap();

    assert!(manifest.is_lock_file_up_to_date(&resolution.snapshot, &lock));

    // If we modify the git dep, it will still count as up-to-date.
    add_git_file(&git_dir, "main.ncl");
    let snap = Snapshot::new(&config, pkg_dir.path(), &manifest).unwrap();
    assert!(manifest.is_lock_file_up_to_date(&snap, &lock));

    // If we modify the git spec in the manifest, it won't be up-to-date anymore.
    let Some(Dependency::Git(dep)) = manifest.dependencies.get_mut(&Ident::new("dep")) else {
        unreachable!()
    };
    dep.target = Target::Branch("master".to_owned());
    assert!(!manifest.is_lock_file_up_to_date(&snap, &lock));

    // Re-lock, and it will be up-to-date again.
    let (lock, resolution) = manifest.lock(config).unwrap();
    assert!(manifest.is_lock_file_up_to_date(&resolution.snapshot, &lock));
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
    let (lock, resolution) = manifest.lock(config.clone()).unwrap();

    assert!(manifest.is_lock_file_up_to_date(&resolution.snapshot, &lock));

    // If we change a path package but don't add new dependencies, the lock is still considered
    // up-to-date.
    let manifest_text = r#"
    {
      name = "new-name",
      description = "New description",
      version = "0.1.0",
      minimal_nickel_version = "1.9.0",
      authors = ["Me"],
      dependencies = {},
    } | std.package.Manifest
    "#;
    std::fs::write(dep_dir.path().join("Nickel-pkg.ncl"), manifest_text).unwrap();

    let snap = Snapshot::new(&config, pkg_dir.path(), &manifest).unwrap();
    assert!(manifest.is_lock_file_up_to_date(&snap, &lock));

    // If we add a dependency but don't update the snapshot, it will still be
    // up-to-date relative to the snapshot.
    let foo_dir = init_pkg();
    let manifest_text = format!(
        r#"
    {{
      name = "new-name",
      description = "New description",
      version = "0.1.0",
      minimal_nickel_version = "1.9.0",
      authors = ["Me"],
      dependencies = {{ foo = 'Path m%"{}"% }},
    }} | std.package.Manifest
    "#,
        foo_dir.path().display()
    );
    dbg!(&manifest_text);
    std::fs::write(dep_dir.path().join("Nickel-pkg.ncl"), manifest_text).unwrap();
    assert!(manifest.is_lock_file_up_to_date(&snap, &lock));

    // Now update the snapshot and we'll see it isn't up-to-date.
    let snap = Snapshot::new(&config, pkg_dir.path(), &manifest).unwrap();
    assert!(!manifest.is_lock_file_up_to_date(&snap, &lock));
}
