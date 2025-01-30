// We avoid re-fetching git repositories if the lock-file matches the requirement.
// This file tests the various cases.

use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use nickel_lang_core::identifier::Ident;
use nickel_lang_git::Target;
use nickel_lang_package::{
    config::Config, lock::EntryName, version::SemVer, Dependency, GitDependency, ManifestFile,
};
use tempfile::TempDir;

const DUMMY_MANIFEST: &str = r#"
{
  name = "pkg",
  description = "A manifest, because packages need manifests",
  version = "0.1.0",
  minimal_nickel_version = "1.9.0",
  authors = ["Me"],
  dependencies = {},
} | std.package.Manifest
"#;

// TODO: figure out where to share code for test utils
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

macro_rules! run {
    { $dir:expr, $($fmt_args:tt)* } => {{
        let s = format!($($fmt_args)*);
        let mut words = s.split_ascii_whitespace();
        let mut cmd = std::process::Command::new(words.next().unwrap());
        let cmd = cmd.args(words).current_dir($dir);
        let output = cmd.output().unwrap();
        assert!(
            output.status.success(),
            "command {cmd:?} failed, stdout {}, stderr {}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }};
}

struct ManifestBuilder {
    parent_dir: Option<PathBuf>,
    name: String,
    dependencies: HashMap<Ident, Dependency>,
}

impl Default for ManifestBuilder {
    fn default() -> Self {
        Self {
            parent_dir: None,
            name: "my-package".to_owned(),
            dependencies: HashMap::new(),
        }
    }
}

impl ManifestBuilder {
    fn with_dir(self, dir: impl AsRef<Path>) -> Self {
        ManifestBuilder {
            parent_dir: Some(dir.as_ref().to_owned()),
            ..self
        }
    }

    fn with_git_dep(mut self, name: impl AsRef<str>, dep_path: impl AsRef<Path>) -> Self {
        self.dependencies.insert(
            Ident::new(name.as_ref()),
            Dependency::Git(GitDependency {
                url: gix::Url::try_from(dep_path.as_ref().display().to_string()).unwrap(),
                target: nickel_lang_git::Target::Head,
                path: PathBuf::new(),
            }),
        );
        self
    }

    fn build(self) -> ManifestFile {
        ManifestFile {
            parent_dir: self.parent_dir,
            name: Ident::new(&self.name),
            version: SemVer::new(0, 0, 1),
            minimal_nickel_version: SemVer::new(2, 0, 0),
            dependencies: self.dependencies,
        }
    }
}

fn init_git() -> TempDir {
    let dir = TempDir::new().unwrap();
    run!(dir.path(), "git init");
    run!(dir.path(), "git config user.email me@example.com");
    run!(dir.path(), "git config user.name me");

    let manifest_path = dir.path().join("package.ncl");
    std::fs::write(&manifest_path, DUMMY_MANIFEST).unwrap();
    run!(dir.path(), "git add {}", manifest_path.display());
    run!(dir.path(), "git commit -m initial");
    dir
}

fn add_git_file(dir: &TempDir, filename: &str) {
    let new_file = dir.path().join(filename);
    std::fs::write(&new_file, "42").unwrap();
    run!(dir.path(), "git add {filename}");
    run!(dir.path(), "git commit -m commit");
}

fn test_config() -> (TempDir, Config) {
    let cache_dir = TempDir::new().unwrap();
    let config = Config::new()
        .unwrap()
        .with_cache_dir(cache_dir.path().to_owned());
    (cache_dir, config)
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
    let (new_lock, _snap) = manifest.lock(config).unwrap();

    dbg!(&lock);
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
