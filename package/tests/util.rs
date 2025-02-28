// This file is used as a module by different tests, and not all of them use everything.
#![allow(dead_code)]

use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    str::FromStr as _,
};

use gix::bstr::ByteSlice as _;
use nickel_lang_core::identifier::Ident;
use nickel_lang_package::{
    config::Config,
    index::{self, PackageIndex},
    manifest::MANIFEST_NAME,
    version::SemVer,
    Dependency, GitDependency, IndexDependency, ManifestFile,
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

/// A macro for running a command and panicking on failure.
///
/// This doesn't do any escaping, or protect against shell injection.
/// For internal use only!
#[macro_export]
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

pub struct ManifestBuilder {
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
    pub fn with_dir(self, dir: impl AsRef<Path>) -> Self {
        ManifestBuilder {
            parent_dir: Some(dir.as_ref().to_owned()),
            ..self
        }
    }

    pub fn with_git_dep(mut self, name: impl AsRef<str>, dep_path: impl AsRef<Path>) -> Self {
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

    pub fn with_path_dep(mut self, name: impl AsRef<str>, path: impl AsRef<Path>) -> Self {
        self.dependencies.insert(
            Ident::new(name.as_ref()),
            Dependency::Path(path.as_ref().to_owned()),
        );
        self
    }

    pub fn with_index_dep(
        mut self,
        name: impl AsRef<str>,
        id: impl AsRef<str>,
        version: impl AsRef<str>,
    ) -> Self {
        self.dependencies.insert(
            Ident::new(name.as_ref()),
            Dependency::Index(IndexDependency {
                id: id.as_ref().parse().unwrap(),
                version: version.as_ref().parse().unwrap(),
            }),
        );
        self
    }

    pub fn build(self) -> ManifestFile {
        ManifestFile {
            parent_dir: self.parent_dir.expect("need a parent dir"),
            name: Ident::new(&self.name),
            version: SemVer::new(0, 0, 1),
            minimal_nickel_version: SemVer::new(2, 0, 0),
            dependencies: self.dependencies,
            authors: Vec::new(),
            description: String::new(),
            keywords: Vec::new(),
            license: String::new(),
        }
    }
}

/// Creates a new git repository in a temporary directory.
///
/// The git repo will contain a dummy manifest file with no dependencies,
/// so that you can use it as a git dependency.
pub fn init_git() -> TempDir {
    let dir = TempDir::new().unwrap();
    run!(dir.path(), "git init --initial-branch=master");
    run!(dir.path(), "git config user.email me@example.com");
    run!(dir.path(), "git config user.name me");

    let manifest_path = dir.path().join(MANIFEST_NAME);
    std::fs::write(&manifest_path, DUMMY_MANIFEST).unwrap();
    run!(dir.path(), "git add {}", manifest_path.display());
    run!(dir.path(), "git commit -m initial");
    dir
}

/// Adds a file to a temporary git repository.
pub fn add_git_file(dir: &TempDir, filename: &str) {
    let new_file = dir.path().join(filename);
    std::fs::write(&new_file, "42").unwrap();
    run!(dir.path(), "git add {filename}");
    run!(dir.path(), "git commit -m commit");
}

/// Creates a new configuration suitable for tests (i.e. with a clean, temporary
/// cache directory).
pub fn test_config() -> (TempDir, Config) {
    let dir = TempDir::new().unwrap();
    let cache_dir = dir.path().join("cache_dir");
    let remote_index_dir = dir.path().join("remote_index");
    let remote_package_dir = dir.path().join("remote_package");

    std::fs::create_dir_all(&remote_index_dir).unwrap();
    run!(&remote_index_dir, "git init");
    run!(&remote_index_dir, "git config user.email me@example.com");
    run!(&remote_index_dir, "git config user.name me");
    run!(&remote_index_dir, "git commit -m initial --allow-empty");

    let config = Config::new()
        .unwrap()
        .with_cache_dir(cache_dir)
        .with_index_url(remote_index_dir.to_str().unwrap().try_into().unwrap())
        .with_github_package_url(remote_package_dir.to_str().unwrap().try_into().unwrap());
    (dir, config)
}

/// Creates a new package in a temporary directory.
pub fn init_pkg() -> TempDir {
    let dir = TempDir::new().unwrap();
    let manifest_path = dir.path().join(MANIFEST_NAME);
    std::fs::write(&manifest_path, DUMMY_MANIFEST).unwrap();
    dir
}

/// Add a package to our local test index.
pub fn publish_package(config: &Config, manifest: &ManifestFile, id: &str) {
    eprintln!("publishing {id}");
    let id = index::Id::from_str(id).unwrap();
    let pkg = index::scrape::read_from_manifest(&id, manifest).unwrap();

    // We don't put the package in config.index_dir, because that's where our downloaded packages
    // go. Instead we put them in the fake github index.
    assert!(config.index_url.scheme.as_str() == "file");
    let dir = Path::new(&config.index_url.path.to_os_str().unwrap()).to_owned();
    let config = config.clone().with_index_dir(dir.clone());

    PackageIndex::exclusive(config).unwrap().save(pkg).unwrap();

    // Because the index will be fetched over git, we need to commit the changes.
    run!(&dir, "git add .");
    run!(&dir, "git commit -m update");
}
