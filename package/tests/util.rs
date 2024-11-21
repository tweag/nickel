use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use nickel_lang_core::identifier::Ident;
use nickel_lang_package::{
    config::Config, version::SemVer, Dependency, GitDependency, ManifestFile,
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

    #[allow(dead_code)]
    pub fn with_path_dep(mut self, name: impl AsRef<str>, path: impl AsRef<Path>) -> Self {
        self.dependencies.insert(
            Ident::new(name.as_ref()),
            Dependency::Path(path.as_ref().to_owned()),
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
        }
    }
}

/// Creates a new git repository in a temporary directory.
///
/// The git repo will contain a dummy `package.ncl` file with no dependencies,
/// so that you can use it as a git dependency.
pub fn init_git() -> TempDir {
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
    let cache_dir = TempDir::new().unwrap();
    let config = Config::new()
        .unwrap()
        .with_cache_dir(cache_dir.path().to_owned());
    (cache_dir, config)
}

/// Creates a new package in a temporary directory.
#[allow(dead_code)]
pub fn init_pkg() -> TempDir {
    let dir = TempDir::new().unwrap();
    let manifest_path = dir.path().join("package.ncl");
    std::fs::write(&manifest_path, DUMMY_MANIFEST).unwrap();
    dir
}
