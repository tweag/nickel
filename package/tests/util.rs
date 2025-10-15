// This file is used as a module by different tests, and not all of them use everything.
#![allow(dead_code)]

use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    process::Command,
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

/// A version of the run macro that doesn't do its own hacky arg splitting,
/// and so is safer to use with things like paths that might contains spaces.
fn run(cmd: &mut Command) {
    let output = cmd.output().unwrap();
    assert!(
        output.status.success(),
        "command {cmd:?} failed, stdout {}, stderr {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

pub struct ManifestBuilder {
    parent_dir: Option<PathBuf>,
    name: String,
    dependencies: HashMap<Ident, Dependency>,
    version: Option<SemVer>,
}

impl Default for ManifestBuilder {
    fn default() -> Self {
        Self {
            parent_dir: None,
            name: "my-package".to_owned(),
            dependencies: HashMap::new(),
            version: None,
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

    pub fn with_version(self, version: SemVer) -> Self {
        Self {
            version: Some(version),
            ..self
        }
    }

    pub fn build(self) -> ManifestFile {
        ManifestFile {
            parent_dir: self.parent_dir.expect("need a parent dir"),
            name: Ident::new(&self.name),
            version: self.version.unwrap_or(SemVer::new(0, 0, 1)),
            minimal_nickel_version: SemVer::new(2, 0, 0),
            dependencies: self.dependencies,
            authors: Vec::new(),
            description: String::new(),
            keywords: Vec::new(),
            license: String::new(),
        }
    }
}

/// A builder interface for [`Package`].
#[derive(Default)]
pub struct PackageBuilder {
    manifest: Option<ManifestFile>,
    repo_dir: Option<PathBuf>,
    id: Option<index::Id>,
}

impl PackageBuilder {
    /// Set the package's manifest file.
    pub fn with_manifest(mut self, manifest: ManifestFile) -> Self {
        self.manifest = Some(manifest);
        self
    }

    /// Set the directory containing the package's contents.
    ///
    /// If this is provided, the packages contents live in this location. Not
    /// necessarily the *root* of this location, but somewhere in it.
    ///
    /// When the package is published, the contents of this directory will be
    /// published too. For example, if this is the package for `github:foo/bar`
    /// then publishing the package will put all the contents of `repo_dir`
    /// into `https://github.com/foo/bar`. (Not for real, obviously: the fake
    /// local version of github that we're using for tests.)
    ///
    /// This is optional, because if this package only exists to test resolution
    /// then it doesn't need contents. Those are only needed for packages that
    /// want to test evaluation.
    pub fn with_repo_dir(mut self, path: impl AsRef<Path>) -> Self {
        self.repo_dir = Some(path.as_ref().to_owned());
        self
    }

    /// Set the package's id, like "github:foo/bar".
    pub fn with_id(mut self, id: &str) -> Self {
        self.id = Some(id.parse().unwrap());
        self
    }

    /// Build the package.
    ///
    /// Panics if either the id or the manifest is missing.
    pub fn build(self) -> Package {
        Package {
            manifest: self.manifest.unwrap(),
            repo_dir: self.repo_dir,
            id: self.id.unwrap(),
        }
    }
}

/// A fake package for testing.
pub struct Package {
    manifest: ManifestFile,
    id: index::Id,
    repo_dir: Option<PathBuf>,
}

impl Package {
    /// Publishes this package in a test environment.
    ///
    /// Assumes that the "remote" index url and the "remote" github url are
    /// actually local paths. Writes an index entry into the "remote" index
    /// and puts the package contents in the appropriate place on "github"
    pub fn publish(&self, config: &Config) {
        eprintln!("publishing {}", &self.id);
        let pkg = index::scrape::read_from_manifest(&self.id, &self.manifest).unwrap();

        // We don't put the package in config.index_dir, because that's where our downloaded index
        // goes. Instead we put them in the fake github index.
        assert!(config.index_url.scheme.as_str() == "file");
        let dir = Path::new(&config.index_url.path.to_os_str().unwrap()).to_owned();
        let config = config.clone().with_index_dir(dir.clone());

        PackageIndex::exclusive(config.clone())
            .unwrap()
            .save(pkg.clone())
            .unwrap();

        // Because the index will be fetched over git, we need to commit the changes.
        run!(&dir, "git add .");
        run!(&dir, "git commit -m update");

        if let Some(contents_dir) = &self.repo_dir {
            // Now copy the package contents to where they belong.
            let download_spec = pkg.id.download_spec(&config);
            let github_dir = Path::new(download_spec.url.path.to_os_str().unwrap());
            dbg!(&contents_dir, &github_dir);

            // If we've already published a version of the package then the directory
            // will already exist. In that case, replace the contents and make a new
            // commit.
            if github_dir.exists() {
                modify_git_repo(contents_dir, github_dir);
            } else {
                set_up_git_repo(contents_dir, github_dir);
            }
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

// Copies the directory `contents` to `to`, and initializes a git repo in the
// new location.
//
// The directory at `to` is assumed not to exist yet.
pub fn set_up_git_repo(contents: &Path, to: &Path) {
    // The rust stdlib doesn't have anything for recursively copying a directory. There are
    // some crates for that, but it's easier just to shell out.
    std::fs::create_dir_all(to.parent().unwrap()).unwrap();
    run(Command::new("cp").arg("-r").args([contents, to]));

    // We have some hacky ways to test branch/tag fetching: if the input contains a tag.txt file,
    // make a git tag named with the contents of that file. If the input contains a branch.txt file,
    // make a git branch named with the contents of that file.
    let tag = std::fs::read_to_string(to.join("tag.txt")).ok();
    let branch = std::fs::read_to_string(to.join("branch.txt")).ok();

    run!(to, "git init");
    run!(to, "git config user.email me@example.com");
    run!(to, "git config user.name me");

    if let Some(branch) = branch {
        run!(to, "git commit -m initial --allow-empty");
        run!(to, "git checkout -b {}", branch.trim());
    }

    run!(to, "git add --all");
    run!(to, "git commit -m initial");

    if let Some(tag) = tag {
        run!(to, "git tag {}", tag.trim());
    }
}

// Copies everything in `contents` to `to` (which we assume already exists),
// and commits them to the git repo that we assume is already in `to`.
pub fn modify_git_repo(contents: &Path, to: &Path) {
    // Delete the old contents (except the .git directory)
    for old in std::fs::read_dir(to).unwrap() {
        let old = old.unwrap();
        if old.path().file_name().unwrap().to_str() != Some(".git") {
            run(Command::new("rm").arg("-rf").arg(old.path()));
        }
    }

    // Now expand `cp -r contents/* to`.
    let contents = std::fs::read_dir(contents).unwrap();
    for elt in contents {
        run(Command::new("cp")
            .arg("-r")
            .arg(elt.unwrap().path())
            .arg(to));
    }

    run!(to, "git add --all");
    run!(to, "git commit -m update --allow-empty");
}
