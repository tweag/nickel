use libtest_mimic::{Arguments, Trial};
use std::{
    path::Path,
    process::{Command, ExitCode},
    sync::Arc,
};
use util::publish_package;

use nickel_lang_core::error::report::report_as_str;
use nickel_lang_package::{
    config::Config, index::PackageIndex, lock::LockFile, manifest::MANIFEST_NAME, resolve,
    snapshot::Snapshot, ManifestFile,
};
use nickel_lang_utils::project_root::project_root;
use tempfile::TempDir;

mod util;

pub fn main() -> ExitCode {
    let args = Arguments::from_args();
    let root = project_root();
    let root_str = project_root().into_os_string().into_string().unwrap();

    let manifest_glob = glob::glob(&format!(
        "{root_str}/package/tests/integration/inputs/path/**/Nickel-pkg.ncl"
    ))
    .unwrap();

    let fixture = Arc::new(Fixture::default());

    let tests: Vec<_> = manifest_glob
        .map(|p| {
            let path = p.unwrap();
            let name = path.strip_prefix(&root).unwrap().to_owned();
            let fixture = fixture.clone();
            Trial::test(name.display().to_string(), move || {
                generate_lock_file(&name, &fixture.config);
                Ok(())
            })
        })
        .collect();

    libtest_mimic::run(&args, tests).exit_code()
}

macro_rules! assert_lock_snapshot_filtered {
    { $name:expr, $snapshot:expr } => {
        insta::with_settings!({filters => vec![
            // Lock files contain git ids, and I haven't figured out if it's possible to
            // get them consistent between runs (e.g., they include hashes of timestamps).
            // So we just filter them out of the comparison.
            (r#""id": "[a-z0-9]+""#, r#""id": <GENERATED>"#)
        ]},
        {
            insta::assert_snapshot!($name, $snapshot);
        })
    }
}

macro_rules! assert_snapshot_filtered {
    { $name:expr, $snapshot:expr } => {
        insta::with_settings!({filters => vec![
            // Error messages contain paths, and windows displays them differently
            (r#"'Path [^ ]+"#, r#"'Path <filtered>"#)
        ]},
        {
            insta::assert_snapshot!($name, $snapshot);
        })
    }
}

// We'd like to test git dependencies, but it's considered bad form (and is annoying to manage)
// to nest the test git repos in our main repo. So what we do is just keep the contents of our
// test git repos in `package/tests/integration/inputs/git`. Then when we run our tests, we
// create temporary git repos for these contents, and use the source replacement mechanism
// to redirect to these temporary git repos.
struct Fixture {
    _tmp_dir: TempDir,
    _git_repos: TempDir,
    config: Config,
}

impl Default for Fixture {
    fn default() -> Self {
        let (tmp_dir, mut config) = util::test_config();
        let git_repos = TempDir::new().unwrap();

        set_up_git_repos(&mut config, &git_repos);
        set_up_test_index(&config, git_repos.path());

        Fixture {
            _tmp_dir: tmp_dir,
            _git_repos: git_repos,
            config,
        }
    }
}

// Creates git repos and populates them with files from our integration test suite.
//
// Modifies the provided config so that the git dependency
// `https://example.com/my-git-repo` will be redirected a git repo with the
// contents of `package/tests/integration/inputs/git/my-git-repo`
fn set_up_git_repos(config: &mut Config, git_dir: &TempDir) {
    let git_inputs =
        std::fs::read_dir(project_root().join("package/tests/integration/inputs/git")).unwrap();

    for input in git_inputs {
        let input = input.unwrap();
        let input_path = input.path();
        let file_name = input_path.file_name().unwrap();

        let dir_path = git_dir.path().join(file_name);

        set_up_git_repo(&input_path, &dir_path);
        let orig_url = gix::Url::try_from(format!(
            "https://example.com/{}",
            Path::new(file_name).display()
        ))
        .unwrap();
        let new_url = gix::Url::try_from(dir_path.display().to_string()).unwrap();
        config.git_replacements.insert(orig_url, new_url);
    }
}

// Copies the directory `contents` to `to`, and initializes a git repo in the
// new location.
fn set_up_git_repo(contents: &Path, to: &Path) {
    let run = |cmd: &mut Command| {
        let output = cmd.output().unwrap();
        assert!(
            output.status.success(),
            "command {cmd:?} failed, stdout {}, stderr {}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    };

    let run_in_dir = |cmd: &mut Command| {
        run(cmd.current_dir(to));
    };

    // The rust stdlib doesn't have anything for recursively copying a directory. There are
    // some crates for that, but it's easier just to shell out.
    run(Command::new("cp").arg("-r").arg(contents).arg(to));

    // We have some hacky ways to test branch/tag fetching: if the input contains a tag.txt file,
    // make a git tag named with the contents of that file. If the input contains a branch.txt file,
    // make a git branch named with the contents of that file.
    let tag = std::fs::read_to_string(to.join("tag.txt")).ok();
    let branch = std::fs::read_to_string(to.join("branch.txt")).ok();

    run_in_dir(Command::new("git").arg("init"));
    run_in_dir(Command::new("git").args(["config", "user.email", "me@example.com"]));
    run_in_dir(Command::new("git").args(["config", "user.name", "me"]));

    if let Some(branch) = branch {
        run_in_dir(Command::new("git").args(["commit", "-m", "initial", "--allow-empty"]));
        run_in_dir(Command::new("git").args(["checkout", "-b", branch.trim()]));
    }

    run_in_dir(Command::new("git").args(["add", "--all"]));
    run_in_dir(Command::new("git").args(["commit", "-m", "initial"]));

    if let Some(tag) = tag {
        run_in_dir(Command::new("git").args(["tag", tag.trim()]));
    }
}

// Creates an index in the configured location (which must be a directory), and
// populates it with the packages found in `package/tests/integration/inputs/index`.
fn set_up_test_index(config: &Config, tmp_dir: &Path) {
    // The packages for populating our index stored in the form
    // package/tests/integration/inputs/index/github/<org>/<package>/<version>.
    let index_dir =
        std::fs::read_dir(project_root().join("package/tests/integration/inputs/index/github"))
            .unwrap();
    for org in index_dir {
        let org = org.unwrap();
        for pkg in std::fs::read_dir(org.path()).unwrap() {
            let pkg = pkg.unwrap();
            for version in std::fs::read_dir(pkg.path()).unwrap() {
                let version = version.unwrap();

                // Package publishing needs a git repo to get a hash from, so first
                // copy the contents into a git repo.
                let tmp_location = tmp_dir
                    .join("index-git-tmp")
                    .join(org.file_name())
                    .join(pkg.file_name())
                    .join(version.file_name());
                std::fs::create_dir_all(tmp_location.parent().unwrap()).unwrap();
                set_up_git_repo(&version.path(), &tmp_location);

                let manifest_path = tmp_location.join(MANIFEST_NAME);
                let manifest = ManifestFile::from_path(&manifest_path).unwrap();
                assert_eq!(
                    manifest.version,
                    version.file_name().into_string().unwrap().parse().unwrap()
                );
                publish_package(
                    config,
                    &manifest,
                    &format!(
                        "github:{}/{}",
                        org.file_name().into_string().unwrap(),
                        pkg.file_name().into_string().unwrap()
                    ),
                );
            }
        }
    }
}

fn generate_lock_file(path: &Path, config: &Config) {
    let full_path = project_root().join(path);
    let index_dir = TempDir::new().unwrap();

    let config = config.clone();

    // Make an empty git repo as the index.
    Command::new("git")
        .arg("init")
        .current_dir(index_dir.path())
        .output()
        .unwrap();
    Command::new("git")
        .args(["config", "user.email", "me@example.com"])
        .current_dir(index_dir.path())
        .output()
        .unwrap();
    Command::new("git")
        .args(["config", "user.name", "Me"])
        .current_dir(index_dir.path())
        .output()
        .unwrap();

    Command::new("git")
        .args(["commit", "--allow-empty", "-m", "initial"])
        .current_dir(index_dir.path())
        .output()
        .unwrap();

    let manifest = match ManifestFile::from_path(&full_path) {
        Ok(m) => m,
        Err(nickel_lang_package::error::Error::ManifestEval {
            package: _package,
            mut files,
            error,
        }) => {
            panic!("{}", report_as_str(&mut files, error, Default::default()));
        }
        Err(e) => panic!("{}", e),
    };
    let index = PackageIndex::shared_or_initialize(config.clone()).unwrap();

    let snap = Snapshot::new(&config, &manifest.parent_dir, &manifest).unwrap();
    match resolve::resolve(&manifest, snap, index, config) {
        Ok(resolution) => {
            let lock = LockFile::new(&manifest, &resolution).unwrap();
            let lock_contents = serde_json::to_string_pretty(&lock).unwrap();

            assert_lock_snapshot_filtered!(path.display().to_string(), lock_contents);
        }
        Err(e) => {
            assert_snapshot_filtered!(path.display().to_string(), e.to_string());
        }
    }
}
