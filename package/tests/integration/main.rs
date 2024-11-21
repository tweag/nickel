use std::{path::Path, process::Command};

use nickel_lang_package::{config::Config, lock::LockFile, ManifestFile};
use nickel_lang_utils::project_root::project_root;
use tempfile::TempDir;
use test_generator::test_resources;

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

// We'd like to test git dependencies, but it's considered bad form (and is annoying to manage)
// to nest the test git repos in our main repo. So what we do is just keep the contents of our
// test git repos in `package/tests/integration/inputs/git`. Then when we run our tests, we
// create temporary git repos for these contents, and use the source replacement mechanism
// to redirect to these temporary git repos.
//
// This function does all the git repo creation and population. We run it on every test, which is
// a bit wasteful because not every test needs every repo. Maybe we can share the set up step?
// The tests shouldn't modify the repos...
fn set_up_git_repos(config: &mut Config) -> TempDir {
    let tmp = TempDir::new().unwrap();
    let git_inputs =
        std::fs::read_dir(project_root().join("package/tests/integration/inputs/git")).unwrap();

    for input in git_inputs {
        let input = input.unwrap();
        let input_path = input.path();
        let file_name = input_path.file_name().unwrap();

        let dir_path = tmp.path().join(file_name);

        let run = |cmd: &mut Command| {
            assert!(cmd.output().unwrap().status.success());
        };

        let run_in_dir = |cmd: &mut Command| {
            run(cmd.current_dir(&dir_path));
        };

        // The rust stdlib doesn't have anything for recursively copying a directory. There are
        // some crates for that, but it's easier just to shell out.
        run(Command::new("cp")
            .arg("-r")
            .arg(&input_path)
            .arg(tmp.path()));

        // We have some hacky ways to test branch/tag fetching: if the input contains a tag.txt file,
        // make a git tag named with the contents of that file. If the input contains a branch.txt file,
        // make a git branch named with the contents of that file.
        let tag = std::fs::read_to_string(dir_path.join("tag.txt")).ok();
        let branch = std::fs::read_to_string(dir_path.join("branch.txt")).ok();

        run_in_dir(Command::new("git").arg("init"));

        if let Some(branch) = branch {
            run_in_dir(Command::new("git").args(["commit", "-m", "initial", "--allow-empty"]));
            run_in_dir(Command::new("git").args(["checkout", "-b", branch.trim()]));
        }

        run_in_dir(Command::new("git").args(["add", "--all"]));
        run_in_dir(Command::new("git").args(["commit", "-m", "initial"]));

        if let Some(tag) = tag {
            run_in_dir(Command::new("git").args(["tag", tag.trim()]));
        }

        let orig_url = gix::Url::try_from(format!(
            "https://example.com/{}",
            Path::new(file_name).display()
        ))
        .unwrap();
        let new_url = gix::Url::try_from(dir_path.display().to_string()).unwrap();
        config.git_replacements.insert(orig_url, new_url);
    }

    tmp
}

#[test_resources("package/tests/integration/inputs/path/**/package.ncl")]
fn generate_lock_file(path: &str) {
    let full_path = project_root().join(path);
    let cache_dir = TempDir::new().unwrap();
    let index_dir = TempDir::new().unwrap();
    let mut config = Config::default().with_cache_dir(cache_dir.path().to_owned());

    let _git_dir = set_up_git_repos(&mut config);

    // Make an empty git repo as the index.
    Command::new("git")
        .arg("init")
        .current_dir(index_dir.path())
        .output()
        .unwrap();
    Command::new("git")
        .args(["commit", "--allow-empty", "-m", "initial"])
        .current_dir(index_dir.path())
        .output()
        .unwrap();
    config.index_url = index_dir.path().try_into().unwrap();

    // TODO: test error messages also, and allow manifests to fail
    let manifest = ManifestFile::from_path(&full_path).unwrap();
    let resolution = manifest.resolve(config).unwrap();
    let lock = LockFile::new(&manifest, &resolution).unwrap();
    let lock_contents = serde_json::to_string_pretty(&lock).unwrap();

    assert_lock_snapshot_filtered!(path, lock_contents);
}
