use std::{path::Path, process::Command};

use nickel_lang_git::Target;
use tempfile::tempdir;

fn write_contents(dir: &Path, branch: &str) {
    let contents = dir.join("contents.txt");

    let run = |cmd: &mut Command| {
        assert!(cmd.current_dir(dir).output().unwrap().status.success());
    };

    run(Command::new("git").args(["checkout", "-b", branch]));
    std::fs::write(&contents, branch.as_bytes()).unwrap();
    run(Command::new("git").args(["add", contents.as_os_str().to_str().unwrap()]));
    run(Command::new("git").args(["commit", "-m", "foo"]));
}

fn check_contents(dir: &Path, target: Target, contents: &str) {
    let out_dir = tempdir().unwrap();
    let spec = nickel_lang_git::Spec {
        url: dir.try_into().unwrap(),
        target,
    };
    let _id = nickel_lang_git::fetch(&spec, out_dir.path()).unwrap();

    let contents_path = out_dir.path().join("contents.txt");
    assert_eq!(1, std::fs::read_dir(out_dir.path()).unwrap().count());
    let actual = std::fs::read_to_string(contents_path).unwrap();
    assert_eq!(contents, actual);
}

fn check_missing(dir: &Path, target: Target) {
    let out_dir = tempdir().unwrap();
    let spec = nickel_lang_git::Spec {
        url: dir.try_into().unwrap(),
        target,
    };
    let result = nickel_lang_git::fetch(&spec, out_dir.path());

    assert!(matches!(
        result,
        Err(nickel_lang_git::Error::TargetNotFound(_))
    ));
}

// Test that the various targets (branch, tag, commit) fetch the expected contents.
#[test]
fn fetch_targets() {
    let repo = tempdir().unwrap();

    let run = |cmd: &mut Command| {
        let output = cmd.current_dir(repo.path()).output().unwrap();
        assert!(output.status.success());
        output.stdout
    };

    run(Command::new("git").arg("init"));
    run(Command::new("git").args(["branch", "-m", "main"]));

    write_contents(repo.path(), "main");
    write_contents(repo.path(), "other_branch");

    run(Command::new("git").args(["tag", "a_tag"]));
    let main_hash = run(Command::new("git").args(["rev-parse", "main"]));
    let main_hash = std::str::from_utf8(main_hash.trim_ascii()).unwrap();
    let branch_hash = run(Command::new("git").args(["rev-parse", "other_branch"]));
    let branch_hash = std::str::from_utf8(branch_hash.trim_ascii()).unwrap();

    check_contents(repo.path(), Target::Head, "other_branch");
    check_contents(repo.path(), Target::Branch("main".to_owned()), "main");
    check_contents(
        repo.path(),
        Target::Branch("other_branch".to_owned()),
        "other_branch",
    );
    check_contents(
        repo.path(),
        Target::Commit(main_hash.parse().unwrap()),
        "main",
    );
    check_contents(
        repo.path(),
        Target::Commit(branch_hash.parse().unwrap()),
        "other_branch",
    );
    check_contents(repo.path(), Target::Tag("a_tag".to_owned()), "other_branch");

    check_missing(repo.path(), Target::Tag("not_a_tag".to_owned()));
    check_missing(repo.path(), Target::Branch("not_a_branch".to_owned()));
    // We don't currently get a nice error for incorrect commit refs, because
    // gix doesn't give us back a useful structured error: we get a
    // FetchResponse(UploadPack(..)) with a string error message.
}
