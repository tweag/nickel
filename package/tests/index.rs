// Basic tests of index dependencies, including tests for when the index is refreshed.

use tempfile::tempdir;

mod util;

use util::{init_git, publish_package, test_config, ManifestBuilder};

#[test]
fn index_fetch_on_lock() {
    let (_tmpdir, config) = test_config();
    let git_dir = init_git();

    // This manifest doesn't match what's stored in the git dir, but no one checks...
    let index_manifest = ManifestBuilder::default().with_dir(git_dir.path()).build();

    publish_package(&config, &index_manifest, "github:myorg/mypackage");

    // No one has downloaded the index yet.
    assert!(!config.index_dir.exists());

    let tmp_dir = tempdir().unwrap();
    let manifest = ManifestBuilder::default()
        .with_dir(tmp_dir.path())
        .with_index_dep("mypackage", "github:myorg/mypackage", "=0.0.1")
        .build();
    manifest.lock(config.clone()).unwrap();

    // Locking triggered an index download
    assert!(config.index_dir.exists());
    assert!(config.index_dir.join("github/myorg/mypackage").exists());

    publish_package(&config, &index_manifest, "github:myorg/myotherpackage");

    // Locking again won't trigger an index update, because it wasn't necessary.
    manifest.lock(config.clone()).unwrap();
    assert!(!config
        .index_dir
        .join("github/myorg/myotherpackage")
        .exists());

    // Modifying our manifest so that it's out-of-date will trigger a refresh.
    let manifest = ManifestBuilder::default()
        .with_dir(tmp_dir.path())
        .with_index_dep("mypackage", "github:myorg/myotherpackage", "=0.0.1")
        .build();
    manifest.lock(config.clone()).unwrap();
    assert!(config
        .index_dir
        .join("github/myorg/myotherpackage")
        .exists());
}
