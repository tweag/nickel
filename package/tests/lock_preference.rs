use nickel_lang_core::identifier::Ident;
use nickel_lang_package::{PrecisePkg, version::SemVer};
use tempfile::TempDir;

mod util;

use util::{ManifestBuilder, PackageBuilder, init_git, test_config};

#[test]
fn prefer_previously_locked() {
    let pkg_dir = TempDir::new().unwrap();
    let (_cache_dir, config) = test_config();

    // Put versions 0.1.0 and 0.1.1 in the index.
    let git_dir = init_git();
    let index_manifest = ManifestBuilder::default()
        .with_dir(git_dir.path())
        .with_version(SemVer::new(0, 1, 0))
        .build();
    PackageBuilder::default()
        .with_manifest(index_manifest.clone())
        .with_id("github:myorg/mypackage")
        .build()
        .publish(&config);

    let index_manifest = ManifestBuilder::default()
        .with_dir(git_dir.path())
        .with_version(SemVer::new(0, 1, 1))
        .build();
    PackageBuilder::default()
        .with_manifest(index_manifest.clone())
        .with_id("github:myorg/mypackage")
        .build()
        .publish(&config);

    // Set the manifest to have a dependency "dep" with a constraint "req" and
    // return the resolved version.
    //
    // The lock file is preserved across multiple calls to this closure.
    let resolve_for_version = |req: &str| -> SemVer {
        let manifest = ManifestBuilder::default()
            .with_dir(pkg_dir.path())
            .with_index_dep("dep", "github:myorg/mypackage", req)
            .build();
        let (_lock, resolution) = manifest.lock(config.clone()).unwrap();

        let dep = manifest.dependencies.get(&Ident::new("dep")).unwrap();
        let PrecisePkg::Index(idx) = resolution.precise(dep) else {
            panic!()
        };
        idx.version
    };

    // We default to the minimum allowed version.
    assert_eq!(resolve_for_version("0.1"), SemVer::new(0, 1, 0));

    // Update the version to 0.1.1, and that should get picked up.
    assert_eq!(resolve_for_version("0.1.1"), SemVer::new(0, 1, 1));

    // Revert back to 0.1.0 and the version will stay where it is,
    // because the lock-file version was compatible and so it won't update.
    assert_eq!(resolve_for_version("0.1"), SemVer::new(0, 1, 1));

    // The lock-file is gone, so it's back to the minimum version.
    std::fs::remove_file(pkg_dir.path().join("Nickel-pkg.lock")).unwrap();
    assert_eq!(resolve_for_version("0.1"), SemVer::new(0, 1, 0));
}
