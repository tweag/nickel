# Release process for Nickel

You're ready to release a new version of Nickel - congrats!

This document explains how to do so step by step while keeping the various
crates and dependent repositories (such as the website) in a consistent state.

## Steps to make a release

### Releasing script

**IMPORTANT**: Since the 1.4 release, `scripts/release.sh` takes care of bumping
versions numbers, updating local cross-dependencies, creating a clean release
branch, updating the stable branch and publishing to crates.io.

The covered steps are still described below for your information, but you
shouldn't need to actually perform them manually.

You'll still have to do the GitHub release, redeploy nickel-lang.org manually,
and backport changes to `master`, which are all described below as well.

#### Script requirements

- A relatively recent bash (> 4.3)
- git
- tomlq
- cargo
- You will need to be signed in to `crates.io` with a GitHub
    account that is part of the [Nickel Core
    team](https://github.com/orgs/nickel-lang/teams/core), and have a
    `crates.io` API key saved locally on your machine (normally via `cargo
    login`). For help with this, contact the Nickel maintainers.

Once `master` is in a releasable state, start the script from the root of the
`nickel` git repository with an argument that is either `major`, `minor` or
`patch`, indicating how to bump the version number. For example:

```console
$./release.sh minor
++ Nickel release script
++
++ This script will:
[..]
```

### About the version numbers

Some of the crates in the Nickel workspace are libraries which are not versioned
according to the version number of the language itself. These are

- `nickel-lang-core`
- `nickel-lang-utils`
- `lsp-harness`
- `nickel-wasm-repl`

Their version numbers take the form `0.W.U` and their public APIs are not
considered stable. Consequently we bump their versions to `0.(W+1).0` on every
release (if needed).

Other crates carry the version number of the Nickel language. These are

- `nickel-lang-cli`
- `nickel-lang-lsp`
- `pyckel`

### Prepare

**IMPORTANT**: this section is covered by the `release.sh` script, and is only
kept for information purpose. Usually, you shouldn't have to perform the
following steps manually.

1. Branch out from `master` to a dedicated branch `X.Y.Z-release`:
   `git checkout -b X.Y.Z-release`
2. Bump the overall workspace version number in `Cargo.toml` to `X.Y.Z`. This
   will be automatically propagated to the CLI, the Nickel language server and
   Pyckel.
3. Update the current version number mentioned in `doc/manual/introduction.md`
   with the new one set in step 2. Grep for the previous version
   number in the various README files, as the latest version is sometimes
   mentioned, and update if needed.
4. Bump the version number of `core` in `core/Cargo.toml` and
   `wasm-repl/Cargo.toml`. The two versions must always be the same.

   Bump the version of the other crates in the workspace **if needed** (usually,
   it's safer to always bump the version of `core` because it's modified all the
   time without special care about its public API, but the following crates are
   often left untouched):

   - `lsp/lsp-harness/Cargo.toml`
   - `utils/Cargo.toml`

   Afterwards, also adjust the version numbers of the dependencies in
   `Cargo.toml`. For example, in

   ```toml
      nickel-lang-core = { version = "0.1", path = "./core", default-features = false }
   ```

   adjust the version `0.1` to reflect the new version number.
5. Make sure that everything builds: run `nix flake check` at the root of the
   repository.
6. Add the changelog since the last release in RELEASES.md. GitHub is able to
   automatically generate release notes: refer to [this
   guide](https://docs.github.com/en/repositories/releasing-projects-on-github/automatically-generated-release-notes).
   While the output needs to be reworked, it's a useful starting point. Commit
   and push your changes.
7. Set the `stable` branch to point to your new `X.Y.Z-release`. Because the
   `stable` branch usually contains specific fixes, or cherry-picked commits,
   we'll have to force push. First save the previous state in a local branch:

   ```console
   git checkout stable
   git branch stable-local-save
   ```

   Update the `stable` branch:

   ```console
   git checkout stable
   git reset --hard X.Y.Z-release`
   git push --force-with-lease
   ```

   If anything goes wrong, you can reset `stable` to its previous state:

   ```console
   git checkout stable
   git reset --hard stable-local-save
   git push --force-with-lease
   ```

### Release on crates.io

**IMPORTANT**: this section is covered by the `release.sh` script, and is only
kept for information purpose. Usually, you shouldn't have to perform the
following steps manually.

1. Remove references to `nickel-lang-utils` from the `[dev-dependencies]`
   sections of the crates to be published: `./core/Cargo.toml` for
   `nickel-lang-core`, `./cli/Cargo.toml` for `nickel-lang-cli` and
   `./lsp/nls/Cargo.toml` for `nickel-lang-lsp` (work-around for
   [cargo:#4242](https://github.com/rust-lang/cargo/issues/4242).

2. Remove references to `lsp-harness` from the `[dev-dependencies]` sections of
   the `./lsp/nls/Cargo.toml` (workaround for the same issue as 1.).

3. For all crates to be published, remove the `format` feature from the list of
   features (in the `[features]` section of their `Cargo.toml` file), remove all
   dependencies referenced by `format` (of the form `dep:xxx`) from the list of
   dependencies of the crate, and finally, remove `"format"` from the list of
   the default features.

   We have to do this because Topiary isn't published on `crates.io` yet, but
   `cargo` insists that we only depend on published crates. Thus, we have to
   abandon the format feature - which requires Topiary - for the version
   published to `crates.io`.

4. **Commit the changes made in 1., 2. and 3. temporarily to please cargo, but
   they will be dropped later. Do not push**.

5. Check that a dry run of `cargo publish` succeeds on the crates to be
   published (`nickel-lang-core`, `nickel-lang-cli` and `nickel-lang-lsp`):

   - `cargo publish -p nickel-lang-core --dry-run`
   - `cargo publish -p nickel-lang-cli --dry-run`
   - `cargo publish -p nickel-lang-lsp --dry-run`

   For this to work, you will need to be signed in to `crates.io` with a GitHub
   account that is part of the [Nickel Core
   team](https://github.com/orgs/nickel-lang/teams/core), and have a `crates.io`
   API key saved locally on your machine (normally via `cargo login`). For help
   with this, contact the Nickel maintainers.

6. Actually release `nickel-lang-core`, `nickel-lang-cli` and `nickel-lang-lsp`
   (in that order, as the cli and the lsp depend on core) on crates.io:
   `cargo publish -p <crate-to-publish>`

7. Ditch the potential changes made to the cargo manifests at steps 1., 2.
   and 3. by dropping the corresponding commit

### Release on GitHub

1. Do the [release on
   GitHub](https://docs.github.com/en/repositories/releasing-projects-on-github/managing-releases-in-a-repository),
   Make sure that you set `X.Y.Z-release` as the target branch and have GitHub
   create the `X.Y.Z` tag on release.

2. Verify that the "Upload release artifacts" GitHub action is getting triggered
   and completes successfully, uploading a static Nickel binary and a Docker
   image for both x86-64 and arm64 Linux platforms.

### Redeploy nickel-lang.org with the new release

1. Checkout the [nickel-lang](https://github.com/tweag/nickel-lang.org/)
   repository.
2. Branch out from `master` and update the Nickel input:

   ```console
   git checkout -b release/X.Y.Z
   nix flake lock --update-input nickel
   git add flake.lock
   git commit -m "Update to Nickel vX.Y.Z"
   git push -u origin @
   ```

   Open a pull request on the nickel-lang repository. Once the CI is green and
   the PR is merged, nickel-lang.org will be automatically redeployed with the
   new version of Nickel used for the playground and the documentation.

### Port changes to master

1. Cherry-pick the following commits into separate PRs to `master`:
     - Bumping the version numbers done in Preparation 2.
     - Updating release notes done in Preparation 5.
     - Fixes that you made locally for `nix flake check` or other commands.
2. Profit!
