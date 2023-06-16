# Release process for Nickel

You're ready to release a new version of Nickel - congrats!

This document explains how to do so step by step while keeping the various
crates and dependent repositories (such as the website) in a consistent state.

## Steps to make a release

### Prepare

1. Branch out from `master` to a dedicated branch `X.Y.Z-release`:
   `git checkout -b X.Y.Z-release`
2. Bump the version number of all the crates of the workspace to `X.Y.Z`:
   - Bump the `version` attribute to `X.Y.Z` in `./Cargo.toml`,
     `./utilities/Cargo.toml`, `lsp/nls/Cargo.toml` and
     `./nickel-wasm-repl/Cargo.toml`.
   - Bump the version of the interdependencies of the crates in `nickel-lang-cli`,
     `nickel-lang-lib`, `lsp/nls` and `nickel-wasm-repl`. For example, `nickel-lang-lib`
      has the following line in `./nickel-lang-lib/Cargo.toml` under `[dev-dependencies]`:

     ```toml
     nickel-lang-utilities = {path = "../utilities", version = "0.3.0"}
     ```

     You have to bump the `version` number to `X.Y.Z` of such dependencies on
     `nickel-lang-lib`, `nickel-lang-utilities`, or any other crate of the
     workspace as well.

   Commit and push your changes.
3. Make sure that everything builds: run `nix flake check` at the root of the
   repository.
4. Add the changelog since the last release in RELEASES.md. GitHub is able to
   automatically generate release notes: refer to [this
   guide](https://docs.github.com/en/repositories/releasing-projects-on-github/automatically-generated-release-notes).
   While the output needs to be reworked, it's a useful starting point. Commit
   and push your changes.
5. Set the `stable` branch to point to your new `X.Y.Z-release`. Because the
   `stable` branch usually contains specific fixes, or cherry-picked commits,
   we'll have to force push. We advise to first save the previous state in a
   local branch:

   ```console
   git checkout stable
   git branch stable-local-save
   ```

   If anything goes wrong, you can reset `stable` to its previous state:

   ```console
   git checkout stable
   git reset --hard stable-local-save
   git push --force-with-lease
   ```

   Update the `stable` branch:

   ```console
   git checkout stable
   git reset --hard X.Y.Z-release`
   git push --force-with-lease
   ```

### Release on crates.io

1. Remove references to `nickel-lang-utilities` from the `[dev-dependencies]`
   sections of the crates to be published: `./nickel-lang-lib/Cargo.toml` for
   `nickel-lang-lib`, `./nickel-lang-cli/Cargo.toml` for `nickel-lang-cli` and
   `./lsp/nls/Cargo.toml` for `nickel-lang-lsp` (work-around for
   [cargo:#4242](https://github.com/rust-lang/cargo/issues/4242).

   **Commit those changes temporarily to please cargo, but they will be
   dropped later. Do not push**.
2. Check that `cargo publish --dry-run` succeeds. For this to work, you will
   need to be signed in to `crates.io` with a GitHub account that is part of
   the [Nickel Core team](https://github.com/orgs/nickel-lang/teams/core),
   and have a `crates.io` API key saved locally on your machine (normally
   via `cargo login`). For help with this, contact the Nickel maintainers.
3. Release on crates.io: `cargo publish`
4. Ditch the potential changes made to the cargo manifests at step 1. by
   dropping the corresponding commit.

### Release on GitHub

1. Build a Docker image and a static binary for ARM64 manually, using

   ```console
   nix build --out-link nickel-arm64-docker-image.tar.gz .#packages.aarch64-linux.dockerImage
   nix build --out-link nickel-arm64-linux .#packages.aarch64-linux.dockerImage
   ```

   on an ARM64 Linux machine (x86_64 assets are automatically handled by a
   workflow, see 3).

2. Do the [release on
   GitHub](https://docs.github.com/en/repositories/releasing-projects-on-github/managing-releases-in-a-repository),
   and include the docker image built in 1. Make sure that you set `X.Y.Z-release`
   as the target branch and have GitHub create the `X.Y.Z` tag on release.
   Upload `nickel-arm64-docker-image.tar.gz` and `nickel-arm64-linux` as release
   assets from the last step.

3. Verify that the "Upload release artifacts" GitHub action is getting triggered
   and completes successfully, uploading a static Nickel binary for x86_64 Linux
   and a Docker image.

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
     - Fixes that you made locally for `nix flake check`, `cargo
       doc` or any other command.
2. Profit!
