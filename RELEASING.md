# Release process for Nickel

You're ready to release a new version of Nickel - congrats!

This document explains how to do so step by step while keeping the various
crates and dependent repositories (such as the website) in a consistent state.

## Steps to make a release

### About the version numbers

Some of the crates in the Nickel workspace are libraries and not versioned
according to the version number of the language itself. These are

- `nickel-lang-core`
- `nickel-lang-utils`
- `lsp-harness`
- `nickel-wasm-repl`

Their version numbers take the form `0.W.U` and their public APIs are not
considered stable. Consequently we bump their versions to `0.(W+1).0` on every
release.

Other crates carry the version number of the Nickel language. These are

- `nickel-lang-cli`
- `nickel-lang-lsp`
- `pyckel`

### Prepare

1. Branch out from `master` to a dedicated branch `X.Y.Z-release`:
   `git checkout -b X.Y.Z-release`
2. Bump the overall version number in `Cargo.toml` to `X.Y.Z`. This will be
   automatically propagated to the CLI, the Nickel language server and Pyckel.
3. Bump the version number of the other crates in the workspace:
   - `core/Cargo.toml`
   - `lsp/lsp-harness/Cargo.toml`
   - `utils/Cargo.toml`
   - `wasm-repl/Cargo.toml`
   Afterwards, also adjust the version numbers in `Cargo.toml`. For example, in

   ```toml
      nickel-lang-core = { version = "0.1", path = "./core", default-features = false }
   ```

   adjust the version `0.1` to reflect the new version number.

4. Make sure that everything builds: run `nix flake check` at the root of the
   repository.
5. Add the changelog since the last release in RELEASES.md. GitHub is able to
   automatically generate release notes: refer to [this
   guide](https://docs.github.com/en/repositories/releasing-projects-on-github/automatically-generated-release-notes).
   While the output needs to be reworked, it's a useful starting point. Commit
   and push your changes.
6. Set the `stable` branch to point to your new `X.Y.Z-release`. Because the
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

1. Remove references to `nickel-lang-utils` from the `[dev-dependencies]`
   sections of the crates to be published: `./core/Cargo.toml` for
   `nickel-lang-core`, `./cli/Cargo.toml` for `nickel-lang-cli` and
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
