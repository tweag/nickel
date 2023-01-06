# Snapshot testing

Nickel snapshot tests are run by invoking the most recently built Nickel binary
(located via the `CARGO_BIN_EXE_nickel` environment variable) with the Nickel
files located in subdirectories of the `inputs` directory.

Each subdirectory corresponds to the type of output we're snapshotting.
Currently we have `errors`, which tests error descriptions written to STDERR,
`export`, which tests the output when exporting JSON, and `pretty` which tests
pretty-printed output.

The actual test code is defined in `main.rs`, and uses the
[`insta`](https://github.com/mitsuhiko/insta) crate to assert against snapshots
stored in the `snapshots` directory. While it is possible to edit snapshot
files manually to update them, it is significantly simpler to use the
[`cargo-insta`](https://crates.io/crates/cargo-insta) tool. If you're using the
nix dev shell, `cargo-insta` is already on your path. Most of this guide will
assume you have `cargo-insta` installed. If you'd prefer not to install it,
please read [this section](#without-cargo-insta).

## What to do if a snapshot test fails

1. Run `cargo insta review`.
2. Check the output to see what changed
  
## How to add a new snapshot test

1. Add the Nickel file whose output you want to test into one of the
  `inputs` subdirectories. Give it a descriptive name, as the name of the file
  is used when printing test failures, and is also used to name the snapshot
  file.
2. Run the snapshot tests with `cargo test --test snapshot`.
3. You'll see a failure message, noting that the test was a new snapshot.
4. Run `cargo insta review`.
5. If you're happy with the output, accept it.
6. Commit the input file and the generated snapshot. You're done!

## Without `cargo-insta`

1. Failing snapshot tests will cause a new snapshot file to be generated in the
  snapshots directory. The naming scheme for these new files is
  `${failing_snapshot_file_name}.new`. So if the snapshot file which failed
  was called `snapshot__error_type_mismatch.ncl.snap`, the new file will be
  called `snapshot__error_type_mismatch.ncl.snap.new`.
2. Diff the old and new files, using your favourite diffing tool (or eyeballs).
3. If you want to accept the new file, simply delete the old snapshot, and
  remove the `.new` extension from the new one. To reject it, simply delete it.
