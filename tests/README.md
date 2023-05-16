# A quick note on how tests are structured

This directory has three subdirectories, each containing a different kind of
test. Each directory contains a `main` module implementing a test runner
for that particular type of test. The tests themselves are then written as
Nickel source files, annotated with toml headers which describe the expected
behaviour of the program.

- `integration` contains general purpose integration tests,
- `snapshot` contains snapshot tests for command-line output,
- `examples` contains a runner for samples in the repository's top-level
  `examples` directory.

## Why are the tests split into separate crates?

By default, `cargo` encourages developers to put integration test files directly
in the `tests` directory. Doing so means that a different test crate (and
associated binary) is built for each file.

There is both a compile-time and a runtime cost to doing this, as (1) multiple
different binaries must be built to run all tests and (2) while `cargo` can
run multiple tests in parallel, it runs multiple binaries sequentially.

By instead having larger crates which contain multiple tests each we save on
compilation time, as well as being able to take advantage of parallelisation
to a greater degree.

See [this blog post](https://matklad.github.io/2021/02/27/delete-cargo-integration-tests.html)
for more information.
