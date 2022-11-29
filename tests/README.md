# A quick note on how tests are structured

By default, `cargo` encourages developers to put integration test files directly
in the `/tests` directory. Doing so means that a different test crate (and
associated binary) is built for each file.

There is both a compile-time and a runtime cost to doing this, as (1) multiple
different binaries must be built to run all tests and (2) while `cargo` can
run multiple tests in parallel, it runs multiple binaries sequentially.

By instead having a single crate which contains all tests (as we do in
`/tests/integration`), we save on compilation time, as well as being able to
fully parallelise all of our test runs.

See [this blog post](https://matklad.github.io/2021/02/27/delete-cargo-integration-tests.html)
for more information.
