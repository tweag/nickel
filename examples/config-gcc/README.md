# GCC Configuration

This is an example of writing contracts and generating a configuration for an
invocation of the `gcc` compiler.

## Run

```console
nickel -f config-gcc.ncl export
```

## Contracts

This example defines a couple contracts:

- `GccFlag`: define valid flags that can be passed to `gcc`. It demonstrates a
    use-case where data can be specified in different format: here, a flag is
    either a simple string like `"-Wextra"` or a structured value `{flag = "W",
    arg = "extra"}`. The contract normalizes the value to a string, so that it
    appears as such in the output. This contract is illustrative and by no mean
    exhaustive.
- `Path`: define a valid `/`-separated path as a string. In particular, the
    `#` sequence is forbidden.
- `SharedObjectFile`: define a file whose extension is `.so`.
- `OptLevel`: optimization level, either `0`, `1` or `2`.
- `Contract`: the schema of the end configuration.

## Playground

You can try to break any of the previous contracts to see what happens: provide
a non supported flag, a `pathLibC` that doesn't end in `".so"`, and so on.
