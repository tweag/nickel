# Kubernetes-like record contract

This is an illustrative (thus incomplete and maybe incorrect) example of a
Kubernetes configuration. It demonstrates how you can write contracts for simple
elements (such as `Port`), and assemble them into a schema for your end
configuration.

Ideally, Kubernetes contracts would be auto-generated from existing Kubernetes
schemas in a real world scenario.

## Run

```console
nickel export --format yaml record-contract.ncl
```

## Playground

You can try to break any of the previous contracts to see what happens: provide
a non supported flag, a `pathLibC` that doesn't end in `".so"`, and so on.
