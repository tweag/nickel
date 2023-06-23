# Mantis bench

This bench is a copy of [nickel-mantis-ops](https://github.com/tweag/nickel-mantis-ops)
by @yannham

## Mantis Ops in Nickel

This is a tentative to convert the [mantis ops repo of
IOHK](https://github.com/input-output-hk/mantis-ops) from CUE to Nickel, as a
dogfooding exercise. This repo tries to respect the same file hierarchy than the
original one, whenever possible.

### Example invocation

```console
nickel export <<< 'import "deploy.ncl" {namespace="mantis-staging", job="miner"}'
```
