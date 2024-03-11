# Merge

This example shows how to merge separate modules into one configuration. The
code to run lies in `main.ncl`. The default value `firewall.enabled` defined in
`security.ncl` is overridden in the final configuration.

## Run

```console
nickel export main.ncl
```
