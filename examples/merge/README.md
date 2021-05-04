# Merge

This example shows how to merge separate modules into one configuration. The
code to run lies in `main.ncl`. The default value `firewall.enabled` defined in
`security.ncl` is overwritten in the final config.

## Run

```
$ nickel -f main.ncl export
```
