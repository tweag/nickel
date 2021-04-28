# Simple contracts

This directory contain two examples of simple contrtacts.

The first one, `simple-contract-bool.ncl`, defines simple contracts on booleans
and illustrates their use as pre-conditions and post-conditions of a function.

The second one, `simple-contract-div.ncl`, defines simple contracts on numbers
and illustrates the application of several contracts to one value. It is
expected to fail, to demonstrate basic error-reporting for contracts.

## Run

```
$ nickel -f simple-contract-bool.ncl
$ nickel -f simple-contract-div.ncl
```

## Playground

You can try to break the one of the contracts of `simple-contract-bool.ncl` to
see them fail. Conversely, you can provide a valid value in
`simple-contract-div.ncl` to see the execution succeed.
