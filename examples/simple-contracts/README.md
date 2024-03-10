# Simple contracts

This directory contains two examples of simple contracts.

- `simple-contract-bool.ncl` defines simple contracts on booleans and
  illustrates the use of contracts as pre-conditions and post-conditions of a function.
- `simple-contract-div.ncl`, defines simple contracts on numbers
  and illustrates the application of several contracts to one value. It is
  expected to fail, to demonstrate basic error-reporting for contracts.

For illustrative purpose, those contracts are written as plain custom contracts:
functions `Label -> Dyn -> Dyn`. However, they are just boolean predicates here.
In this case, you should rather write them more succinctly as predicates `Dyn ->
Bool` and use `std.contract.from_predicate` to obtain the corresponding
contracts.

## Run

```console
nickel eval simple-contract-bool.ncl
nickel eval simple-contract-div.ncl
```

## Playground

You can try to break the one of the contracts of `simple-contract-bool.ncl` to
see them fail. Conversely, you can provide a valid value in
`simple-contract-div.ncl` to see the execution succeed.
