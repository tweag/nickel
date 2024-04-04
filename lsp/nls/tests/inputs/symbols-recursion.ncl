### /syms.ncl
let rec foo = { bar = foo } in foo
### # In principle there are infinitely many symbols, but we bound the recursion instead
### # of crashing.
### [[request]]
### type = "Symbols"
### textDocument.uri = "file:///syms.ncl"
