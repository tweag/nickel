### /diagnostics-recursion.ncl
let rec foo = { bar = foo, quux | String = 1 } in
[
  foo,
  foo.bar.bar.bar.bar.bar.baz
]
### diagnostic = ["file:///diagnostics-recursion.ncl"]
