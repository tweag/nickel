### /diagnostics-recursion.ncl
let rec foo = { bar = foo } in
[
  foo,
  foo.bar.bar.bar.bar.bar.baz
]
### diagnostic = ["file:///diagnostics-recursion.ncl"]
