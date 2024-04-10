# test.type = 'pass'
let {Assert, check, ..} = import "../lib/assert.ncl" in

[
  let this-isn't-invalid = true in this-isn't-invalid,
  let ___multi_underscore_start = true in ___multi_underscore_start,
]
|> check
