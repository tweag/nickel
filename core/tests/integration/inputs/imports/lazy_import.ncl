# test.type = 'pass'

let {check, ..} = import "../lib/assert.ncl" in
[
  (import %lazy% "imported/empty.yaml") == null,
]
|> check
