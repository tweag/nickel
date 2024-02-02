# test.type = 'pass'

let {check, ..} = import "../pass/lib/assert.ncl" in
[
  (import %lazy% "imported/empty.yaml") == null,
]
|> check
