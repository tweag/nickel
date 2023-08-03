# test.type = 'pass'

let {check, ..} = import "../pass/lib/assert.ncl" in
[
  (import "imported/empty.yaml") == null,

  (import "imported/multiple.yaml") == [
    { type = "event", id = 1 },
    { type = "event", id = 2 }
  ],
]
|> check