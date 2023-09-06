# test.type = 'pass'
let {check, ..} = import "../lib/assert.ncl" in

let record = {foo = 1, bar = { baz = 2 }} in
let field = "bar" in
let part = "ba" in
[
  (.) "foo" record == 1,
  (.) field record == { baz = 2 },
  let res = (.) field record == {baz = 2} in res,
  (.) "%{part}r" record == { baz = 2 },
] |> check
