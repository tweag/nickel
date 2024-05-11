# test.type = 'pass'


let record = {foo = 1, bar = { baz = 2 }} in
let field = "bar" in
let part = "ba" in
[
  (.) record "foo" == 1,
  (.) record field == { baz = 2 },
  let res = (.) record field == {baz = 2} in res,
  (.) record "%{part}r" == { baz = 2 },
] |> std.test.assert_all
