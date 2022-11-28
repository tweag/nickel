let {check, Assert, ..} = import "testlib.ncl" in

[
  # left_annot_precedence
  (let dummy = null in
    let LocalAssert = Assert in
    true | LocalAssert),

  (if false then
      null
    else
      let dummy = null in
      let LocalAssert = Assert in
      true | LocalAssert),

  (let f = fun x =>
      let dummy = null in
      let LocalAssert = Assert in
      x | LocalAssert in
    f true),

  # others_precedence
  ((fun x => x | Assert) true),
  (let AssertOk = fun l t => if t == `Ok then t else %blame% l in
    switch {`Ok => true, `Err => false} `Ok | AssertOk),
]
|> check
