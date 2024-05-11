# test.type = 'pass'


[
  {foo | default = 1} & {foo = 2} == {foo = 2},
  {foo = 1 + 1, bar = foo + 1} == {foo = 2, bar = 3},

  let r = {foo | default = 1 + 1, bar = foo + 1} in
  std.deep_seq (r & {foo = 3}) r == {foo = 2, bar = 3},

  let r = {foo | default = 1 + 1, bar = foo + 1} in
  (r & {foo = 3}) == {foo = 3, bar = 4},

  let r = {foo | default = 1 + 1, bar = foo + 1} in
  std.deep_seq r (r & {foo = 3}) == {foo = 3, bar = 4},

  let ext = 0 + 1 in
  let r = {
    foo | default = 1 + 1,
    a = let inner = 0 in foo + ext + inner,
    b = let inner = 1 in a + ext + inner,
    c = let inner = 2 in b + ext + inner,
  } in
  std.deep_seq (r & {foo = 1 - 1}) r
    == {foo = 2, a = 3, b = 5, c = 8},

  let ext = 0 + 1 in
  let r = {
    foo | default = 1 + 1,
    a = let inner = 0 in foo + ext + inner,
    b = let inner = 1 in a + ext + inner,
    c = let inner = 2 in b + ext + inner,
  } in
  std.deep_seq r (r & {foo = 1 - 1})
    == {foo = 0, a = 1, b = 3, c = 6},

  let nested = {
    foo.bar.baz | default = 0,
    some.nested = {
      stuff = foo.bar.baz + 1,
      other = stuff + 1,
    },
  } in
  std.deep_seq (nested & {foo.bar.baz = 1}) nested
    == {foo.bar.baz = 0, some.nested.stuff = 1, some.nested.other = 2},

  let nested = {
    foo.bar.baz | default = 0,
    some.nested = {
      stuff = foo.bar.baz + 1,
      other = stuff + 1,
    },
  } in
  std.deep_seq nested (nested & {foo.bar.baz = 1})
    == {foo.bar.baz = 1, some.nested.stuff = 2, some.nested.other = 3},

  let base = {foo | default = "a", bar = foo ++ "b"} in
  let ext1 = base & {foo = "1"} in
  let ext2 = base & {foo = "A"} in
  (base.bar == "ab"
  && ext1.bar == "1b"
  && ext2.bar == "Ab")
  |> std.deep_seq base
  |> std.deep_seq ext1
  |> std.deep_seq ext2,

  # Test that the evaluation of a recursive record patches the right
  # environment in some edge cases (when fields only contains a variable)
  let foo = 1 in
  let y = foo in
  {bar = y, foo = 2}.bar == 1,
  ({foo | default = 1, bar = foo} & {foo = 2}).bar == 2,
  ({foo | default = 1, bar = foo, baz = bar} & {foo = 2}).baz == 2,
]
|> std.test.assert_all
