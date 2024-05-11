# test.type = 'pass'


[
  # basic
  0 == 0 + 0 + 0,
  true == (if true then true else false),
  "a" ++ "b" ++ "c" == "%{"a" ++ "b"}" ++ "c",
  'Less == 'Less,
  'small == 'small,

  1 + 1 != 0,
  true != (if true then false else true),
  "a" != "a" ++ " ",
  1 != true,
  "1" != 1,
  "true" != true,
  'Less != 'small,
  'Less != 0,
  'Greater != false,

  # arrays
  [] == [],
  [(1 + 0), (1 + 1), (1 + 1 + 1)] == [1, 2, 3],
  [(1 + 0), ("a" ++ "b"), (if true then true else false)]
    == [1, "ab", true],
  [[[]]] == [[[]]],
  [[1], [[2]]] == [[2 + (-1)], [[1 + 1]]],
  [[true, false]] == std.array.flatten [[[true, false]]],

  [] != [1],
  [] != 1,
  [] != {},
  [1, "a", true] != [1, "a", false],
  [[true]] != [[[true]]],

  # records
  {} == {},
  ({}
  |> std.record.insert "a" 1
  |> std.record.insert "b" true)
    == {a = 1, b = true},
  {a = 1 + 0, b = 1 + 1, c = 1 + 1 + 1}
    == { a = 1, b = 2, c = 3 },
  {
      foo = 1 + 0,
      bar = "a" ++ "b",
      baz = if true then true else false
    } == {foo = 1, bar = "ab", baz = true},

  std.record.insert "a" {a.a = {}} {}
    == { a = { a = { a = {} } } },
  {
      foo = {bar = 2 + (-1)},
      baz = {foo = {bar = 1 + 1}}
    }
    == {foo = {bar = 1}, baz = {foo = {bar = 2}}},
  {} != {a = true},
  {a = 1} != {a = 2},
  { a = "a", b = true } != { a = true, b = "a"},
  { a = { a = true } } != {a = { a = { a = true } } },

  # Now that the equality operator directly uses the stack to store its continuation (see
  # https://github.com/tweag/nickel/pull/247), check that it correctly cleans the stack when
  # evaluating a subequality to `false`.
  # Generate an non-empty evaluation context to evaluate equalities over a non-empty stack
  let eq_with_ctxt = fun x y =>
      let not = fun b =>
        if b then true else false in
      not (not (not (not ((x) == (y))))) in
    (eq_with_ctxt
        {a = 1 + 0, b = 1 + 1 + a, c = 0, d = 0}
        { a = 1, b = 3, c = 0, d = 0}
  ) &&
    (eq_with_ctxt
        [[1,2,3,4], [1,0,3,4], [1,2,3,4], [1,2,3,4]]
        [[1,2,3,4], [1,2,3,4], [1,2,3,4], [1,2,3,4]]
      == false
  ),

  # ADTs
  'Left (1+1) == 'Left 2,
  'Left 1 != 'Left 2,
  'Left 2 != 'Right 2,
  'Up [1,2,3] == 'Up [0+1,1+1,2+1],
]
|> std.test.assert_all
