# test.type = 'pass'


let assertSerInv = fun x =>
    let assertAux = fun format x =>
      %deserialize% format (%serialize% format (%force% x)) == x in
    assertAux 'Json x &&
    assertAux 'Yaml x &&
    assertAux 'Toml x in

let assertDeserInv = fun x =>
    let assertAux = fun format x =>
      let s = %serialize% format (%force% x) in
      %serialize% format (%deserialize% format s) == s in
    assertAux 'Json x &&
    assertAux 'Yaml x &&
    assertAux 'Toml x in

[
  assertSerInv {val = 1 + 1},
  assertSerInv {val = "Some string"},
  assertSerInv {val = ["a", 3, []]},
  assertSerInv {a.foo.bar = "2", b = false, c = [{d = "e"}, {d = "f"}]},

  # regression test for a previously missing `#[serde(skip)]` on the `Match`
  # variant of `term::Term`. That was causing a list with the right size to be
  # serialized as a `Match` instead of an array. This test checks that lists of
  # various sizes don't get misinterpreted again as a term construct that misses a
  # `#[serde(skip)]` annotations
  {
    one = [
      {foo = 1},
    ],
    two = [
      {foo = 1},
      {bar = "bar"},
    ],
    three = [
      {foo = 1},
      {bar = "bar"},
      {baz = false},
    ],
    four = [
      {foo = 1},
      {bar = "bar"},
      {baz = false},
      {last = 4},
    ]
  }
  |> assertSerInv,


  assertDeserInv {a = 1, b = 4, c = 3},
  assertDeserInv {a.b.c = "richtig"},
  assertDeserInv {
      foo = 1,
      bar = ["str", true],
      baz = {subfoo = true, subbar = 0}
    },

  # regression test for issue #668 (https://github.com/tweag/nickel/issues/668)
  let base = {foo | {_: {bar | default = 2}}} in
  let ext = {foo = {some = {}}} in
  assertSerInv (base & ext),
]
|> std.test.assert_all
