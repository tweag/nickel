let Assert = fun l x => x || %blame% l in

let assertSerInv = fun x =>
    let assertAux = fun format x =>
      %deserialize% format (%serialize% format (%deep_seq% x x)) == x in
    assertAux `Json x &&
    assertAux `Yaml x &&
    assertAux `Toml x in

let assertDeserInv = fun x =>
    let assertAux = fun format x =>
      let s = %serialize% format (%deep_seq% x x) in
      %serialize% format (%deserialize% format s) == s in
    assertAux `Json x &&
    assertAux `Yaml x &&
    assertAux `Toml x in

[
  assertSerInv {val = 1 + 1},
  assertSerInv {val = "Some string"},
  assertSerInv {val = ["a", 3, []]},
  assertSerInv {a.foo.bar = "2", b = false, c = [{d = "e"}, {d = "f"}]},

  assertDeserInv {a = 1, b = 4, c = 3},
  assertDeserInv {a.b.c = "richtig"},
  assertDeserInv {
      foo = 1,
      bar = ["str", true],
      baz = {subfoo = true, subbar = 0}
    },
]
|> list.foldl (fun x y => (x | Assert) && y) true
