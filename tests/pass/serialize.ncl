let Assert = fun l x => x || %blame% l in

let assertSerInv = fun x =>
    let assertAux = fun format x =>
      %deserialize% format (%serialize% format (%deepSeq% x x)) == x in
    assertAux `Json x &&
    assertAux `Yaml x &&
    assertAux `Toml x in

let assertDeserInv = fun x =>
    let assertAux = fun format x =>
      let s = %serialize% format (%deepSeq% x x) in
      %serialize% format (%deserialize% format s) == s in
    assertAux `Json x &&
    assertAux `Yaml x &&
    assertAux `Toml x in

(assertSerInv {val = 1 + 1} | #Assert) &&
(assertSerInv {val = "Some string"} | #Assert) &&
(assertSerInv {val = ["a", 3, []]} | #Assert) &&
(assertSerInv {a.foo.bar = "2", b = false, c = [{d = "e"}, {d = "f"}]}
  | #Assert) &&

(assertDeserInv {a = 1, b = 4, c = 3} | #Assert) &&
(assertDeserInv {a.b.c = "richtig"} | #Assert) &&
(assertDeserInv {
    foo = 1,
    bar = ["str", true],
    baz = {subfoo = true, subbar = 0}
  } | #Assert) &&


true
