# test.type = 'pass'
let {check, ..} = import "../lib/assert.ncl" in
let {string, ..} = std in

[
  # string.BoolLiteral
  ("true" | string.BoolLiteral) == "true",
  ("false" | string.BoolLiteral) == "false",

  # string.NumberLiteral
  ("+1.2" | string.NumberLiteral) == "+1.2",
  ("-1.2" | string.NumberLiteral) == "-1.2",
  ("9001" | string.NumberLiteral) == "9001",

  # string.Character
  # single ascii chars
  ("e" | string.Character) == "e",
  ("x" | string.Character) == "x",
  # single unicode codepoints
  ("😀" | string.Character) == "😀",
  ("😶" | string.Character) == "😶",
  # an extended grapheme cluster is also a "character"
  ("ő" | string.Character) == "ő",
  ("👩🏿‍❤️‍💋‍👩🏼" | string.Character) == "👩🏿‍❤️‍💋‍👩🏼",

  # string.EnumTag
  ('Foo | std.enum.Tag) == 'Foo,
  ('Barr | std.enum.Tag) == 'Barr,

  # string.Stringable
  ('Foo | string.Stringable) == 'Foo,
  (true | string.Stringable) == true,
  (1 | string.Stringable) == 1,
  ("" | string.Stringable) == "",

  # string.NonEmpty
  ("_" | string.NonEmpty) == "_",
  ("a" | string.NonEmpty) == "a",
  (" " | string.NonEmpty) == " ",
] |> check
