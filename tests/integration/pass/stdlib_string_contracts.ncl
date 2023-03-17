let {check, ..} = import "lib/assert.ncl" in

[
  # string.BoolLiteral
  ("True" | string.BoolLiteral) == "true",
  ("False" | string.BoolLiteral) == "false",
  ("true" | string.BoolLiteral) == "true",
  ("false" | string.BoolLiteral) == "false",

  # string.NumLiteral
  ("+1.2" | string.NumLiteral) == "+1.2",
  ("-1.2" | string.NumLiteral) == "-1.2",
  ("9001" | string.NumLiteral) == "9001",

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
  (`Foo | enum.Tag) == `Foo,
  (`Barr | enum.Tag) == `Barr,

  # string.Stringable
  (`Foo | string.Stringable) == `Foo,
  (true | string.Stringable) == true,
  (1 | string.Stringable) == 1,
  ("" | string.Stringable) == "",

  # string.NonEmpty
  ("_" | string.NonEmpty) == "_",
  ("a" | string.NonEmpty) == "a",
  (" " | string.NonEmpty) == " ",
] |> check